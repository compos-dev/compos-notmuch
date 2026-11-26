;;; mail-gmail.scm --- the Gmail backend of the mail UI.
;;;
;;; A mailbox (gmail "me@example.com") reads, tags and sends that Google
;;; account's mail through the Gmail API, with the account the google
;;; package connected. It answers every operation of the notmuch backend
;;; (see "mail backends" in mail.scm) in the same shapes, so the mail UI
;;; does not know which one it reads:
;;;
;;;   tags are labels: inbox INBOX, unread UNREAD, sent SENT, draft DRAFT,
;;;   spam SPAM, trash TRASH, flagged STARRED, important IMPORTANT, and
;;;   the categories promotions, social, forums and updates; any other
;;;   tag is the user label of that name, made the first time a thread is
;;;   given it.
;;;   queries are notmuch queries, translated to Gmail search: tag:, from:,
;;;   to:, subject:, id:, date:, and, or, not and parentheses. thread:ID
;;;   picks threads by id. What has no Gmail form (lastmod:, folder:,
;;;   tag:/regex/) is dropped.
;;;   the change counter is Gmail's historyId, so the mail feed and
;;;   autotag read what arrived since their last look from history.list.

(domain! 'mail)
(effects! '(write external))

(define gm--base "https://gmail.googleapis.com/gmail/v1/users/me")

(define (gm--email mb) (cadr (or mb (nm--current-mailbox))))

(define (gm--account mb)
  (let* ((email (gm--email mb))
         (hit (filter (lambda (a) (equal? (plist-get a 'email) email)) (google-accounts))))
    (if (null? hit)
        (error (string-append "Gmail: " email " is not a connected Google account. Run M-x google-connect."))
        (plist-get (car hit) 'id))))

(define (gm--qs pairs)
  (if (null? pairs)
      ""
      (string-append "?" (string-join (map (lambda (p) (string-append (car p) "=" (url-encode (cadr p)))) pairs) "&"))))

(define (gm--call mb method path &optional body)
  ;; five arguments: a sixth, the callback, makes it asynchronous
  (let ((r (google-http! (gm--account mb) method (string-append gm--base path) '() (or body #f))))
    (if (plist-get r 'ok)
        (or (plist-get r 'data) '())
        (error (string-append "Gmail " method " " path ": " (or (plist-get r 'error) "failed"))))))

(define (gm--get mb path . pairs) (gm--call mb "GET" (string-append path (gm--qs pairs))))
(define (gm--post mb path body) (gm--call mb "POST" path body))

;; F over XS at once, in tasks; answers in order. A failed one answers #f.
(define (gm--pmap f xs)
  (let ((tasks (map (lambda (x) (task-spawn (lambda () (f x)))) xs)))
    (map (lambda (t) (ignore-errors (lambda () (task-await t 60000)))) tasks)))

;;; --- base64url

(define (gm--b64url->b64 s)
  ;; string-replace is a builtin: string-map recurses once per character
  ;; and overflows on a body of 100k characters
  (let* ((t (string-replace (string-replace s "-" "+") "_" "/"))
         (pad (modulo (- 4 (modulo (string-length t) 4)) 4)))
    (string-append t (make-string pad #\=))))

(define (gm--b64->b64url s)
  (string-replace (string-replace (string-replace s "=" "") "+" "-") "/" "_"))

(define (gm--decode data)
  (string-replace (or (ignore-errors (lambda () (base64-decode (gm--b64url->b64 data)))) "") "\r\n" "\n"))

;;; --- labels and tags

(define *gm-labels* '())   ; ((EMAIL (ID NAME) ...) ...)

(define gm--system-tags
  '(("INBOX" "inbox") ("UNREAD" "unread") ("SENT" "sent") ("DRAFT" "draft")
    ("SPAM" "spam") ("TRASH" "trash") ("STARRED" "flagged") ("IMPORTANT" "important")
    ("CATEGORY_PROMOTIONS" "promotions") ("CATEGORY_SOCIAL" "social")
    ("CATEGORY_FORUMS" "forums") ("CATEGORY_UPDATES" "updates")))

(define (gm--labels mb)
  (let ((hit (assoc (gm--email mb) *gm-labels*)))
    (if hit
        (cdr hit)
        (let ((ls (map (lambda (l) (list (plist-get l 'id) (plist-get l 'name)))
                       (or (plist-get (gm--get mb "/labels") 'labels) '()))))
          (set! *gm-labels* (cons (cons (gm--email mb) ls) *gm-labels*))
          ls))))

(define (gm--labels-reset! mb)
  (set! *gm-labels* (filter (lambda (e) (not (equal? (car e) (gm--email mb)))) *gm-labels*)))

;; the tag a label id reads as, or #f for one that stays hidden
(define (gm--tag mb id)
  (let ((sys (assoc id gm--system-tags)))
    (cond (sys (cadr sys))
          ((string-prefix? "Label_" id)
           (let ((l (assoc id (gm--labels mb)))) (and l (cadr l))))
          (else #f))))

(define (gm--tags mb ids)
  (filter (lambda (t) t) (map (lambda (id) (gm--tag mb id)) (or ids '()))))

;; the label id of TAG; CREATE? makes a missing user label
(define (gm--label-id mb tag create?)
  (let ((sys (filter (lambda (p) (equal? (cadr p) tag)) gm--system-tags)))
    (if (pair? sys)
        (car (car sys))
        (let ((hit (filter (lambda (l) (string-ci=? (cadr l) tag)) (gm--labels mb))))
          (cond ((pair? hit) (car (car hit)))
                ((not create?) #f)
                (else
                 (let ((made (gm--post mb "/labels"
                                       (list 'name tag 'labelListVisibility "labelShow"
                                             'messageListVisibility "show"))))
                   (gm--labels-reset! mb)
                   (plist-get made 'id))))))))

;;; --- queries

;; notmuch query text as words, parentheses apart, quotes kept inside a word
(define (gm--words q)
  (let loop ((cs (string->list q)) (cur '()) (quote-ch #f) (acc '()))
    (let ((flush (lambda () (if (null? cur) acc (cons (list->string (reverse cur)) acc)))))
      (cond ((null? cs) (reverse (flush)))
            (quote-ch (loop (cdr cs) (cons (car cs) cur) (if (char=? (car cs) quote-ch) #f quote-ch) acc))
            ((memv (car cs) '(#\" #\')) (loop (cdr cs) (cons (car cs) cur) (car cs) acc))
            ((memv (car cs) '(#\( #\))) (loop (cdr cs) '() #f (cons (list->string (list (car cs))) (flush))))
            ((char-whitespace? (car cs)) (loop (cdr cs) '() #f (flush)))
            (else (loop (cdr cs) (cons (car cs) cur) #f acc))))))

(define (gm--unquote s)
  (if (and (>= (string-length s) 2) (memv (string-ref s 0) '(#\" #\'))
           (char=? (string-ref s 0) (string-ref s (- (string-length s) 1))))
      (substring s 1 (- (string-length s) 1))
      s))

(define (gm--tag-term tag)
  (let ((sys (assoc tag '(("inbox" "in:inbox") ("unread" "is:unread") ("sent" "in:sent")
                          ("draft" "in:draft") ("spam" "in:spam") ("trash" "in:trash")
                          ("deleted" "in:trash") ("flagged" "is:starred") ("important" "is:important")
                          ("promotions" "category:promotions") ("social" "category:social")
                          ("forums" "category:forums") ("updates" "category:updates")))))
    (cond (sys (cadr sys))
          ((string-prefix? "/" tag) #f)
          (else (string-append "label:" (string-map (lambda (c) (if (memv c '(#\space #\/)) #\- c)) tag))))))

(define (gm--date-bound s after?)
  (cond ((equal? s "") #f)
        ((string-prefix? "@" s) (string-append (if after? "after:" "before:") (substring s 1 (string-length s))))
        ((re-match? "^[0-9]{4}-[0-9]{2}-[0-9]{2}$" s)
         (string-append (if after? "after:" "before:") (string-map (lambda (c) (if (char=? c #\-) #\/ c)) s)))
        ((and after? (re-match? "^[0-9]+[dwmy]$" s)) (string-append "newer_than:" s))
        (else #f)))

(define (gm--date-term v)
  (let* ((parts (string-split v ".."))
         (a (gm--date-bound (car parts) #t))
         (b (and (pair? (cdr parts)) (gm--date-bound (cadr parts) #f)))
         (terms (filter (lambda (x) x) (list a b))))
    (if (null? terms) #f (string-join terms " "))))

(define (gm--index s ch)
  (let loop ((i 0))
    (cond ((>= i (string-length s)) #f)
          ((char=? (string-ref s i) ch) i)
          (else (loop (+ i 1))))))

;; one word in Gmail search, or #f when it has none
(define (gm--term w)
  (let* ((neg? (string-prefix? "-" w))
         (w (if neg? (substring w 1 (string-length w)) w))
         (colon (gm--index w #\:))
         (key (and colon (string-downcase (substring w 0 colon))))
         (val (and colon (gm--unquote (substring w (+ colon 1) (string-length w)))))
         (out (cond ((not key) (if (equal? w "*") #f w))
                    ((equal? key "tag") (gm--tag-term val))
                    ((member key '("from" "to" "cc" "bcc" "subject"))
                     (string-append key ":" (if (gm--index val #\space) (string-append "\"" val "\"") val)))
                    ((equal? key "id") (string-append "rfc822msgid:" val))
                    ((equal? key "date") (gm--date-term val))
                    ((member key '("is" "in" "label" "has" "after" "before" "newer_than" "older_than"
                                   "larger" "smaller" "category" "filename" "rfc822msgid")) w)
                    (else #f))))
    (and out (if neg? (string-append "-" out) out))))

;; QUERY as (THREAD-IDS GMAIL-Q): the thread:ID terms it names, and the rest
;; in Gmail search
(define (gm--query query)
  (let loop ((ws (gm--words (or query ""))) (threads '()) (acc '()) (neg #f))
    (if (null? ws)
        ;; thread:A or thread:B leaves only ORs and parentheses as text: that
        ;; is a list of threads, not a search for the word OR
        (list (reverse threads)
              (if (null? (filter (lambda (w) (not (member w '("OR" "(" ")")))) acc))
                  ""
                  (string-join (reverse acc) " ")))
        (let* ((w (car ws)) (lw (string-downcase w)))
          (cond ((equal? lw "and") (loop (cdr ws) threads acc neg))
                ((equal? lw "or") (loop (cdr ws) threads (cons "OR" acc) neg))
                ((equal? lw "not") (loop (cdr ws) threads acc #t))
                ((string-prefix? "thread:" w)
                 (loop (cdr ws) (cons (substring w 7 (string-length w)) threads) acc #f))
                ((member w '("(" ")")) (loop (cdr ws) threads (cons w acc) #f))
                (else
                 (let ((t (gm--term w)))
                   (loop (cdr ws) threads
                         (if t (cons (if neg (string-append "-" t) t) acc) acc)
                         #f))))))))

;;; --- threads and messages

(define (gm--header msg name)
  (let ((hit (filter (lambda (h) (string-ci=? (or (plist-get h 'name) "") name))
                     (or (plist-get (plist-get msg 'payload) 'headers) '()))))
    (and (pair? hit) (plist-get (car hit) 'value))))

(define (gm--ts msg)
  (quotient (or (string->number (or (plist-get msg 'internalDate) "0")) 0) 1000))

(define (gm--relative ts)
  (let ((now (time->parts (current-time))) (then (time->parts ts)))
    (cond ((and (= (vector-ref now 0) (vector-ref then 0)) (= (vector-ref now 1) (vector-ref then 1))
                (= (vector-ref now 2) (vector-ref then 2)))
           (string-append "Today " (format-time ts "%H:%M")))
          ((< (- (current-time) ts) (* 6 86400)) (format-time ts "%a %H:%M"))
          ((= (vector-ref now 0) (vector-ref then 0)) (format-time ts "%b %d"))
          (else (format-time ts "%Y")))))

(define (gm--name from)
  (let* ((from (or from ""))
         (lt (gm--index from #\<))
         (name (if lt (string-trim (substring from 0 lt)) from)))
    (gm--unquote (if (equal? name "") from name))))

(define (gm--address from)
  (let* ((from (or from "")) (lt (gm--index from #\<)))
    (if lt
        (car (string-split (substring from (+ lt 1) (string-length from)) ">"))
        (string-trim from))))

(define (gm--dedupe xs)
  (reverse (fold (lambda (acc x) (if (member x acc) acc (cons x acc))) '() xs)))

;; threads.get with headers, cached by historyId: a thread Gmail has not
;; changed is not fetched again
(define *gm-threads* '())   ; ((ID HISTORY-ID JSON) ...), newest first

(define gm--meta-path
  "?format=metadata&metadataHeaders=From&metadataHeaders=To&metadataHeaders=Cc&metadataHeaders=Subject&metadataHeaders=Date&metadataHeaders=Message-ID")

(define (gm--thread-meta mb id history)
  (let ((hit (assoc id *gm-threads*)))
    (if (and hit history (equal? (cadr hit) history))
        (caddr hit)
        (let ((j (gm--call mb "GET" (string-append "/threads/" id gm--meta-path))))
          (set! *gm-threads*
                (cons (list id (plist-get j 'historyId) j)
                      (let ((rest (filter (lambda (e) (not (equal? (car e) id))) *gm-threads*)))
                        (list-head rest (min 2000 (length rest))))))
          j))))

(define (gm--thread-row mb j)
  (let* ((msgs (or (plist-get j 'messages) '()))
         (last (if (null? msgs) '() (list-ref msgs (- (length msgs) 1))))
         (ts (if (null? msgs) 0 (gm--ts last))))
    (list 'thread (plist-get j 'id)
          'timestamp ts
          'date_relative (gm--relative ts)
          'matched (length msgs)
          'total (length msgs)
          'authors (string-join (gm--dedupe (map (lambda (m) (gm--name (gm--header m "From"))) msgs)) ", ")
          'subject (or (and (pair? msgs) (gm--header (car msgs) "Subject")) "")
          'query (list (string-append "thread:" (plist-get j 'id)) #f)
          'tags (gm--dedupe (apply append (map (lambda (m) (gm--tags mb (plist-get m 'labelIds))) msgs))))))

;; (THREAD-ID HISTORY-ID) for QUERY, at most LIMIT
(define (gm--list-threads mb query limit)
  (let* ((q (gm--query query)) (ids (car q)) (text (cadr q)))
    (if (pair? ids)
        (map (lambda (id) (list id #f)) ids)
        (let loop ((token #f) (acc '()))
          (let* ((j (apply gm--get mb "/threads"
                           (append (list (list "maxResults" (number->string (min 500 (- limit (length acc))))))
                                   (if (equal? text "") '() (list (list "q" text)))
                                   (if token (list (list "pageToken" token)) '()))))
                 (acc (append acc (map (lambda (t) (list (plist-get t 'id) (plist-get t 'historyId)))
                                       (or (plist-get j 'threads) '())))))
            (if (and (plist-get j 'nextPageToken) (< (length acc) limit))
                (loop (plist-get j 'nextPageToken) acc)
                acc))))))

(define (gm--search-now mb query limit)
  (let ((js (gm--pmap (lambda (p) (gm--thread-meta mb (car p) (cadr p))) (gm--list-threads mb query limit))))
    (map (lambda (j) (gm--thread-row mb j)) (filter (lambda (j) (and j (pair? (plist-get j 'messages)))) js))))

;;; --- answers from the cache first
;;;
;;; Every Gmail request costs a round trip of a few hundred ms. A search or
;;; count answers from its last result at once, and a task asks Gmail again
;;; when the answer is older than gm-fresh-ms; a changed answer redraws the
;;; index. Threads are fetched whole ahead of the highlight, so a move draws
;;; from memory.

(define gm-fresh-ms 5000)
(define *gm-swr* '())   ; ((KEY TIME VALUE INFLIGHT) ...)

(define (gm--swr-put! key time value inflight)
  (set! *gm-swr* (cons (list key time value inflight)
                       (filter (lambda (e) (not (equal? (car e) key))) *gm-swr*))))

(define (gm--swr-set! key value inflight)
  (gm--swr-put! key (monotonic-ms) value inflight))


(define (gm--redraw-soon!)
  (debounce! 'gm-redraw 50
             (lambda (_)
               (when (and (buffer-exists? *notmuch-search-buffer*)
                          (symbol? (car (nm--current-mailbox))))
                 (nm--refresh! *notmuch-search-buffer*)))
             #f))

(define (gm--swr key compute)
  (let ((hit (assoc key *gm-swr*)))
    (cond ((not hit)
           (let ((v (compute))) (gm--swr-set! key v #f) v))
          (else
           (when (and (not (cadddr hit)) (> (- (monotonic-ms) (cadr hit)) gm-fresh-ms))
             (gm--swr-put! key (cadr hit) (caddr hit) #t)
             (task-run! compute
                        (lambda (ok? v)
                          (let ((old (caddr (or (assoc key *gm-swr*) hit))))
                            (gm--swr-set! key (if ok? v old) #f)
                            (when (and ok? (not (equal? v old))) (gm--redraw-soon!))))))
           (caddr hit)))))

;; the next answer for every cached search and count is Gmail's again
(define (gm--swr-stale!)
  (set! *gm-swr* (map (lambda (e) (list (car e) 0 (caddr e) (cadddr e))) *gm-swr*)))

(define (gm--search mb query limit)
  (let ((rows (gm--swr (list 'search (gm--email mb) query limit)
                       (lambda () (gm--search-now mb query limit)))))
    (gm--prefetch! mb (list-head (map (lambda (r) (plist-get r 'thread)) rows) (min 6 (length rows))))
    (set! *gm-last-rows* (map (lambda (r) (plist-get r 'thread)) rows))
    rows))

;;; whole threads, by historyId

(define *gm-full* '())        ; ((ID HISTORY-ID JSON) ...)
(define *gm-last-rows* '())   ; thread ids of the last index, in order
(define *gm-fetching* '())

(define (gm--known-history id)
  (let ((m (assoc id *gm-threads*))) (and m (cadr m))))

(define (gm--full-cached id)
  (let ((f (assoc id *gm-full*)))
    (and f (or (not (gm--known-history id)) (equal? (cadr f) (gm--known-history id))) (caddr f))))

(define (gm--full-put! id j)
  (set! *gm-full* (cons (list id (plist-get j 'historyId) j)
                        (let ((rest (filter (lambda (e) (not (equal? (car e) id))) *gm-full*)))
                          (list-head rest (min 300 (length rest)))))))

(define (gm--full mb id)
  (or (gm--full-cached id)
      (let ((j (gm--get mb (string-append "/threads/" id) (list "format" "full"))))
        (gm--full-put! id j)
        j)))

;; fetch IDS whole in the background, those not cached or in flight
(define (gm--prefetch! mb ids)
  (let ((todo (filter (lambda (id) (and (not (gm--full-cached id)) (not (member id *gm-fetching*)))) ids)))
    (unless (null? todo)
      (set! *gm-fetching* (append todo *gm-fetching*))
      (task-run! (lambda () (gm--pmap (lambda (id) (gm--full mb id)) todo))
                 (lambda (ok? v)
                   (set! *gm-fetching* (filter (lambda (id) (not (member id todo))) *gm-fetching*)))))))

;; the threads around ID in the last index
(define (gm--neighbours id n)
  (let loop ((ids *gm-last-rows*) (i 0))
    (cond ((null? ids) '())
          ((equal? (car ids) id)
           (let ((lo (max 0 (- i n))))
             (filter (lambda (x) (not (equal? x id)))
                     (list-head (list-tail *gm-last-rows* lo) (min (+ n n 1) (- (length *gm-last-rows*) lo))))))
          (else (loop (cdr ids) (+ i 1))))))

;; (MESSAGE-ID THREAD-ID) for Gmail search TEXT, every page up to LIMIT
(define (gm--list-messages mb text limit)
  (let loop ((token #f) (acc '()))
    (let* ((j (apply gm--get mb "/messages"
                     (append (list (list "maxResults" "500"))
                             (if (equal? text "") '() (list (list "q" text)))
                             (if token (list (list "pageToken" token)) '()))))
           (acc (append acc (map (lambda (m) (list (plist-get m 'id) (plist-get m 'threadId)))
                                 (or (plist-get j 'messages) '())))))
      (if (and (plist-get j 'nextPageToken) (< (length acc) limit))
          (loop (plist-get j 'nextPageToken) acc)
          acc))))

;;; --- a message as notmuch show JSON

(define *gm-parts* '())   ; ((MSG-ID (PART ATTACHMENT-ID DATA) ...) ...)

(define (gm--cid p)
  (let ((h (filter (lambda (h) (string-ci=? (or (plist-get h 'name) "") "Content-ID"))
                   (or (plist-get p 'headers) '()))))
    (and (pair? h)
         (string-trim (string-map (lambda (c) (if (memv c '(#\< #\>)) #\space c))
                                  (plist-get (car h) 'value))))))

;; PAYLOAD as notmuch parts, numbered depth first from 1 as notmuch numbers
;; them; each part's attachment id is kept for part-command
(define (gm--parts mb msg-id payload html?)
  (let ((n 0) (found '()))
    (define (walk p)
                     (set! n (+ n 1))
                     (let* ((id n)
                            (mt (string-downcase (or (plist-get p 'mimeType) "text/plain")))
                            (body (or (plist-get p 'body) '()))
                            (file (let ((f (plist-get p 'filename))) (and f (not (equal? f "")) f)))
                            (cid (gm--cid p))
                            (kids (plist-get p 'parts)))
                       (set! found (cons (list id (plist-get body 'attachmentId) (plist-get body 'data)) found))
                       (append
                        (list 'id id 'content-type mt)
                        (if file (list 'filename file) '())
                        (if cid (list 'content-id cid) '())
                        (cond ((pair? kids) (list 'content (map walk kids)))
                              ((and (not file) (plist-get body 'data)
                                    (or (equal? mt "text/plain") (and html? (equal? mt "text/html"))))
                               (list 'content (gm--decode (plist-get body 'data))))
                              (else (list 'content-length (or (plist-get body 'size) 0)))))))
    (let ((parts (list (walk payload))))
      (set! *gm-parts* (cons (cons msg-id found)
                             (filter (lambda (e) (not (equal? (car e) msg-id))) *gm-parts*)))
      parts)))

(define (gm--message mb m body? html?)
  (let ((ts (gm--ts m)))
    (list 'id (plist-get m 'id)
          'thread_id (plist-get m 'threadId)
          'match #t 'excluded #f 'filename '()
          'timestamp ts
          'date_relative (gm--relative ts)
          'tags (gm--tags mb (plist-get m 'labelIds))
          'headers (list 'Subject (or (gm--header m "Subject") "")
                         'From (or (gm--header m "From") "")
                         'To (or (gm--header m "To") "")
                         'Cc (gm--header m "Cc")
                         'Date (or (gm--header m "Date") "")
                         'Message-ID (gm--header m "Message-ID"))
          'body (if body? (gm--parts mb (plist-get m 'id) (plist-get m 'payload) html?) '()))))

;; a flat thread in notmuch's nesting: each message a (MESSAGE REPLIES) pair
(define (gm--forest msgs)
  (list (map (lambda (m) (list m '())) msgs)))

(define (gm--show mb thread html?)
  (when html? (gm--prefetch! mb (gm--neighbours thread 2)))
  (let ((j (if html?
               (gm--full mb thread)
               (or (gm--full-cached thread) (gm--thread-meta mb thread #f)))))
    (gm--forest (map (lambda (m) (gm--message mb m html? html?)) (or (plist-get j 'messages) '())))))

;;; --- tagging

;; CHANGES, "+a -b" or a list of words, as ((SIGN TAG) ...)
(define (gm--changes changes)
  (let ((ws (map gm--unquote
                 (if (string? changes)
                     (filter (lambda (w) (not (equal? w ""))) (string-split changes " "))
                     changes))))
    (map (lambda (w) (list (string-ref w 0) (substring w 1 (string-length w))))
         (filter (lambda (w) (and (> (string-length w) 1) (memv (string-ref w 0) '(#\+ #\-)))) ws))))

;; PLIST with KEY set to VALUE, as a new list
(define (gm--plist-set plist key value)
  (cond ((null? plist) (list key value))
        ((eq? (car plist) key) (cons key (cons value (cddr plist))))
        (else (cons (car plist) (cons (cadr plist) (gm--plist-set (cddr plist) key value))))))

(define (gm--relabel-json j add drop)
  (gm--plist-set j 'messages
    (map (lambda (m)
           (gm--plist-set m 'labelIds
             (append (filter (lambda (l) (not (member l drop))) (or (plist-get m 'labelIds) '()))
                     (filter (lambda (l) (not (member l (or (plist-get m 'labelIds) '())))) add))))
         (or (plist-get j 'messages) '()))))

;; thread ID as if Gmail had taken the change already: its headers, its
;; whole text and every cached index row that holds it
(define (gm--tag-terms-of query)
  (let loop ((ws (gm--words (or query ""))) (acc '()))
    (cond ((null? ws) (reverse acc))
          ((equal? (string-downcase (car ws)) "and") (loop (cdr ws) acc))
          ((member (car ws) '("(" ")")) #f)
          ((member (string-downcase (car ws)) '("or" "not")) #f)
          (else
           (let* ((w (car ws)) (neg? (string-prefix? "-" w)) (w (if neg? (substring w 1 (string-length w)) w))
                  (colon (gm--index w #\:)))
             (if (and colon (equal? (string-downcase (substring w 0 colon)) "tag"))
                 (loop (cdr ws) (cons (cons (gm--unquote (substring w (+ colon 1) (string-length w))) neg?) acc))
                 #f))))))

(define (gm--row-matches-tags? query tags)
  (let ((terms (gm--tag-terms-of query)))
    (or (not terms)
        (not (find (lambda (tn) (if (cdr tn) (member (car tn) tags) (not (member (car tn) tags)))) terms)))))

(define (gm--relabel-cached! id add drop cs)
  (let ((relabel (lambda (e) (if (equal? (car e) id) (list (car e) (cadr e) (gm--relabel-json (caddr e) add drop)) e))))
    (set! *gm-threads* (map relabel *gm-threads*))
    (set! *gm-full* (map relabel *gm-full*)))
  (let ((plus (map cadr (filter (lambda (c) (char=? (car c) #\+)) cs)))
        (minus (map cadr (filter (lambda (c) (char=? (car c) #\-)) cs))))
    (set! *gm-swr*
     (map
     (lambda (e)
       (if (not (and (pair? (car e)) (eq? (car (car e)) 'search)))
         e
         (let* ((query (caddr (car e)))
                (relabeled
                 (map (lambda (r)
                        (if (equal? (plist-get r 'thread) id)
                            (gm--plist-set r 'tags
                              (append (filter (lambda (t) (not (member t minus))) (plist-get r 'tags))
                                      (filter (lambda (t) (not (member t (plist-get r 'tags)))) plus)))
                            r))
                      (caddr e))))
           (list (car e) (cadr e)
             (filter (lambda (r) (or (not (equal? (plist-get r 'thread) id))
                                      (gm--row-matches-tags? query (plist-get r 'tags))))
                     relabeled)
             (cadddr e)))))
     *gm-swr*))))

;; a modify answers the thread: its new historyId keeps the caches current
(define (gm--history-from! j)
  (let ((id (and (pair? j) (plist-get j 'id))) (h (and (pair? j) (plist-get j 'historyId))))
    (when (and id h)
      (let ((rehist (lambda (e) (if (equal? (car e) id) (list (car e) h (caddr e)) e))))
        (set! *gm-threads* (map rehist *gm-threads*))
        (set! *gm-full* (map rehist *gm-full*))))))

(define (gm--tag! mb changes query)
  (let* ((cs (gm--changes changes))
         (add (filter (lambda (x) x) (map (lambda (c) (and (char=? (car c) #\+) (gm--label-id mb (cadr c) #t))) cs)))
         (drop (filter (lambda (x) x) (map (lambda (c) (and (char=? (car c) #\-) (gm--label-id mb (cadr c) #f))) cs)))
         (body (list 'addLabelIds add 'removeLabelIds drop))
         (q (gm--query query)))
    (cond ((and (null? add) (null? drop)) "")
          ((and (pair? (car q)) (equal? (cadr q) ""))
           ;; a thread by id: the caches change now, Gmail in the background
           (for-each (lambda (id) (gm--relabel-cached! id add drop cs)) (car q))
           (gm--redraw-soon!)
           (task-run! (lambda ()
                        (map (lambda (id) (gm--post mb (string-append "/threads/" id "/modify") body)) (car q)))
                      (lambda (ok? answers)
                        (if ok?
                            (for-each gm--history-from! answers)
                            (message "Gmail did not take the tag change; g reloads"))))
           "")
          (else
           (let* ((ms (gm--list-messages mb (cadr q) 100000))
                  (ms (if (pair? (car q)) (filter (lambda (m) (member (cadr m) (car q))) ms) ms)))
             (let loop ((ids (map car ms)))
               (unless (null? ids)
                 (let ((n (min 1000 (length ids))))
                   (gm--post mb "/messages/batchModify" (append (list 'ids (list-head ids n)) body))
                   (loop (list-tail ids n)))))
             "")))))

;;; --- counting

(define (gm--count mb query)
  (gm--swr (list 'count (gm--email mb) query) (lambda () (gm--count-now mb query))))

(define (gm--count-now mb query)
  (let* ((q (gm--query query)) (ids (car q)) (text (cadr q)))
    (cond ((and (pair? ids) (equal? text ""))
           (apply + (map (lambda (id)
                           (length (or (plist-get (gm--get mb (string-append "/threads/" id) (list "format" "minimal"))
                                                  'messages)
                                       '())))
                         ids)))
          ((pair? ids)
           (length (filter (lambda (m) (member (cadr m) ids)) (gm--list-messages mb text 5000))))
          (else
           (or (plist-get (apply gm--get mb "/messages"
                                 (append (list (list "maxResults" "1"))
                                         (if (equal? text "") '() (list (list "q" text)))))
                          'resultSizeEstimate)
               0)))))

;;; --- raw text and parts, through files the shell reads

(define (gm--b64-file name data)
  (let ((path (compos-temp-file name)))
    (write-file! path (gm--b64url->b64 data))
    path))

(define (gm--raw mb msg-id filter)
  (let* ((j (gm--get mb (string-append "/messages/" msg-id) (list "format" "raw")))
         (path (gm--b64-file (string-append "gmail-" msg-id ".b64") (or (plist-get j 'raw) ""))))
    (shell-command->string
     (string-append "base64 -D -i " (sh-quote path) (if (equal? filter "") "" (string-append " | " filter))))))

(define (gm--part-entry mb msg-id part)
  (let ((have (assoc msg-id *gm-parts*)))
    (unless (and have (assv part (cdr have)))
      (gm--parts mb msg-id (plist-get (gm--get mb (string-append "/messages/" msg-id) (list "format" "full")) 'payload) #t))
    (assv part (cdr (assoc msg-id *gm-parts*)))))

(define (gm--part-command mb msg-id part)
  (let* ((e (gm--part-entry mb msg-id part))
         (data (cond ((not e) "")
                     ((cadr e) (or (plist-get (gm--get mb (string-append "/messages/" msg-id "/attachments/" (cadr e))) 'data) ""))
                     (else (or (caddr e) "")))))
    (string-append "base64 -D -i "
                   (sh-quote (gm--b64-file (string-append "gmail-" msg-id "-" (number->string part) ".b64") data)))))

;;; --- identity, reply and send

(define (gm--send-as mb)
  (or (plist-get (gm--get mb "/settings/sendAs") 'sendAs) '()))

(define (gm--config mb key)
  (cond ((equal? key "user.primary_email") (gm--email mb))
        ((equal? key "user.name")
         (let ((d (filter (lambda (s) (plist-get s 'isDefault)) (gm--send-as mb))))
           (or (and (pair? d) (plist-get (car d) 'displayName)) "")))
        ((equal? key "user.other_email")
         (string-join (filter (lambda (e) (not (equal? e (gm--email mb))))
                              (map (lambda (s) (plist-get s 'sendAsEmail)) (gm--send-as mb)))
                      "\n"))
        (else "")))

(define (gm--mine? mb addr)
  (let ((a (string-downcase (gm--address addr))))
    (or (equal? a (string-downcase (gm--email mb)))
        (and (member a (map (lambda (s) (string-downcase (or (plist-get s 'sendAsEmail) ""))) (gm--send-as mb))) #t))))

(define (gm--split-addresses s)
  (filter (lambda (x) (not (equal? x "")))
          (map string-trim (string-split (or s "") ","))))

;; reply-all, as `notmuch reply --format=json` answers it
(define (gm--reply mb msg-id)
  (let* ((m (gm--get mb (string-append "/messages/" msg-id) (list "format" "full")))
         (orig (gm--message mb m #t #t)) ; html too: the reply quotes it
         (name (gm--config mb "user.name"))
         (me (if (equal? name "") (gm--email mb) (string-append name " <" (gm--email mb) ">")))
         (from (or (gm--header m "Reply-To") (gm--header m "From") ""))
         (sent-by-me? (gm--mine? mb (or (gm--header m "From") "")))
         (to (gm--dedupe (filter (lambda (a) (not (gm--mine? mb a)))
                                 (append (if sent-by-me? '() (list from))
                                         (gm--split-addresses (gm--header m "To"))))))
         (cc (filter (lambda (a) (and (not (gm--mine? mb a)) (not (member a to))))
                     (gm--split-addresses (gm--header m "Cc"))))
         (subject (or (gm--header m "Subject") ""))
         (mid (gm--header m "Message-ID"))
         (refs (gm--header m "References")))
    (list 'reply-headers
          (append (list 'From me
                        'To (string-join (if (null? to) (list from) to) ", ")
                        'Subject (if (string-prefix? "re:" (string-downcase subject)) subject (string-append "Re: " subject)))
                  (if (null? cc) '() (list 'Cc (string-join cc ", ")))
                  (if mid (list 'In-reply-to mid 'References (if refs (string-append refs " " mid) mid)) '()))
          'original orig)))

(define (gm--header-of text name)
  (let loop ((ls (string-split text "\n")))
    (cond ((null? ls) #f)
          ((equal? (string-trim (car ls)) "") #f)
          ((string-prefix? (string-append (string-downcase name) ":") (string-downcase (car ls)))
           (string-trim (substring (car ls) (+ (string-length name) 1) (string-length (car ls)))))
          (else (loop (cdr ls))))))

;; send RFC 822 TEXT; a reply joins the Gmail thread of the mail it answers
(define (gm--send mb text)
  (or (report-errors "Gmail send"
        (lambda ()
    (let* ((parent (gm--header-of text "In-Reply-To"))
           (thread (and parent
                        (let ((ms (gm--list-messages mb (string-append "rfc822msgid:" (gm--unquote parent)) 1)))
                          (and (pair? ms) (cadr (car ms))))))
           (raw (gm--b64->b64url (base64-encode text))))
      (gm--post mb "/messages/send" (append (list 'raw raw) (if thread (list 'threadId thread) '())))
      #t)))
      "Gmail send failed; *Messages* has why"))

;;; --- changes, for the mail feed and autotag

(define (gm--revision mb)
  (string->number (or (plist-get (gm--get mb "/profile") 'historyId) "")))

;; (MESSAGE-ID THREAD-ID) of every message added after history AFTER; none
;; when Gmail no longer keeps history that old
(define (gm--added mb after)
  (or (ignore-errors (lambda ()
    (let loop ((token #f) (acc '()))
      (let* ((j (apply gm--get mb "/history"
                       (append (list (list "startHistoryId" (number->string after))
                                     (list "historyTypes" "messageAdded")
                                     (list "maxResults" "500"))
                               (if token (list (list "pageToken" token)) '()))))
             (acc (append acc
                          (apply append
                                 (map (lambda (h)
                                        (map (lambda (a) (let ((m (plist-get a 'message)))
                                                           (list (plist-get m 'id) (plist-get m 'threadId))))
                                             (or (plist-get h 'messagesAdded) '())))
                                      (or (plist-get j 'history) '()))))))
        (if (and (plist-get j 'nextPageToken) (< (length acc) 5000))
            (loop (plist-get j 'nextPageToken) acc)
            acc)))))
      '()))

(define (gm--changed mb after since)
  (let* ((ids (gm--dedupe (map car (gm--added mb after))))
         (_ (unless (null? ids) (gm--swr-stale!) (gm--redraw-soon!)))
         (ms (gm--pmap (lambda (id) (gm--call mb "GET" (string-append "/messages/" id gm--meta-path))) ids)))
    (filter (lambda (m) (>= (plist-get m 'timestamp) since))
            (map (lambda (m) (gm--message mb m #f #f)) (filter (lambda (m) m) ms)))))

(define (gm--changed-threads mb after since rest)
  (let ((new (gm--dedupe (map cadr (gm--added mb after)))))
    (if (null? new)
        '()
        (let ((ok (map car (gm--list-threads mb (string-append "date:@" (number->string since) ".." rest) 2000))))
          (map (lambda (id) (string-append "thread:" id))
               (filter (lambda (id) (member id ok)) new))))))

;;; --- the rest

(define (gm--all-tags mb)
  (gm--dedupe (append (map cadr gm--system-tags)
                      (map cadr (filter (lambda (l) (string-prefix? "Label_" (car l))) (gm--labels mb))))))

(define (gm--senders mb query)
  (let* ((js (gm--pmap (lambda (p) (gm--thread-meta mb (car p) (cadr p))) (gm--list-threads mb query 200)))
         (froms (apply append (map (lambda (j) (map (lambda (m) (gm--address (gm--header m "From")))
                                                    (or (and j (plist-get j 'messages)) '())))
                                   js)))
         (counts (fold (lambda (acc a)
                         (let ((hit (assoc a acc)))
                           (if hit
                               (map (lambda (e) (if (equal? (car e) a) (list a (+ 1 (cadr e))) e)) acc)
                               (cons (list a 1) acc))))
                       '() froms)))
    (string-join (map (lambda (c) (string-append (number->string (cadr c)) "\t" (car c))) counts) "\n")))

;; a Gmail filter that sends ADDRESS to the trash; it needs the
;; gmail.settings.basic scope, and says so when the account lacks it
(define (gm--block-sender! mb email)
  (unless (ignore-errors
           (lambda ()
             (gm--post mb "/settings/filters"
                       (list 'criteria (list 'from email)
                             'action (list 'addLabelIds (list "TRASH") 'removeLabelIds (list "INBOX"))))))
    (message (string-append "Gmail could not make a filter for " email
                            " (it needs the gmail.settings.basic scope)")))
  email)

(mail-backend-register! 'gmail
  (list
   'search (lambda (mb query limit) (gm--search mb query limit))
   'show (lambda (mb thread html?) (gm--show mb thread html?))
   'changed (lambda (mb after since) (gm--changed mb after since))
   'changed-threads (lambda (mb after since rest) (gm--changed-threads mb after since rest))
   'revision (lambda (mb) (gm--revision mb))
   'tag! (lambda (mb changes query) (gm--tag! mb changes query))
   'count (lambda (mb query) (gm--count mb query))
   'count-batch (lambda (mb queries k)
                  (task-run! (lambda ()
                               (string-join (map (lambda (n) (if n (number->string n) ""))
                                                 (gm--pmap (lambda (q) (gm--count mb q)) queries))
                                            "\n"))
                             (lambda (ok? value) (k (if ok? value "")))))
   'tags (lambda (mb query)
           (if query
               (gm--dedupe (apply append (map (lambda (r) (plist-get r 'tags)) (gm--search mb query 50))))
               (gm--all-tags mb)))
   'threads (lambda (mb query)
              (map (lambda (p) (string-append "thread:" (car p))) (gm--list-threads mb query 2000)))
   'newest-message (lambda (mb thread)
                     (let ((ms (or (plist-get (gm--get mb (string-append "/threads/" thread) (list "format" "minimal")) 'messages) '())))
                       (and (pair? ms) (plist-get (list-ref ms (- (length ms) 1)) 'id))))
   'raw (lambda (mb msg-id filter) (gm--raw mb msg-id filter))
   'part-command (lambda (mb msg-id part) (gm--part-command mb msg-id part))
   'reply (lambda (mb msg-id) (gm--reply mb msg-id))
   'config (lambda (mb key) (gm--config mb key))
   'saved-searches (lambda (mb)
                     '(("inbox" "tag:inbox") ("unread" "tag:unread") ("flagged" "tag:flagged") ("sent" "tag:sent")))
   'senders (lambda (mb query) (gm--senders mb query))
   'block-sender! (lambda (mb email) (gm--block-sender! mb email))
   'send (lambda (mb text) (gm--send mb text))))
