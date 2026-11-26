;;; mail.scm --- the mail UI, over any mail backend, userland Scheme.
;;;
;;; No Elixir knows what email is. A backend reads and writes the mail:
;;; notmuch.scm runs the notmuch CLI over a local or remote index (sync is
;;; external, lieer/mbsync), mail-gmail.scm the Gmail API. This file
;;; renders search and thread buffers, tags, replies, and sends. The
;;; extensibility bar, same as dired: primitives + shell + json-parse.
;;;
;;; HTML first — we are in a browser. When a message carries a text/html
;;; part the thread buffer becomes an HTML document rendered by the UI's
;;; sandboxed iframe (render-mode "html" + preview-authored); v toggles the
;;; text view. Tools always read text, through notmuch-html-renderer when
;;; a message has no text/plain part.
;;;
;;; Search buffer keys (ported from the user's Emacs config):
;;;   n/p next/prev (n marks read; both auto-preview) · RET open · v preview
;;;   a archive · d trash · u smart-untag · . toggle unread · @ by sender
;;;   SPC mark+advance · M or * mark all (again unmarks) · U unmark all · F show marked
;;;   a archive marked · d trash marked · t tag marked · T tag this thread
;;;   / custom query filter · l add a tag filter · \ remove the last filter
;;;   s new search · g refresh · q quit
;;; Thread buffer keys:  v html/text view · a archive · r reply · q quit
;;; Compose buffer keys: C-c C-c send · C-c C-k abort
;;;
;;; Chat integration: context providers tell chat/agent what "this" means
;;; (the selected search line, or the thread being read), and tools let
;;; the model search and read mail itself.

(require 'tabulated-list)
(require 'detail)
(require 'events)

(defgroup 'notmuch "Email: notmuch search, reading, and sending.")


(defcustom 'notmuch-profile #f
  "NOTMUCH_PROFILE for every call. #f until a mailbox is chosen; \"\" is the default database."
  'group 'notmuch)
;;; The mail store can live on another machine. One host at a time: search,
;;; show, count and tag all run where the database is, so a thread id always
;;; means the same thing.
(defcustom 'notmuch-host #f
  "Machine that owns the mail store. #f until a mailbox is chosen; \"\" is this one,
anything else is an ssh destination."
  'group 'notmuch)

(defcustom 'notmuch-mailboxes '(("" ""))
  "The mailboxes notmuch-switch-host offers, each (HOST PROFILE); empty host is this machine. (gmail ADDRESS) reads a connected Google account through the Gmail API."
  'group 'notmuch)

(defcustom 'notmuch-ssh-program
  "ssh -o BatchMode=yes -o ControlMaster=auto -o ControlPath=/tmp/compos-ssh-%C -o ControlPersist=300"
  "How a remote mail host is reached; one multiplexed connection keeps a call cheap."
  'group 'notmuch)

;; How many threads a search buffer shows.
(define notmuch-search-limit 50)
(defcustom 'notmuch-default-query "tag:inbox"
  "The query the notmuch command opens with." 'group 'notmuch)
(defcustom 'notmuch-prefer-html #t
  "Render threads as HTML when a message has an HTML part (v toggles text)."
  'group 'notmuch)
(defcustom 'notmuch-html-original-colors #f
  "Render emails with their authored colors on a white canvas. Off, the
theme repaints them (shr-style: layout survives, colors follow the theme
— readable in dark mode)." 'group 'notmuch)
(defcustom 'notmuch-html-renderer "w3m -dump -O utf-8 -T text/html"
  "Command that turns HTML into text, for the text view and the mail tools
when a message has no text/plain part." 'group 'notmuch)
(defcustom 'notmuch-show-newest-first #t
  "Show the newest message of a thread first." 'group 'notmuch)
(defcustom 'notmuch-auto-preview #t
  "n/p in the search buffer preview the thread in the other window."
  'group 'notmuch)

(defcustom 'notmuch-preview-delay 500
  "How long the highlight rests on a row before the mail pane fetches it, in milliseconds. A fetch is a round trip to the mail host, so a held n reaches the row you want before one call goes out. 0 fetches on every move."
  'group 'notmuch 'type 'number)


;; (substring-of-From-or-filename  send-command) — first match wins,
;; "" is the fallback route. set! your accounts' routes in init.scm.
(define notmuch-send-routes
  '(("" "msmtp -t")))

(define *notmuch-search-buffer* "*notmuch*")

;; The mail views are singletons: one *mailboxes*, one *notmuch*, one
;; *mail* for the whole editor, and a buffer lives in ONE group. What ties
;; the three together is this app id, not a group of their own: an app that
;; owned a named group pulled the frame into that group's saved layout the
;; moment any of its panes was shown, which changed the arrangement under
;; the reader. The id says "these three are one app" wherever they are.
(define *nm-app-id* "mail")


(define (nm--join-group! buf &optional role)
  ;; The three views are one app, not three visitors. What makes them one is
  ;; the app id they carry, not a group of their own: they open in the group
  ;; the frame stands in, like every other buffer, and app-buffers finds them
  ;; there whatever that group is.
  (when (buffer-exists? buf)
    (when (boundp 'app-claim!) (app-claim! buf *nm-app-id* (or role 'aux)))
    (when (boundp 'buffer-join-here!) (buffer-join-here! buf)))
  buf)

;; A search has one base and a stack of added terms. Commands change only
;; these two locals. The effective query is derived, so no command can leave
;; the rows and the filter stack describing different searches.
(define (nm--query-with-filters base filters)
  (if (null? filters)
      base
      (let ((filter (car filters)))
        (if (and (pair? filter) (equal? (car filter) "only"))
            (cadr filter)
            (string-append "( "
                           (nm--query-with-filters base (cdr filters))
                           " ) and " filter)))))

(define (nm--query-base-of buf)
  (or (buffer-local-value 'notmuch-query-base buf)
      ;; a buffer with no base yet searches the default
      (let ((base notmuch-default-query))
        (buffer-set-local! buf 'notmuch-query-base base)
        (buffer-set-local! buf 'notmuch-query-filters '())
        base)))

(define (nm--query-filters-of buf)
  (or (buffer-local-value 'notmuch-query-filters buf) '()))

(define (nm--query-of buf)
  (nm--query-with-filters (nm--query-base-of buf)
                          (nm--query-filters-of buf)))

(define (nm--query-reset! buf query)
  (buffer-set-local! buf 'notmuch-selection #f)
  (buffer-set-local! buf 'notmuch-query-base query)
  (buffer-set-local! buf 'notmuch-query-filters '())
  (buffer-set-local! buf 'notmuch-query-positions '()))

(define (nm--query-push! buf term)
  (buffer-set-local! buf 'notmuch-selection #f)
  (nm--query-base-of buf)
  (buffer-set-local! buf 'notmuch-query-positions
    (cons (nm--position-of buf)
          (or (buffer-local-value 'notmuch-query-positions buf) '())))
  (buffer-set-local! buf 'notmuch-query-filters
    (cons term (nm--query-filters-of buf))))

(define (nm--query-push-only! buf term)
  (buffer-set-local! buf 'notmuch-selection #f)
  (nm--query-base-of buf)
  (buffer-set-local! buf 'notmuch-query-positions
    (cons (nm--position-of buf)
          (or (buffer-local-value 'notmuch-query-positions buf) '())))
  (buffer-set-local! buf 'notmuch-query-filters
    (cons (list "only" term) (nm--query-filters-of buf))))

(define (nm--query-pop! buf)
  (let ((filters (nm--query-filters-of buf)))
    (if (null? filters)
        #f
        (begin
          (buffer-set-local! buf 'notmuch-selection #f)
          (buffer-set-local! buf 'notmuch-query-filters (cdr filters))
          (let ((positions (or (buffer-local-value 'notmuch-query-positions buf) '())))
            (unless (null? positions)
              (buffer-set-local! buf 'notmuch-query-positions (cdr positions))))
          #t))))

;;; --- CLI plumbing -------------------------------------------------------------

;;; A mailbox is a host and a profile, chosen together. Neither has a usable
;;; default: an unset value used to fall through to whatever database this
;;; machine opens by itself, and every count, search and tag then answered
;;; from the wrong mail store without a word. Unset stops the call instead.
(define (nm--mailbox-set?)
  (and (string? notmuch-host) (string? notmuch-profile)))

(define (nm--mailbox-check!)
  (unless (nm--mailbox-set?)
    (error (string-append
             "No notmuch mailbox chosen: host "
             (if (string? notmuch-host) (string-append "\"" notmuch-host "\"") "unset")
             ", profile "
             (if (string? notmuch-profile) (string-append "\"" notmuch-profile "\"") "unset")
             ". Run M-x notmuch-switch-host. Refusing to read this machine's default database."))))



(define (nm--host-label)
  (cond ((not (string? notmuch-host)) "no mailbox")
        ((equal? notmuch-host "") "this machine")
        (else notmuch-host)))

(define (nm--mailbox-label host profile)
  (string-append (if (equal? host "") "local" host)
                 (if (equal? profile "") "" (string-append " [" profile "]"))))

;; which notmuch: several databases answer to the same program name, so
;; the identity is the host, the profile, and the database that profile
;; opens. The path costs one round trip, so hold it per identity and drop
;; the cache when the host changes.
(define *nm-db-paths* '())

(define (nm--db-key)
  (string-append (nm--host-label) "|" (if (string? notmuch-profile) notmuch-profile "unset")))

(define (nm--db-path)
  (let ((hit (assoc (nm--db-key) *nm-db-paths*)))
    (if hit
        (cadr hit)
        (let ((path (string-trim (nm--op 'config "database.path"))))
          ;; a failed lookup is not an answer: leave it uncached and retry
          (when (not (equal? path ""))
            (set! *nm-db-paths* (cons (list (nm--db-key) path) *nm-db-paths*)))
          path))))

(define (nm--source-label)
  (let ((path (nm--db-path)))
    (string-append (nm--host-label)
                   (if (or (not (string? notmuch-profile)) (equal? notmuch-profile ""))
                       ""
                       (string-append " [" notmuch-profile "]"))
                   (if (equal? path "") "" (string-append ":" path)))))

;; The header names the account first, then where its mail lives.
(define (nm--account-label)
  (if (and (string? notmuch-profile) (not (equal? notmuch-profile "")))
      notmuch-profile
      "Mail"))

(define (nm--short-path path)
  ;; /home/svs/Mail/x and /Users/svs/Mail/x read as ~/Mail/x
  (let ((parts (string-split path "/")))
    (if (and (> (length parts) 3) (member (cadr parts) '("home" "Users")))
        (string-join (cons "~" (cdddr parts)) "/")
        path)))

(define (nm--where-label)
  (if (not (string? notmuch-host))
      (nm--host-label)
      (let ((path (nm--db-path)))
        (string-append "on " (nm--host-label)
                       (if (equal? path "") "" (string-append " · " (nm--short-path path)))))))

(define (nm--query-label query)
  ;; a lone tag:NAME reads as its name
  (if (and (string-prefix? "tag:" query) (> (string-length query) 4)
           (not (string-contains? query " ")))
      (string-append (string-upcase (substring query 4 5))
                     (substring query 5 (string-length query)))
      query))







;;; --- mail backends -----------------------------------------------------------
;;; A mailbox names its backend. (HOST PROFILE), two strings, is a notmuch
;;; database on HOST; (KIND ARG ...) with a symbol KIND names a registered
;;; backend, e.g. (gmail "me@example.com"). The UI reads and writes mail only
;;; through these operations, so it is the same over every backend. Each
;;; operation takes the mailbox first (#f is the chosen one) and queries in
;;; notmuch syntax; a backend translates what it can and filters the rest.
;;;
;;;   search MB QUERY LIMIT          threads as `search --format=json`
;;;   show MB THREAD HTML?           the thread as `show --format=json`
;;;   changed MB AFTER SINCE         messages changed after revision AFTER and
;;;                                  dated after SINCE, as `show` with no bodies
;;;   changed-threads MB AFTER SINCE REST
;;;                                  thread lines changed after AFTER, REST a
;;;                                  notmuch clause starting with " and "
;;;   revision MB                    the change counter, a number or #f
;;;   tag! MB CHANGES QUERY          CHANGES "+a -b" or a list of "+a" "-b"
;;;   count MB QUERY                 messages matching, a number or #f
;;;   count-batch MB QUERIES K       K gets one count per line, without blocking
;;;   tags MB QUERY                  the tags on QUERY's messages; #f is all
;;;   threads MB QUERY               "thread:ID" lines
;;;   newest-message MB THREAD       the newest message id, or #f
;;;   raw MB MSG-ID FILTER           the RFC 822 text through shell FILTER ("" none)
;;;   part-command MB MSG-ID PART    a shell command that prints that part
;;;   reply MB MSG-ID                reply headers and original, as `reply --format=json`
;;;   config MB KEY                  a setting: user.name, user.primary_email,
;;;                                  user.other_email, database.path
;;;   saved-searches MB              ((NAME QUERY) ...)
;;;   senders MB QUERY               "COUNT\tADDRESS" lines
;;;   block-sender! MB ADDRESS       future mail from ADDRESS is junked on arrival
;;;   send MB TEXT                   optional: send RFC 822 TEXT; #t or the error

(define *mail-backends* '())

(define (mail-backend-register! kind ops)
  "(mail-backend-register! KIND OPS) — make KIND a mail backend; OPS is a plist of operation procedures"
  (set! *mail-backends*
        (cons (cons kind ops)
              (filter (lambda (b) (not (eq? (car b) kind))) *mail-backends*)))
  kind)

(define (nm--mailbox-kind mailbox)
  (if (and (pair? mailbox) (symbol? (car mailbox))) (car mailbox) 'notmuch))

;; The chosen mailbox. notmuch-host holds "KIND:ARG" for a mailbox of a
;; registered backend other than notmuch, e.g. "gmail:me@example.com".
(define (nm--current-mailbox)
  (nm--pair->mailbox notmuch-host notmuch-profile))

(define (nm--pair->mailbox host profile)
  (let* ((m (and (string? host) (re-match "^([a-z]+):(.+)$" host)))
         (kind (and m (string->symbol (cadr m)))))
    (if (and kind (not (eq? kind 'notmuch)) (assq kind *mail-backends*))
        (list kind (caddr m))
        (list host profile))))

;; An entry of notmuch-mailboxes as the (HOST PROFILE) strings the chosen
;; mailbox is kept in: (gmail "me@example.com") is ("gmail:me@example.com" "").
(define (nm--entry->pair entry)
  (if (symbol? (car entry))
      (list (string-append (symbol->string (car entry)) ":" (cadr entry)) "")
      entry))

(define (nm--backend &optional mailbox)
  (let* ((kind (nm--mailbox-kind (or mailbox (nm--current-mailbox))))
         (hit (assq kind *mail-backends*)))
    (if hit
        (cdr hit)
        (error (string-append "No mail backend " (symbol->string kind))))))

(define (nm--backend-has? name &optional mailbox)
  (and (plist-get (nm--backend mailbox) name) #t))

;; (nm--op-on MAILBOX NAME ARG ...) runs operation NAME of MAILBOX's backend;
;; (nm--op NAME ARG ...) runs it on the chosen mailbox.
(define (nm--op-on mailbox name . args)
  (let ((f (plist-get (nm--backend mailbox) name)))
    (if f
        (apply f mailbox args)
        (error (string-append "This mailbox cannot " (symbol->string name))))))

(define (nm--op name . args)
  (apply nm--op-on #f name args))

















(define (nm--fit s n)
  (let ((s (or s "")))
    (if (> (string-length s) n)
        (substring s 0 n)
        (string-pad-right s n))))

(define (nm--trunc s n)
  (let ((s (or s "")))
    (if (> (string-length s) n) (substring s 0 n) s)))

(define (nm--html->text html)
  (let ((tmp (compos-temp-file "mail-part.html")))
    (write-file! tmp html)
    (shell-command->string
      (string-append notmuch-html-renderer " < " (sh-quote tmp)))))

;;; --- search buffer ------------------------------------------------------------

;; The index rows are a character grid: every row is padded to the same column
;; count, so the date and the tags right-align by counting characters. A face
;; here may set colour and weight; it must not set size, because a segment at a
;; different size breaks that alignment. The reading order is carried by
;; contrast instead: unread subject, then read subject, then author, then date
;; and tags. Each value below clears 4.5:1 on the paper background.
(defface! 'nm-date 'fg "#676257")
(defface! 'nm-author 'fg "#515c86" 'weight "400" 'style "italic")
(defface! 'nm-unread 'weight "700")
(defface! 'nm-subject 'fg "color-mix(in srgb, var(--default-fg) 78%, var(--default-bg))")
(defface! 'nm-tags 'fg "#64603a")
(defface! 'nm-marked 'fg "#a03020" 'weight "700")

;; A marked thread wears a bar and a tint, so a selection reads at a glance
;; in the block view. The tint skips the cursor's row, so the cursor's own
;; background still shows on a marked row and you can tell where you are.
(define-style! 'notmuch-marked-rows "
mail-threads > .semantic-direct[marked=true],
mail-threads mail-thread[marked=true] {
  box-shadow: inset 4px 0 var(--accent-fg, #e37170);
}
mail-threads mail-thread[marked=true]:not(.selected) {
  background-color: color-mix(in srgb, var(--accent-fg, #e37170) 18%, transparent);
}
")
(defface! 'nm-bar 'fg "#26356b")

(define (nm--search-json query limit)
  (or (nm--op 'search query limit) '()))

;; stored per row: (thread-id subject authors tags date)
(define (nm--th-id th) (car th))
(define (nm--th-subject th) (cadr th))
(define (nm--th-authors th) (caddr th))
(define (nm--th-tags th) (list-ref th 3))
(define (nm--th-date th) (list-ref th 4))

(define (nm--search-rows buf)
  (let* ((query (nm--query-of buf))
         (rows (map (lambda (th) (list (plist-get th 'thread)
                                      (or (plist-get th 'subject) "")
                                      (or (plist-get th 'authors) "")
                                      (or (plist-get th 'tags) '())
                                      (or (plist-get th 'date_relative) "")))
                    (nm--search-json query notmuch-search-limit))))
    (nm--draw-marks! buf rows)
    ;; the tag column measures itself against the threads this search
    ;; found. It reads the number here, and not from the entries: a draw
    ;; lays the columns out while the entries it replaces are still the
    ;; ones on the buffer.
    (buffer-set-local! buf 'nm-tags-need
      (fold (lambda (acc th) (max acc (string-length (nm--tags-text th)))) 6 rows))
    rows))

;; Two lines per thread. The subject owns the first line, so a narrow
;; window never truncates it; the date sits at the right of that line,
;; where a compressed date still reads. The author and the tags share
;; the second line, and the bar in front of the row says the tag you
;; read first: unread.
;;
;; The reader sees EVERY tag a thread carries, and reads each one in
;; full while the column has the room. A narrow window takes that room
;; back: then a tag longer than five characters keeps its first three
;; letters and says the rest is missing, so "attachment" reads "att..".
;; The author is what gives way first.
(define nm-tag-width 5)

(define (nm--short-tag t)
  (if (> (string-length t) nm-tag-width)
      (string-append (substring t 0 3) "..")
      t))

(define (nm--fit-tags s w)
  (let* ((tags (filter (lambda (t) (not (equal? t ""))) (string-split s " ")))
         (full (string-join tags " "))
         (short (string-join (map nm--short-tag tags) " ")))
    (cond ((null? tags) "")
          ;; the column asks for what the busiest row needs, so a tag
          ;; normally reads under its own name
          ((<= (string-length full) w) full)
          ((<= (string-length short) w) short)
          ;; not even the short forms fit: one letter per tag, and the
          ;; reader still counts them
          (else (string-join (map (lambda (t) (substring t 0 1)) tags) "")))))

;; A hidden tag is a real tag: it stays in the database, stays searchable, and
;; still completes in notmuch-remove-tag. It just never draws. Every row carries
;; inbox, the bar already says unread, and compos-mark is drawn as a face, so a
;; row that prints them spends width saying what it has already said.
(defcustom 'notmuch-hidden-tags '("inbox" "unread" "compos-mark")
  "Tags the thread list never draws.")

(define (nm--visible-tags th)
  (filter (lambda (t) (not (member t notmuch-hidden-tags))) (nm--th-tags th)))

(define (nm--tags-text th)
  (string-join (nm--visible-tags th) " "))

;; The tag column is as wide as the busiest thread of this search needs,
;; so every thread shows every tag it has. It stops at half the window:
;; past that the author has nothing left to say, and a thread with that
;; many tags falls back to the initials.
(define (nm--tags-width buf)
  (let ((need (or (buffer-local-value 'nm-tags-need buf) 6)))
    (min need (max 10 (quotient (list-view-width buf) 2)))))

(define (nm--search-columns buf)
  (list (list (list "" 1) (list "subject" #f 'left 'end) (list "date" 13 'right))
        (list (list "" 1) (list "author" #f)
              (list "tags" (nm--tags-width buf) 'right nm--fit-tags))))

(define (nm--subject-face tags)
  (cond ((member "unread" tags) "nm-unread")
        (else "nm-subject")))

(define (nm--search-cells buf th)
  (let* ((tags (nm--th-tags th))
         (bar (if (member "unread" tags) "▌" " ")))
    (list (list (list bar "nm-bar")
                (list (nm--th-subject th) (if (nm--row-marked? buf th) "nm-marked" (nm--subject-face tags)))
                (list (nm--th-date th) "nm-date"))
          (list " "
                (list (nm--th-authors th) "nm-author")
                (list (nm--tags-text th) "nm-tags")))))

;; every draw asks for this line, and one draw must not cost a round
;; trip: count once per database and query, and let nm--refresh! drop the
;; answer when the mail behind it can have changed.
(define (nm--count-for buf query)
  (let ((key (string-append (nm--source-label) "|" query))
        (hit (buffer-local-value 'nm-count buf)))
    (if (and (pair? hit) (equal? (car hit) key))
        (cadr hit)
        ;; count, and not count --output=threads: grouping threads over a
        ;; large query costs seconds
        (let ((n (let ((c (nm--op 'count query))) (if c (number->string c) ""))))
          (buffer-set-local! buf 'nm-count (list key n))
          n))))

(define (nm--selection-label buf)
  (let ((s (nm--selection buf)))
    (if (not (nm--any-marked? buf)) ""
      (let ((n (length (list-ref s 3))))
        (if (caddr s)
            (if (= n 0)
                (string-append "All " (nm--count-for buf (nm--query-of buf)) " matching messages selected")
                (string-append "All matching messages selected except " (number->string n)
                               (if (= n 1) " thread" " threads")))
            (string-append (number->string n) (if (= n 1) " thread selected" " threads selected")))))))

(define (nm--search-meta buf)
  (let ((selection (nm--selection-label buf)))
    (string-append (if (equal? selection "") "" (string-append selection " · "))
                   (nm--count-for buf (nm--query-of buf)) " messages · "
                   (nm--where-label))))

;; the list machinery owns the refresh, the row lookup and the header
;; offset (R8); these names stay for the commands and tests that call them
(define (nm--refresh! buf)
  (buffer-set-local! buf 'nm-count #f)
  (buffer-set-local! buf 'nm-tab-counts #f)
  (list-refresh! buf))
(define (nm--index-at buf) (list-index buf))
(define (nm--thread-at buf) (list-current buf))

(mode-icon! "notmuch-mode" "")

;; Footer hints read the buffer's own keymap. A hardcoded hint goes stale the
;; moment a key is rebound or taken by another map, so only the label is written
;; here and the key is looked up.
(define (nm--footer-key buf names)
  (let loop ((n (if (pair? names) names (list names))))
    (if (null? n)
        ""
        (let ((k (key-for-command (car n) buf)))
          (if (equal? k "") (loop (cdr n)) k)))))

(define (nm--footer buf specs)
  (let loop ((s specs) (acc '()))
    (if (null? s)
        (reverse acc)
        (let ((k (nm--footer-key buf (car (car s)))))
          (loop (cdr s)
                (if (equal? k "") acc (cons (list k (cadr (car s))) acc)))))))

;; Fields keep their full values; CSS controls the visual compression.
(define (nm--field tag value &optional field)
  (list 'tag tag 'attrs (if field (list (list "field" field)) '()) 'text (if (number? value) (number->string value) (or value ""))))

(define (nm--thread-composml buf th)
  (list 'tag "mail-thread"
        'attrs (list (list "unread" (if (member "unread" (nm--th-tags th)) "true" "false"))
                     (list "marked" (if (nm--row-marked? buf th) "true" "false")))
        'children
        (list (nm--field "mail-subject" (nm--th-subject th) "primary")
              (nm--field "mail-date" (nm--th-date th) "trailing")
              (nm--field "mail-participants" (nm--th-authors th) "secondary")
              (list 'tag "mail-tags" 'attrs '(("field" "tags")) 'children
                    (map (lambda (tag) (nm--field "mail-tag" tag))
                         (nm--visible-tags th))))))

(define (nm--click-thread! buf th)
  ;; The shared list already moved point to this record. Match n/p behavior.
  (nm--maybe-preview! buf))

;; Tabs over the list: each one is a whole search, not a filter on top.
;; Taking a tab replaces the base query and drops the filters.
(define nm-tabs-notmuch
  '(("inbox" "Inbox" "tag:inbox")
    ("unread" "Unread" "tag:inbox and tag:unread")
    ("important" "Important" "tag:inbox and tag:important")
    ("newsletter" "Newsletters" "tag:newsletter and date:30d..")))

;; A Gmail mailbox gets Gmail's own category tabs; its backend reads each
;; category as a tag.
(define nm-tabs-gmail
  '(("primary" "Primary" "tag:inbox and tag:personal")
    ("promotions" "Promotions" "tag:inbox and tag:promotions")
    ("social" "Social" "tag:inbox and tag:social")
    ("updates" "Updates" "tag:inbox and tag:updates")
    ("forums" "Forums" "tag:inbox and tag:forums")))

(define (nm--tab-list)
  (if (string-prefix? "gmail:" notmuch-host) nm-tabs-gmail nm-tabs-notmuch))

;; Thread counts for every tab in one trip to the mail host, kept until the
;; next refresh (g, a tab, or a tag change) so a redraw costs nothing.
(define (nm--tab-counts buf)
  ;; through the backend, in a task: a redraw never waits on the mail host.
  ;; The tabs draw without counts until they arrive, then draw again.
  (or (buffer-local-value 'nm-tab-counts buf)
      (begin
        (unless (buffer-local-value 'nm-tab-counts-pending buf)
          (buffer-set-local! buf 'nm-tab-counts-pending #t)
          (task-run!
            (lambda ()
              (map (lambda (t)
                     (let ((n (ignore-errors (lambda () (nm--op 'count (caddr t))))))
                       (if (number? n) (number->string n) "")))
                   (nm--tab-list)))
            (lambda (ok? counts)
              (when (buffer-known? buf)
                (buffer-set-local! buf 'nm-tab-counts-pending #f)
                (when ok?
                  (buffer-set-local! buf 'nm-tab-counts counts)
                  (list-refresh! buf))))))
        (map (lambda (t) "") (nm--tab-list)))))

(define (nm--tabs buf)
  (let ((base (nm--query-base-of buf))
        (plain? (null? (nm--query-filters-of buf)))
        (counts (nm--tab-counts buf)))
    (tab-bars-keyed 'index
      (let loop ((ts (nm--tab-list)) (cs counts) (acc '()))
        (if (null? ts) (reverse acc)
            (let ((t (car ts)) (c (car cs)))
              (loop (cdr ts) (cdr cs)
                    (cons (list (string-append "notmuch-tab-" (car t))
                                (if (equal? c "") (cadr t) (string-append (cadr t) " " c))
                                (and plain? (equal? base (caddr t))))
                          acc))))))))

(define (nm--take-tab! buf id)
  (let ((t (assoc id (nm--tab-list))))
    (when t
      (buffer-set-local! buf 'notmuch-selection #f)
      (buffer-set-local! buf 'notmuch-query-base (caddr t))
      (buffer-set-local! buf 'notmuch-query-filters '())
      (buffer-set-local! buf 'notmuch-query-positions '())
      (nm--refresh! buf)
      (list-goto-first-entry buf)
      #t)))

(add-hook! (list 'block-click 'notmuch)
  (lambda (buf id)
    (and (string? id) (string-prefix? "notmuch-tab-" id)
         (buffer-exists? buf) (buffer-derived-mode? buf "notmuch-mode")
         (nm--take-tab! buf (substring id 12 (string-length id))))))

(define-list-mode! "notmuch-mode"
  (list
    'doc (string-append
           "One notmuch search as a list of threads. `RET` opens, `v` "
           "previews, `a`/`d` tag, `t` classifies with the mailbox's own tags, "
           "`L` files a thread for the agent (+liked -inbox), "
           "`0` strips every tag, `SPC` marks and the capital keys act "
           "on every marked thread. Selection is local and clears after bulk actions or filter changes. `/` adds a custom query filter. "
           "`l` adds a tag filter; `\\` removes it. "
           "`s` starts a new search; `q` removes the last filter, "
           "or goes back to mailboxes when no filters remain.")
    'on-click (lambda (buf th) (nm--click-thread! buf th))
    'composml-root (lambda (buf)
      (list 'tag "mailbox" 'attrs
        (list (list "source" (nm--host-label))
              (list "profile" (if (string? notmuch-profile) notmuch-profile ""))
              (list "query" (nm--query-of buf)))))
    'collection "mail-threads"
    'head-blocks (lambda (buf) (list (component 'ui/tabs (list 'class "notmuch-tabs" 'tabs (nm--tabs buf)))))
    'composml (lambda (buf th) (nm--thread-composml buf th))
    'rows (lambda (buf) (nm--search-rows buf))
    'key (lambda (buf th) (nm--th-id th))
    'selection-face "select"
    'mark-command "notmuch-mark-toggle"
    'row-columns (lambda (buf) (nm--search-columns buf))
    'row-cells (lambda (buf th) (nm--search-cells buf th))
    'title (lambda (buf)
             (let ((q (nm--query-of buf)))
               (string-append (nm--account-label) " · " (nm--query-label q)
                              " · " (nm--count-for buf q))))
    'meta (lambda (buf) (nm--search-meta buf))
    'total (lambda (buf) (length (list-entries buf)))
    'footer (lambda (buf)
              (nm--footer buf
                (if (nm--any-marked? buf)
                    '(("notmuch-archive" "archive") ("notmuch-trash" "trash")
                      ("notmuch-autotag" "autotag")
                      ("notmuch-tag-marked" "tag") ("notmuch-like" "like")
                      ("notmuch-filter-marked" "filter")
                      ("notmuch-unmark-all" "unmark")
                      ("notmuch-mark-toggle" "unmark this")
                      ("notmuch-open-thread" "open")
                      ("notmuch-refresh" "refresh")
                      (("notmuch-back" "dismiss-buffer") "back"))
                    '(("notmuch-open-thread" "open") ("notmuch-preview" "preview")
                      ("notmuch-mark-toggle" "mark")
                      ("notmuch-archive" "archive") ("notmuch-trash" "trash")
                      ("notmuch-edit-tags" "tag") ("notmuch-autotag" "autotag")
                      ("notmuch-like" "like")
                      ("notmuch-search" "search")
                      ("notmuch-filter" "custom filter")
                      ("notmuch-filter-by-tag" "tag filter")
                      ("notmuch-unfilter-last" "unfilter")
                      ("notmuch-refresh" "refresh")
                      (("notmuch-back" "dismiss-buffer") "back")))))
    'keys '(("n" "notmuch-next") ("p" "notmuch-prev")
            ("RET" "notmuch-open-thread") ("v" "notmuch-preview")
            ("M-<" "notmuch-first-thread") ("M->" "notmuch-last-thread")
            ("r" "notmuch-reply") ("a" "notmuch-archive") ("d" "notmuch-trash")
            ("u" "notmuch-smart-untag") ("." "notmuch-toggle-unread")
            ("@" "notmuch-filter-by-sender")
            ("P" "notmuch-purge-sender")
            ("m" "notmuch-mark-toggle") ("M" "notmuch-mark-all")
            ("*" "notmuch-mark-all") ("U" "notmuch-unmark-all")
            ("F" "notmuch-filter-marked") ("A" "notmuch-archive-marked")
            ("D" "notmuch-trash-marked") ("t" "notmuch-autotag")
            ("C-c t" "notmuch-tag-marked")
            ("T" "notmuch-edit-tags") ("+" "notmuch-add-tag")
            ("-" "notmuch-remove-tag") ("0" "notmuch-remove-all-tags")
            ("L" "notmuch-like")
            ("j" "notmuch-jump")
            ("/" "notmuch-filter")
            ("\\" "notmuch-unfilter-last") ("l" "notmuch-filter-by-tag")
            ("s" "notmuch-search") ("g" "notmuch-refresh")
            ("C-c a" "notmuch-open-attachment")
            ("q" "notmuch-back"))
    'remap '(("next-line" "notmuch-next") ("previous-line" "notmuch-prev"))))

(define (nm--open-index! query)
  (let* ((buf *notmuch-search-buffer*)
         (cached? (and (buffer-exists? buf)
                       (pair? (buffer-local-value 'list-entries buf)))))
    (unless (buffer-exists? buf) (buffer-create buf))
    (nm--join-group! buf 'list)
    (nm--query-reset! buf query)
    (switch-to-buffer! buf)
    (set-mode! "notmuch-mode")
    ;; The mode setup can reuse cached rows. A mailbox selection requests
    ;; current rows for its new base query.
    (when cached? (nm--refresh! buf))
    (list-goto-first-entry buf)
    ;; a fresh index has shown no pane, dismissed nothing and followed no
    ;; row, so its first row previews even when it held the last one shown
    (set! *notmuch-pane-shown* #f)
    (set! *nm-dismissed* #f)
    (set! *nm-follow-id* #f)))

(define-command "notmuch-inbox" "Open the mail index on the default query"
  (lambda () (nm--open-index! notmuch-default-query)))

;;; --- mailboxes (notmuch-hello): saved searches with counts -----------------------

(define *notmuch-hello-buffer* "*mailboxes*")

;; query.NAME.query=Q entries from the notmuch config
(define (nm--saved-searches)
  (nm--op 'saved-searches))

(define (nm--parse-saved-searches config-list)
  (let loop ((ls (string-split config-list "\n")) (acc '()))
    (if (null? ls)
        (reverse acc)
        (let ((line (car ls)))
          (if (string-prefix? "query." line)
              (let* ((name (car (string-split
                                  (substring line 6 (string-length line)) ".")))
                     (parts (string-split line "="))
                     (q (if (pair? (cdr parts)) (string-join (cdr parts) "=") "")))
                (loop (cdr ls) (cons (list name q) acc)))
              (loop (cdr ls) acc))))))

(define (nm--tag-query tag)
  (string-append "tag:\"" (string-join (string-split tag "\"") "\"\"") "\""))

(define (nm--tag-search? query tag)
  (or (equal? query (nm--tag-query tag))
      (and (re-match? "^[A-Za-z0-9_.@+-]+$" tag)
           (equal? query (string-append "tag:" tag)))))

;; Tags belong to the selected database. A mailbox does not need a
;; separately configured saved search just to make its tag visible.
(define (nm--mailboxes)
  (let ((saved (nm--saved-searches)))
    (append saved
      (map (lambda (tag) (list tag (nm--tag-query tag)))
           (filter
             (lambda (tag)
               (null? (filter (lambda (s) (nm--tag-search? (cadr s) tag)) saved)))
             (nm--all-tags))))))



(define (nm--parse-counts output)
  (map string->number (string-split (string-trim output) "\n")))

;; Kept for callers that need a synchronous count outside the UI.
(define (nm--batch-count queries)
  (if (null? queries) '()
      (nm--parse-counts
        (shell-command->string (nm--batch-count-command queries)))))

;; Count pairs share one database open and SSH call. A thread expansion
;; in a saved search can take seconds even over a warm SSH connection.
;; Show selectable mailbox names first; counts must never hold navigation.
(define *nm-hello-request* 0)

(define (nm--hello-count-queries searches)
  (fold (lambda (acc s)
          (append acc (list (cadr s)
                            (string-append "( " (cadr s) " ) and tag:unread"))))
        '() searches))

(define (nm--hello-count-rows searches counts)
  (if (null? searches) '()
      (cons (list (car (car searches)) (cadr (car searches)) (car counts) (cadr counts))
            (nm--hello-count-rows (cdr searches) (cddr counts)))))

(define (nm--hello-rows buf)
  (let* ((searches (nm--mailboxes))
         (identity (nm--db-key))
         (request (+ *nm-hello-request* 1)))
    (set! *nm-hello-request* request)
    (desktop-skip! buf 'nm-hello-request)
    (desktop-skip! buf 'nm-hello-status)
    (buffer-set-local! buf 'nm-hello-request request)
    (buffer-set-local! buf 'nm-hello-status
      (if (null? searches) "" " · loading counts…"))
    (unless (null? searches)
      (nm--op 'count-batch (nm--hello-count-queries searches)
        (lambda (output)
          ;; An older refresh, another account, or a recreated buffer must
          ;; never receive these results. The callback never selects a window.
          (when (and (buffer-exists? buf)
                     (equal? identity (nm--db-key))
                     (equal? request (buffer-local-value 'nm-hello-request buf)))
            (let ((counts (nm--parse-counts output)))
              (if (and (= (length counts) (* 2 (length searches)))
                       (null? (filter (lambda (n) (not (and (number? n) (>= n 0)))) counts)))
                  (begin
                    (buffer-set-local! buf 'list-source-entries
                      (nm--hello-count-rows searches counts))
                    (buffer-set-local! buf 'nm-hello-status ""))
                  (buffer-set-local! buf 'nm-hello-status " · counts unavailable; g retries"))
              (list-redraw! buf))))))
    (map (lambda (s) (list (car s) (cadr s) #f #f)) searches)))

(define (nm--hello-count-label n)
  (if (number? n) (number->string n) "…"))

(define (nm--hello-unread? row)
  (let ((n (list-ref row 3))) (and (number? n) (> n 0))))

(define (nm--hello-cols row)
  (list (nm--fit (car row) 16)
        (string-append
          (string-pad-left (nm--hello-count-label (list-ref row 3)) 6) " / "
          (string-pad-left (nm--hello-count-label (list-ref row 2)) 6))))

(define (nm--hello-line row)
  (let ((c (nm--hello-cols row)))
    (string-append "  " (car c) (cadr c) "   " (cadr row))))

(define (nm--hello-overlays buf row off)
  (let* ((c (nm--hello-cols row))
         (n-start (+ off 2))
         (n-end (+ n-start (string-byte-length (car c))))
         (c-end (+ n-end (string-byte-length (cadr c))))
         (l-end (+ off (string-byte-length (nm--hello-line row)))))
    (list (list n-start n-end
                (if (nm--hello-unread? row) "nm-unread" "nm-author"))
          (list n-end c-end "nm-date")
          (list c-end l-end "nm-tags"))))

(define (nm--hello-cells buf row)
  (list (list (car row) (if (nm--hello-unread? row) "nm-unread" "nm-author"))
        (list (nm--hello-count-label (list-ref row 3)) "nm-date")
        (list (nm--hello-count-label (list-ref row 2)) "nm-date")
        (list (cadr row) "nm-tags")))

(define (nm--hello-at buf) (list-current buf))

(mode-icon! "notmuch-hello-mode" "")

(define (nm--mailbox-composml buf row)
  (list 'tag "mailbox" 'attrs (list (list "query" (cadr row)))
        'children (list (nm--field "mailbox-name" (car row) "primary")
                        (nm--field "unread-count" (nm--hello-count-label (list-ref row 3)) "count")
                        (nm--field "message-count" (nm--hello-count-label (list-ref row 2)) "count")
                        (nm--field "mail-query" (cadr row) "detail"))))

(define-list-mode! "notmuch-hello-mode"
  (list
    'doc (string-append
           "The account’s tags and saved searches with their unread and total "
           "counts. `RET` opens one as a thread list; `s` runs a free-form "
           "search. Counts load in the background; mailboxes open immediately.")
    'buffer *notmuch-hello-buffer*
    'collection "mailboxes"
    'composml (lambda (buf row) (nm--mailbox-composml buf row))
    'rows (lambda (buf) (nm--hello-rows buf))
    'key (lambda (buf row) (cadr row))
    'columns (lambda (buf)
               (list (list "mailbox" 16) (list "unread" 7 'right)
                     (list "total" 7 'right) (list "query" #f)))
    'cells (lambda (buf row) (nm--hello-cells buf row))
    'title (lambda (buf) (string-append (nm--account-label) " · Mailboxes"))
    'meta (lambda (buf)
            (string-append (number->string (length (list-entries buf)))
                           " mailboxes · " (nm--where-label)
                           (or (buffer-local-value 'nm-hello-status buf) "")))
    'total (lambda (buf) (length (list-source-entries buf)))
    'local-filter #t
    'footer (lambda (buf)
              '(("RET" "open") ("s" "search") ("/" "filter")
                ("h" "host") ("g" "refresh") ("q" "quit")))
    'keys '(("n" "next-line") ("p" "previous-line")
            ("RET" "notmuch-hello-open") ("g" "notmuch-hello-refresh")
            ("j" "notmuch-jump") ("s" "notmuch-search")
            ("h" "notmuch-switch-host")
            ("q" "quit-window"))))

(define (nm--hello-wake!)
  ;; Pending callbacks and their status do not survive desktop restore.
  ;; Cached mailbox rows still need fresh counts after the mode wakes.
  (let ((buf (current-buffer)))
    (unless (buffer-local-value 'nm-hello-status buf)
      (list-refresh! buf))))

(add-hook! 'notmuch-hello-mode-hook 'nm--hello-wake!)

(define-command "notmuch" "Open the mailboxes (saved searches)"
  (lambda ()
    (let ((buf *notmuch-hello-buffer*))
      (unless (buffer-exists? buf) (buffer-create buf))
      (nm--join-group! buf 'home)
      (switch-to-buffer! buf)
      (set-mode! "notmuch-hello-mode")
      (list-goto-first-entry buf))))

(define-command "notmuch-hello-open" "Open the saved search at point"
  (lambda ()
    (let ((s (nm--hello-at (current-buffer))))
      (if s
          (nm--open-index! (cadr s))
          (message "No mailbox on this line"))))
  'modes '(notmuch-hello-mode))

(define-command "notmuch-hello-refresh" "Refresh the mailbox counts"
  (lambda () (list-refresh! (current-buffer)) (message "Refreshed"))
  'modes '(notmuch-hello-mode))

;;; A thread id belongs to one database, so a host change makes every cached
;;; view stale. Drop the rows and read again rather than keep ids that no
;;; longer resolve.
(define (nm--host-changed! buf)
  (set! *nm-db-paths* '())
  (for-each (lambda (b)
              (when (buffer-exists? b) (buffer-set-local! b 'list-entries '())))
            (list *notmuch-search-buffer* *notmuch-hello-buffer*))
  (when (buffer-exists? *notmuch-search-buffer*)
    (nm--query-reset! *notmuch-search-buffer* notmuch-default-query))
  (list-refresh! buf)
  (message (string-append "Mail on " (nm--source-label))))

;; One act picks both halves. A command that set the host alone could leave
;; the profile unset, and the mailbox unusable.
(define-command "notmuch-switch-host" "Read mail from another mailbox"
  (lambda ()
    (let ((buf (current-buffer)))
      (minibuffer-read "Mailbox: "
        (map (lambda (m) (nm--mailbox-label (car m) (cadr m))) (map nm--entry->pair notmuch-mailboxes))
        (lambda (choice)
          (let* ((typed (string-trim choice))
                 (hit (filter (lambda (m) (equal? (nm--mailbox-label (car m) (cadr m)) typed))
                              (map nm--entry->pair notmuch-mailboxes)))
                 (host (if (null? hit)
                           (if (equal? typed "local") "" typed)
                           (car (car hit))))
                 (profile (if (null? hit) "" (cadr (car hit)))))
            (customize-save! 'notmuch-host host)
            (customize-save! 'notmuch-profile profile)
            (nm--host-changed! buf))))))
  'modes '(notmuch-hello-mode))

;; the mail views are derived state — killing them loses nothing. The
;; chat survives (it holds a conversation); a scene toggle in init.scm
;; can lean on this for teardown.
(define (nm--view-buffers)
  (append (filter buffer-exists? (list *notmuch-search-buffer* *notmuch-hello-buffer*))
          (nm--show-buffers)))

(define-command "notmuch-quit" "Close mail: kill the view buffers, back to work"
  (lambda ()
    ;; land on real work: any buffer that is not one of the mail app's own.
    ;; The app id names them wherever they are, so no group has to, and mail
    ;; now shares the group of whatever you were doing when you opened it.
    ;; Work still on screen beside the mail comes first, then the most recent.
    (let* ((work? (lambda (b)
                    (and (not (member b (nm--view-buffers)))
                         (not (equal? (app-id b) *nm-app-id*)))))
           (shown (filter work? (map cadr (window-list))))
           (others (filter work? (buffer-list-mru))))
      (delete-other-windows!)
      (switch-to-buffer! (cond ((pair? shown) (car shown))
                               ((pair? others) (car others))
                               (else "*scratch*")))
      (for-each buffer-kill! (nm--view-buffers))
      (message "Mail closed"))))

;; the preview helpers target the *next* window in cyclic order, so any
;; window arrangement works: put the index left of where you want mail
;; shown and SPC/n/p keep filling that pane. Per-account profile commands
;; and keybindings stay personal; the arrangement below does not.

;;; --- mail as an app: index | thread | chat ------------------------------------
;;; Mail opens in the group you are in, as every app does: no group of its
;;; own and no jump. Its buffers join that group. The index opens in the
;;; window you are in and the page of the thread at point in the pane
;;; beside it; M-x app-layout gives each role its pane when you want it.

(define (nm--app-showing?)
  (and (window-showing *notmuch-search-buffer*) #t))

(define (nm--open-app!)
  (if (buffer-exists? *notmuch-search-buffer*)
      (nm--join-group! *notmuch-search-buffer* 'list)
      (nm--open-index! notmuch-default-query))
  (let ((page (nm--show-buffer)))
    (when page (nm--join-group! page 'detail))
    (app-open! *notmuch-search-buffer* (and page (buffer-exists? page) page))))

(define-command "mail" "Toggle mail in the group you are in: index, thread, chat"
  (lambda ()
    (if (nm--app-showing?)
        (run-command "notmuch-quit")
        (nm--open-app!))))
(catalog-meta! 'command "mail" 'domain 'mail
               'effects '(write display external execute))

;; Mail on one account, opened where you are: the account's inbox, laid out
;; as the app. Personal commands (mail-recruiting, ...) are this one call.
(define (mail-open! &optional profile)
  (let* ((buf *notmuch-search-buffer*)
         (existed? (buffer-exists? buf)))
    (when profile
      (let ((box (filter (lambda (m) (or (equal? (cadr m) profile) (equal? (car m) profile)))
                         (map nm--entry->pair notmuch-mailboxes))))
        (if (null? box)
            (set! notmuch-profile profile)
            (begin (set! notmuch-host (car (car box)))
                   (set! notmuch-profile (cadr (car box))))))
      (set! *nm-db-paths* '())
      (for-each (lambda (b)
                  (when (buffer-exists? b)
                    (buffer-set-local! b 'list-entries '())
                    (buffer-set-local! b 'list-source-entries '())))
                (list buf *notmuch-hello-buffer*)))
    (nm--open-app!)
    (when existed?
      (nm--query-reset! buf notmuch-default-query)
      (nm--refresh! buf)
      (list-goto-first-entry buf))
    buf))
(public! 'mail-open! "(mail-open! [PROFILE]) — open mail in the group you are in, on PROFILE's inbox when given")

;;; --- preview: thread in the other window, focus stays --------------------------

;; The thread opens as the index's detail (packages/detail.scm): beside the
;; list, in the pane its last thread took, and it selects nothing, so focus
;; and point stay put.
(define (nm--show-pane! buf &optional index)
  (display-buffer-detail! buf (or index (current-buffer))))

;; Every thread has its own page. M-RET keeps the one on screen: it leaves
;; the list, so q on the list no longer takes it along.
(detail-name!
  "notmuch-show-mode"
  (lambda (buf)
    (let ((subject (or (buffer-local-value 'notmuch-subject buf) "")))
      (if (equal? subject "")
          buf
          (string-append "*mail: "
            (if (> (string-length subject) 60)
                (string-append (substring subject 0 60) "...")
                subject)
            "*")))))

;; #t once a thread has been shown for the live index. A pane that was
;; shown and is gone was dismissed, and a dismissal stays dismissed.
(define *notmuch-pane-shown* #f)

(define (nm--preview! buf)
  (let ((th (nm--thread-at buf)))
    (when th
      ;; The index re-asserts its own window only when it IS the selected one,
      ;; because opening a thread can otherwise take that window. Called from
      ;; anywhere else — a purge run from the mail view — the pane is filled
      ;; and focus is left where the user put it.
      (let* ((origin (active-window))
             (from-index? (equal? (window-buffer origin) buf))
             (mail (nm--open-thread! (nm--th-id th) (nm--th-subject th) 'defer-read)))
        (when from-index?
          (window-set-buffer! origin buf)
          (select-window! origin))
        (nm--show-pane! mail buf)
        (set! *notmuch-pane-shown* #t))
      ;; Keep the list focused before a database write can fail.
      (nm--op 'tag! "-unread" (string-append "thread:" (nm--th-id th)))
      ;; opening marked it read — show that in the index right away, except
      ;; in a search for unread mail: a refresh there would drop the row you
      ;; are reading. It leaves on the next refresh (g or a tab).
      (when (and (member "unread" (nm--th-tags th))
                 (not (string-contains? (nm--query-of buf) "tag:unread")))
        (nm--refresh! buf)))))

(define-command "notmuch-preview" "Preview the thread at point in the other window"
  (lambda () (nm--preview! (current-buffer)))
  'modes '(notmuch-mode))

;; The index's current thread as its page, read by name: it never consults
;; the current buffer, point or the windows. Answers the page, or #f while
;; the index has no rows yet; the first move fills the pane then.
(define-command "notmuch-show-current"
  "Open the page of the index's current thread, if there is one"
  (lambda ()
    (let ((th (and (buffer-exists? *notmuch-search-buffer*)
                   (with-current-buffer *notmuch-search-buffer*
                     (lambda () (nm--thread-at *notmuch-search-buffer*))))))
      (and th (nm--open-thread! (nm--th-id th) (nm--th-subject th))))))

;; The row the highlight RESTS on is the one worth fetching. A move
;; schedules the fetch and the next move cancels it, so holding n costs
;; one round trip instead of one per row.
(define (nm--preview-now! buf)
  (when (and (buffer-exists? buf) (nm--mail-window? buf))
    (nm--preview! buf)))

(define (nm--preview-soon! buf)
  (if (and (number? notmuch-preview-delay) (> notmuch-preview-delay 0))
      (debounce! "notmuch-preview" notmuch-preview-delay nm--preview-now! buf)
      (nm--preview! buf)))

(define (nm--maybe-preview! buf)
  (when notmuch-auto-preview (nm--preview-soon! buf)))

;; #t when the mail pane already holds the thread at point. A focus change
;; fires the configuration hook often, and a preview is an ssh round trip,
;; so only a pane that disagrees with point is worth refilling.
(define (nm--pane-at-point? buf)
  (let* ((th (nm--thread-at buf))
         (pane (and th (nm--thread-buffer (nm--th-id th)))))
    (or (not th)
        (and pane (window-showing pane) #t))))

;; ONE rule decides what the mail pane shows, and no verb states it.
;; The pane holds the thread the list has at point; whenever it does not,
;; it is refilled. Moving the highlight, archiving the row out from under
;; it, purging a sender, popping a filter, landing back on the index from
;; another buffer — all of them are the same event, and none of them has
;; to say so.
;;
;; It runs after every command in every buffer, so the cheap tests come
;; first: no index, or a window that is not the mail work, and it is two
;; calls and out. The fetch itself is an ssh round trip, so a pane that
;; already agrees with point costs nothing.
;; The one write the rule must not answer: marking a thread unread. A
;; fetch reads the thread, and reading it clears unread again. The verb
;; says so once, here, and the next pass consumes it.
(define *nm-skip-follow* #f)
;; the thread at point the last time the pane followed it
(define *nm-follow-id* #f)
(define (nm--skip-follow!) (set! *nm-skip-follow* #t))

;; the thread the reader dismissed the pane on, or #f
(define *nm-dismissed* #f)

(define (nm--follow-point!)
  (let ((buf *notmuch-search-buffer*))
    (cond
      ;; one verb asked to be left alone this pass
      (*nm-skip-follow* (set! *nm-skip-follow* #f))
      ((not (and notmuch-auto-preview
                 (buffer-exists? buf)
                 (nm--mail-window? buf)
                 (not (minibuffer-active?))))
       #f)
      ;; q closed the pane on a thread: while point stays on it nothing
      ;; reopens, and a move to any other thread brings the pane back.
      (*nm-dismissed*
       (unless (equal? *nm-dismissed* (nm--id-at buf))
         (set! *nm-dismissed* #f)
         (nm--preview-soon! buf)))
      ;; the pane went some other way than q: that is a dismissal too
      ((and *notmuch-pane-shown* (not (find window-showing (nm--show-buffers))))
       (set! *nm-dismissed* (nm--id-at buf)))
      ;; the pane follows point, not the other way round: only a move to
      ;; another thread refills it. A pane the user walked to another mail
      ;; (s-[ / s-], C-x b) while point stays put keeps that mail.
      (else
       (let ((id (nm--id-at buf)))
         (cond ((nm--pane-at-point? buf) (set! *nm-follow-id* id))
               ((equal? id *nm-follow-id*) #f)
               (else (set! *nm-follow-id* id)
                     (nm--preview-soon! buf))))))))

(define (nm--id-at buf)
  (let ((th (nm--thread-at buf))) (and th (nm--th-id th))))

;; A command run from the mail view — purge this sender, archive and back —
;; changes the list too, so the pane follows from either window.
(define (nm--mail-window? buf)
  (let ((w (window-buffer (active-window))))
    (or (equal? w buf) (nm--show-buffer? w))))

(add-hook! 'post-command-hook 'nm--follow-point!)
(add-hook! 'window-configuration-change-hook 'nm--follow-point!)

;; the shown mail follows the highlight: every move previews, and opening
;; a thread marks it read (the open itself tags -unread)
;; a thread is a ROW, and a row is two lines — the list moves by rows,
;; so every move here goes through the list and none of them counts
;; lines
(define (nm--move! step)
  (list-move-in! (current-buffer) step))

(define-command "notmuch-next" "Move down; the shown mail follows"
  (lambda () (nm--move! 1))
  'modes '(notmuch-mode))

(define-command "notmuch-prev" "Move up; the shown mail follows"
  (lambda () (nm--move! -1))
  'modes '(notmuch-mode))

(define-command "notmuch-first-thread" "Jump to the newest thread"
  (lambda () (list-goto-first-entry (current-buffer)))
  'modes '(notmuch-mode))

(define-command "notmuch-last-thread" "Jump to the oldest listed thread"
  (lambda ()
    (let* ((buf (current-buffer)) (n (length (list-entries buf))))
      (when (> n 0) (list-goto-index! buf (- n 1)))))
  'modes '(notmuch-mode))

(define-command "notmuch-refresh" "Re-run the search and refresh the listing"
  (lambda ()
    (let ((buf (current-buffer)))
      (nm--after-change! buf)
      (message "Refreshed")))
  'modes '(notmuch-mode))

(define-command "notmuch-search" "Prompt for a notmuch query and show it"
  (lambda ()
    (let ((buf (current-buffer)))
      (minibuffer-read "Notmuch search: " '()
        (lambda (q)
          (nm--query-reset! buf q)
          (nm--refresh! buf)
          (list-goto-first-entry buf))))))

;;; --- tagging ------------------------------------------------------------------

(define (nm--goto-index! buf i) (list-goto-index! buf i))

;; A position is an index AND the thread ids from that row down. An index
;; alone lies the moment the list under it changes: filter to a sender,
;; trash every mail they ever sent, come back, and row 12 is a stranger.
;; The ids say which thread the reader was on, and which ones follow it
;; when that thread is one of the ones that went.
(define (nm--position-of buf)
  (let ((i (or (nm--index-at buf) 0))
        (ids (map nm--th-id (list-entries buf))))
    (cons i (nm--tail-from ids i))))

;; list-tail throws past the end; a row count that shrank under us must not
(define (nm--tail-from lst i)
  (cond ((null? lst) '())
        ((<= i 0) lst)
        (else (nm--tail-from (cdr lst) (- i 1)))))

;; the first remembered thread the list still has, else the same index
(define (nm--goto-position! buf pos)
  (let ((n (length (list-entries buf))))
    (when (> n 0)
      (let* ((ids (map nm--th-id (list-entries buf)))
             (i (if (pair? pos) (car pos) (or pos 0)))
             (wanted (if (pair? pos) (cdr pos) '()))
             (hit (let loop ((w wanted))
                    (cond ((null? w) #f)
                          ((member (car w) ids) (nm--index-of ids (car w)))
                          (else (loop (cdr w)))))))
        (nm--goto-index! buf (or hit (min i (- n 1))))))))

(define (nm--index-of lst x)
  (let loop ((l lst) (i 0))
    (cond ((null? l) #f)
          ((equal? (car l) x) i)
          (else (loop (cdr l) (+ i 1))))))

;; Any command that changes the listing ends here: refresh, then land on
;; the thread the reader was on, or on the first one still there below it.
;; What the mail pane shows is not this function's business — nm--follow-point!
;; settles that after every command.
(define (nm--after-change! buf &optional pos)
  (when (buffer-exists? buf)
    (let ((p (or pos (nm--position-of buf))))
      (nm--refresh! buf)
      (nm--goto-position! buf p))))

(define (nm--tag! buf changes)
  (let ((th (nm--thread-at buf)) (pos (nm--position-of buf)))
    (if th
        (begin
          (nm--op 'tag! changes (string-append "thread:" (nm--th-id th)))
          (nm--after-change! buf pos)
          (message
            (if (and (equal? changes "-inbox")
                     (member (nm--th-id th) (map nm--th-id (list-entries buf))))
                "Archive: this search also includes mail outside the inbox"
                changes)))
        (message "No thread on this line"))))

;; A mark is a standing instruction, so the verb obeys the marks while
;; there are any and the row under point when there are none. The reader
;; learns one key for archiving, and the footer says which set it means.
(define-command "notmuch-archive" "Archive the marked threads, or the thread at point (-inbox)"
  (lambda ()
    (let ((buf (current-buffer)))
      (if (nm--any-marked? buf)
          (nm--confirm-marked buf "Archive" "-inbox")
          (nm--tag! buf "-inbox"))))
  'modes '(notmuch-mode))
(define-command "notmuch-trash" "Trash the marked threads, or the thread at point (+trash -inbox -unread)"
  (lambda ()
    (let ((buf (current-buffer)))
      (if (nm--any-marked? buf)
          (nm--confirm-marked buf "Trash" "+trash -inbox -unread")
          (nm--tag! buf "+trash -inbox -unread"))))
  'modes '(notmuch-mode))
(catalog-meta! 'command "notmuch-trash" 'domain 'mail 'effects '(destroy))

(define-command "notmuch-toggle-unread" "Toggle the unread tag on the thread at point"
  (lambda ()
    (let* ((buf (current-buffer)) (th (nm--thread-at buf)))
      (if th
          ;; Reading a preview clears unread. An explicit toggle must
          ;; keep its new state instead of immediately reading it again.
          (begin
            (nm--skip-follow!)
            (nm--tag! buf (if (member "unread" (nm--th-tags th)) "-unread" "+unread")))
          (message "No thread on this line"))))
  'modes '(notmuch-mode))

;; on a plain tag:X search, u strips that tag from the thread — inbox zero
;; as a single keystroke on any tag view
(define-command "notmuch-smart-untag" "Remove the searched-for tag from this thread"
  (lambda ()
    (let* ((buf (current-buffer))
           (query (nm--query-of buf))
           (th (nm--thread-at buf)))
      (cond ((not th) (message "No thread on this line"))
            ((and (string-prefix? "tag:" query)
                  (not (string-contains? query " ")))
             (nm--tag! buf (string-append "-" (substring query 4 (string-length query)))))
            (else (message "Not a simple tag: search")))))
  'modes '(notmuch-mode))

(define-command "notmuch-edit-tags" "Edit tags of the thread at point (+tag -tag ...)"
  (lambda ()
    (let ((buf (current-buffer)))
      (if (nm--thread-at buf)
          (minibuffer-read "Tags (+add -remove): " '()
            (lambda (changes) (nm--tag! buf changes)))
          (message "No thread on this line"))))
  'modes '(notmuch-mode))

;; every tag in the database — the completion source for +
(define (nm--all-tags)
  (filter (lambda (t) (not (equal? t "")))
          (nm--op 'tags #f)))

;; Only offer tags that occur in the current result. The menu cannot lead
;; from a useful inbox view to an empty result through an unrelated tag.
(define (nm--query-tags buf)
  (filter (lambda (t) (not (equal? t "")))
          (nm--op 'tags (nm--query-of buf))))

(define-command "notmuch-add-tag" "Add a tag to marked threads, or the thread at point (completes)"
  (lambda ()
    (let ((buf (current-buffer)))
      (if (or (nm--any-marked? buf) (nm--thread-at buf))
          (minibuffer-read "Add tag: " (nm--all-tags)
            (lambda (tag)
              (unless (equal? (string-trim tag) "")
                (if (nm--any-marked? buf)
                    (nm--tag-marked! buf (sh-quote (string-append "+" (string-trim tag))))
                    (nm--tag! buf (sh-quote (string-append "+" (string-trim tag))))))))
          (message "No thread on this line"))))
  'modes '(notmuch-mode))

(catalog-meta! 'command "notmuch-add-tag" 'domain 'mail 'effects '(write external execute))

(define-command "notmuch-remove-tag" "Remove a tag from marked threads, or the thread at point (completes)"
  (lambda ()
    (let* ((buf (current-buffer)) (th (nm--thread-at buf))
           (marked? (nm--any-marked? buf)))
      (if (or marked? th)
          (minibuffer-read "Remove tag: "
            (if marked? (nm--marked-tags buf) (nm--th-tags th))
            (lambda (tag)
              (unless (equal? (string-trim tag) "")
                (if (nm--any-marked? buf)
                    (nm--tag-marked! buf (sh-quote (string-append "-" (string-trim tag))))
                    (nm--tag! buf (sh-quote (string-append "-" (string-trim tag))))))))
          (message "No thread on this line"))))
  'modes '(notmuch-mode))
(catalog-meta! 'command "notmuch-remove-tag" 'domain 'mail 'effects '(write external execute))
;; Archive drops one tag and trash swaps three. This is the blunt one:
;; after it the thread answers to no tag: search at all, so it names the
;; tags it is about to take and asks first.
;; Archive drops one tag and trash swaps three. This is the blunt one:
;; notmuch's own --remove-all takes them all, whatever they are. (`-*` is
;; the tag-file spelling of that wildcard; on the command line it reads as
;; a literal tag named * and quietly does nothing.) After it the thread
;; answers to no tag: search at all, so it asks first.
(define-command "notmuch-remove-all-tags" "Remove every tag from the marked threads, or the thread at point"
  (lambda ()
    (let* ((buf (current-buffer)) (th (nm--thread-at buf))
           (marked? (nm--any-marked? buf)))
      (if (not (or marked? th))
          (message "No thread on this line")
          (minibuffer-read
            (if marked?
                "Remove all tags from the marked threads? "
                (string-append "Remove all tags (" (string-join (nm--th-tags th) " ") ")? "))
            (list "yes" "no")
            (lambda (ans)
              (if (equal? ans "yes")
                  (if marked?
                      (nm--tag-marked! buf "--remove-all")
                      (nm--tag! buf "--remove-all"))
                  (message "Cancelled")))))))
  'modes '(notmuch-mode))
(catalog-meta! 'command "notmuch-remove-all-tags" 'domain 'mail 'effects '(destroy))
;; Autotag: the mailbox's own tag list is the label set, sent to JEV as the
;; options of one choice question. JEV reads the thread and picks exactly one of
;; them, so it cannot invent a folder. The state tags (inbox, unread, replied)
;; are not on offer, and nothing is ever removed, so a classification can only
;; add a tag this mailbox already uses.
(defcustom 'notmuch-autotag-exclude
  '("inbox" "unread" "attachment" "signed" "encrypted" "draft" "sent"
    "replied" "trash" "flagged" "compos-mark")
  "Tags notmuch-autotag never offers. These say delivery state, not subject.")

;; How much of a thread's text notmuch-autotag sends to the model.
(define notmuch-autotag-limit 6000)
;; A tag name alone is a thin description, and a thin description is what makes
;; a classifier miss. These say what the tag covers. A tag with no entry here is
;; described by its own name.
;; Per-tag descriptions notmuch-autotag sends JEV. A tag with no entry is described by its own name.
(define notmuch-autotag-hints '("banking" "Anything from a bank, card issuer, or payment network: account and card statements, transaction and card alerts, balance and credit notices, loan and EMI notices, cheque and transfer confirmations, KYC and account-service mail. Mail from a bank belongs here even when it also looks like a bill, a receipt, or marketing. A broker, depository, demat provider, or fund house is not a bank."
    "newsletter" "A recurring bulk mailing the reader subscribed to and that anyone on the list receives unchanged: a newsletter, digest, roundup, briefing, or new-post notification from a blog or publication. The giveaway is an unsubscribe link plus editorial content addressed to a list, not to this reader. A one-off promotion, an order receipt, or a service notice is not a newsletter."
    "bills" "A bill or payment request from an ordinary vendor: a utility, a landlord, a telco, a subscription. Not a bank's own statement or fee, and not a broker's account or maintenance charge."
    "invoice" "An invoice the reader issued or received for work or goods, with line items and an amount."
    "expenses" "A receipt or expense record for something already paid."
    "investments" "Anything from a broker, depository, demat provider, exchange, fund house, or wealth platform: holdings and portfolio statements, contract notes, trade confirmations, dividends, capital-gains statements, and that provider's own account and maintenance charges. Mail from a broker or a depository belongs here and not under banking, even when it reads as a bank alert or a bill."
    "taxes" "Tax filings, tax notices, and tax authority correspondence."
    "gov" "Mail from a government body or public authority: visas, licences, registrations, official notices."
    "spam" "Unsolicited bulk mail from a sender the reader has no relationship with."))
;; These three are not subjects, so they are not options in the one choice: a
;; thread can be about investments AND be phishing. Each is its own yes/no
;; question, and the answer is a probability, so the threshold lives here too.
(defcustom 'notmuch-autotag-flags
  '("important" "Does this thread need this reader to act, decide, reply, or pay, or does it carry a deadline, a sum of money, or a consequence that matters to them personally? A bulk mailing that merely sounds urgent is not important."
    "spam" "Is this unsolicited bulk mail from a sender this reader never gave their address to and has no relationship with?"
    "phishing" "Does this thread pretend to be a person or an organisation it is not, so it can take credentials, money, card or account details, or personal data? Read the From line before you read anything else. When the display name claims a brand or a person and the sender domain has nothing to do with that brand, the mail is a fake and the answer is yes, however ordinary and however correct the rest of it looks. The brand's own domain, or a subdomain of it, is not a mismatch, so mail that really comes from the brand is no even when it asks the reader to verify something or to act at once. The other signs of a fake are a link that does not go where its text says and an attachment that asks the reader to sign in."
    "reply-due" "The most recent message in this thread, by its Date header, comes from somebody other than this reader, and it waits on an answer from them: a question, a request, a reminder, or a decision that is theirs to make. Is that true of this thread? It is not true when this reader sent the most recent message, when the matter is already settled, and when the mail is a bulk mailing, a notification, a receipt, or comes from an address that takes no reply.")
  "Yes/no tags notmuch-autotag asks about one at a time, as tag then question.")

;; How the mail reached the reader, asked apart from what it is about. The two
;; axes are independent: one bank sends this reader a card alert made for them
;; alone and a rate circular sent to every customer, and both are "banking".
;; The tells live in the headers, which is why autotag sends those too.
(defcustom 'notmuch-autotag-kinds
  '("transactional" "This message was made for this reader alone and would not exist without them: something they did, bought, owe, hold, or booked set it off. Their own address stands in To, the text names their account, card, order, booking, ticket, or case, and nobody else got this same message. A card or account alert, an order or payment confirmation, a password reset, a bill for this account, a delivery notice, and a reply in a conversation are all transactional."
    "broadcast" "This message went out unchanged to many readers at once. The headers say so before the words do: a List-Id, a List-Unsubscribe, a List-Post, a Precedence of bulk or list, an Auto-Submitted, a campaign or feedback id, a From nobody answers, or a To that is a list address, undisclosed-recipients, or not this reader at all. A newsletter, a promotion, an announcement, a product update, a policy notice, and a circular a bank sends all its customers are all broadcast, however warmly they greet the reader by name.")
  "The one-of delivery kinds notmuch-autotag picks between, as tag then criterion.")

(define nm--autotag-kind-instructions
  (string-append
    "Decide how this thread reached the reader. The raw headers stand above "
    "the body: who the envelope names, and which list, bulk, or campaign "
    "headers are present, settle this, not the tone of the writing and not "
    "the subject of it. A greeting by name proves nothing, because a bulk "
    "mailing fills the name in. Pick none when the headers and the body do "
    "not settle it."))

(defcustom 'notmuch-autotag-threshold 0.75
  "How sure JEV must be before notmuch-autotag applies a yes/no tag.")


(define (nm--autotag-pair-tags plist)
  (let loop ((xs plist) (out '()))
    (if (or (null? xs) (null? (cdr xs)))
        (reverse out)
        (loop (cdr (cdr xs)) (cons (car xs) out)))))

(define (nm--autotag-flag-tags)
  (nm--autotag-pair-tags notmuch-autotag-flags))

(define (nm--autotag-kind-tags)
  (nm--autotag-pair-tags notmuch-autotag-kinds))


(define (nm--autotag-vocabulary)
  ;; Neither a flag tag nor a kind tag is ever a subject option: each has its
  ;; own question, and offering one twice would make the subject pick fight
  ;; with the answer that already covers it.
  (let ((own (append (nm--autotag-flag-tags) (nm--autotag-kind-tags))))
    (filter (lambda (t) (and (not (member t notmuch-autotag-exclude))
                             (not (member t own))))
            (nm--all-tags))))

(define (nm--thread-tags id)
  (filter (lambda (t) (not (equal? t "")))
    (nm--op 'tags (string-append "thread:" id))))

(define (nm--uniq lst)
  (let loop ((l lst) (acc '()))
    (cond ((null? l) (reverse acc))
          ((member (car l) acc) (loop (cdr l) acc))
          (else (loop (cdr l) (cons (car l) acc))))))

(define nm--autotag-instructions
  (string-append
    "Pick the one tag that says what this email thread is about. "
    "The raw headers stand above the body. Read the sender before the "
    "wording: who sent it decides more than how it reads, so decide what "
    "kind of organisation sent this before you weigh a single word in it. "
    "A tag fits only when it is true of the whole thread, "
    "not of one sentence in it. Pick none when no tag on the list tells the "
    "truth."))
(define (nm--autotag-hint tag)
  (let loop ((xs notmuch-autotag-hints))
    (cond ((or (null? xs) (null? (cdr xs))) tag)
          ((equal? (car xs) tag) (car (cdr xs)))
          (else (loop (cdr (cdr xs)))))))

;; The options are t0, t1, ... and not the tags themselves: a tag is free text
;; and carries spaces and case, and an option key has to survive as a symbol.
(define (nm--autotag-option i)
  (string->symbol (string-append "t" (number->string i))))

(define (nm--autotag-criteria vocab)
  (let loop ((xs vocab) (i 0) (out '()))
    (if (null? xs)
        (append out (list 'none "No tag on this list is true of this thread."))
        (loop (cdr xs) (+ i 1)
              (append out (list (nm--autotag-option i)
                                (string-append (car xs) " - "
                                               (nm--autotag-hint (car xs)))))))))

(define (nm--autotag-tag-of vocab chosen)
  (let loop ((xs vocab) (i 0))
    (cond ((null? xs) #f)
          ((equal? chosen (symbol->string (nm--autotag-option i))) (car xs))
          (else (loop (cdr xs) (+ i 1))))))







(define (nm--autotag-thread-id line)
  (if (and (>= (string-length line) 7) (equal? (substring line 0 7) "thread:"))
      (substring line 7 (string-length line))
      line))

(defcustom 'notmuch-autotag-only-unprocessed #t
  "When true, notmuch-autotag and notmuch-autotag-new skip a thread that already
carries a tag outside notmuch-autotag-exclude. C-u notmuch-autotag takes them all.")

(define (nm--autotag-unprocessed-clause)
  ;; " and not thread:\"{tag:a or ...}\"" over every tag that says what a mail is,
  ;; so a thread counts as processed when any of its messages carries one.
  ;; Thread-level, since a bare `not tag:x` matches a thread with one untagged
  ;; message. Blocks on the mail host, as nm--all-tags does.
  (let ((done (filter (lambda (t) (not (member t notmuch-autotag-exclude)))
                      (nm--all-tags))))
    (if (null? done)
        ""
        ;; The quoted form takes the spaces; a quote inside it doubles.
        (string-append " and not thread:\"{"
          (string-join (map (lambda (t) (string-append "tag:\"\"" t "\"\"")) done) " or ")
          "}\""))))

(define (nm--autotag-targets buf &optional all)
  ;; ALL takes processed threads too; otherwise notmuch-autotag-only-unprocessed
  ;; narrows the marks or the thread at point to the unprocessed ones.
  (let* ((th (and (not (nm--any-marked? buf)) (nm--thread-at buf)))
         (base (cond ((nm--any-marked? buf) (nm--marked-query buf))
                     (th (string-append "thread:" (nm--th-id th)))
                     (else #f))))
    (cond ((not base) '())
          ((and th (or all (not notmuch-autotag-only-unprocessed))) (list (nm--th-id th)))
          (else
           (map nm--autotag-thread-id
                (nm--op 'threads (string-append "(" base ")"
                                   (if (or all (not notmuch-autotag-only-unprocessed))
                                       ""
                                       (nm--autotag-unprocessed-clause)))))))))

(define (nm--autotag-flag-key i)
  (string->symbol (string-append "f" (number->string i))))

(define (nm--autotag-kind-option i)
  (string->symbol (string-append "k" (number->string i))))

(define (nm--autotag-kind-criteria)
  (let loop ((xs notmuch-autotag-kinds) (i 0) (out '()))
    (if (or (null? xs) (null? (cdr xs)))
        (append out (list 'none "Neither kind is true of this thread."))
        (loop (cdr (cdr xs)) (+ i 1)
              (append out (list (nm--autotag-kind-option i)
                                (string-append (car xs) " - " (car (cdr xs)))))))))

(define (nm--autotag-kind-of chosen)
  (let loop ((xs (nm--autotag-kind-tags)) (i 0))
    (cond ((null? xs) #f)
          ((equal? chosen (symbol->string (nm--autotag-kind-option i))) (car xs))
          (else (loop (cdr xs) (+ i 1))))))

(define (nm--autotag-questions vocab)
  ;; One choice for the subject, one for how it was delivered, then one yes/no
  ;; per flag. The keys are t0, k0, f0, ... for the same reason throughout: a
  ;; tag is free text and an option key has to survive as a symbol.
  (let loop ((xs notmuch-autotag-flags) (i 0)
             (out (list 'tag (jev-choice nm--autotag-instructions
                                         (nm--autotag-criteria vocab))
                        'kind (jev-choice nm--autotag-kind-instructions
                                          (nm--autotag-kind-criteria)))))
    (if (or (null? xs) (null? (cdr xs)))
        out
        (loop (cdr (cdr xs)) (+ i 1)
              (append out (list (nm--autotag-flag-key i)
                                (jev-noul (car (cdr xs)))))))))

;; Per-tag override of notmuch-autotag-threshold, as tag then number. A tag with
;; no entry answers to the general threshold. A missed phishing mail costs more
;; than a wrong tag, so phishing sits lower than the rest.
(define notmuch-autotag-thresholds '("phishing" 0.5))

(define (nm--autotag-threshold-for tag)
  "How sure JEV must be before TAG is applied."
  (let loop ((xs notmuch-autotag-thresholds))
    (cond ((or (null? xs) (null? (cdr xs))) notmuch-autotag-threshold)
          ((equal? (car xs) tag) (cadr xs))
          (else (loop (cdr (cdr xs)))))))

(define (nm--autotag-flags-said-yes reply)
  (let loop ((xs (nm--autotag-flag-tags)) (i 0) (out '()))
    (if (null? xs)
        (reverse out)
        (let ((p (jev-answer-noul reply (nm--autotag-flag-key i))))
          (loop (cdr xs) (+ i 1)
                (if (and p (>= p (nm--autotag-threshold-for (car xs))))
                    (cons (car xs) out)
                    out))))))

(defcustom 'notmuch-autotag-archive
  '()
  "Tags that archive a thread when autotag gives them, per mailbox, as
(PROFILE TAG ...). A thread autotag tags so leaves the inbox.")

(define (nm--autotag-archive-tags)
  (let ((hit (assoc notmuch-profile notmuch-autotag-archive)))
    (if hit (cdr hit) '())))

(define (nm--autotag-apply! id want)
  ;; WANT is every tag this thread should end up carrying besides the system
  ;; ones. Everything else goes, so a second run corrects an old answer instead
  ;; of piling a new tag on top of it. Answers the ops it ran, already signed.
  ;; A tag this mailbox archives on also takes the thread out of the inbox.
  (let* ((have (nm--thread-tags id))
         (add (filter (lambda (t) (not (member t have))) want))
         (drop (filter (lambda (t) (and (not (member t notmuch-autotag-exclude))
                                        (not (member t want))))
                       have))
         (archive? (and (member "inbox" have)
                        (pair? (filter (lambda (t) (member t (nm--autotag-archive-tags))) want))))
         (ops (append (map (lambda (t) (string-append "+" t)) add)
                      (map (lambda (t) (string-append "-" t)) drop)
                      (if archive? '("-inbox") '()))))
    (unless (null? ops)
      (nm--op 'tag! ops (string-append "thread:" id)))
    ops))

;; notmuch's json carries only From/To/Cc/Subject/Date, and the delivery axis
;; lives in the headers it leaves out. These are the ones that say who a
;; message was written for. They go raw and undecoded, folded lines kept.
(define notmuch-autotag-headers
  '("from" "sender" "to" "cc" "reply-to" "return-path" "delivered-to"
    "envelope-to" "subject" "date" "list-id" "list-unsubscribe" "list-post"
    "precedence" "auto-submitted" "x-mailer" "x-campaign-id" "feedback-id"))

;; How many of a thread's messages send their headers. The first few settle
;; the kind; a long thread would spend the whole budget on repeats.
(define notmuch-autotag-header-messages 3)

(define nm--autotag-my-addresses #f)

(define (nm--autotag-addresses)
  ;; One notmuch config read per session: the reader's own addresses, so the
  ;; model can see whether the envelope names them or somebody else.
  (unless nm--autotag-my-addresses
    (set! nm--autotag-my-addresses
          (filter (lambda (s) (not (equal? s "")))
                  (map string-trim
                       (append (string-split (nm--op 'config "user.primary_email") "\n")
                               (string-split (nm--op 'config "user.other_email") "\n"))))))
  nm--autotag-my-addresses)

(define (nm--autotag-header-text msg-id)
  (let ((prog (string-append
                "/^$/{exit} /^[ \\t]/{if(p)print;next} "
                "{p=(tolower($0) ~ /^(" (string-join notmuch-autotag-headers "|")
                "):/); if(p)print}")))
    (string-trim
      (nm--op 'raw msg-id (string-append "awk " (sh-quote prog))))))

(define (nm--autotag-header-block id)
  (let* ((all (nm--show-msgs id))
         (msgs (if (> (length all) notmuch-autotag-header-messages)
                   (list-head all notmuch-autotag-header-messages)
                   all)))
    (string-append
      "This reader's own addresses: "
      (string-join (nm--autotag-addresses) ", ") "\n\n"
      "Raw headers, as delivered:\n"
      (string-join (map (lambda (m) (nm--autotag-header-text (plist-get m 'id))) msgs)
                   "\n\n"))))

(define (nm--autotag-input id)
  ;; Headers first and whole: the body is cut at the limit, the evidence for
  ;; who this was written for never is.
  (string-append (nm--autotag-header-block id)
                 "\n\nThread:\n"
                 (nm--trunc (mail-read-thread id) notmuch-autotag-limit)))

(define (nm--autotag-one! id vocab k)
  ;; K gets the signed ops this thread actually took, so the caller reports a
  ;; real change and never a blind "done", or #f when the read, the model or
  ;; the write failed, so a failure never passes for "nothing to change".
  ;; Each notmuch call is a blocking trip to the mail host, so the read and the
  ;; write run in tasks; on the lane they would put the threads in a line
  ;; behind each other.
  (task-run! (lambda () (nm--autotag-input id))
    (lambda (ok? input)
      (if (not ok?)
          (k #f)
          (jev-ask input
                   (nm--autotag-questions vocab)
            (lambda (reply)
              (if (not reply)
                  (k #f)
                  (let* ((chosen (jev-answer-choice reply 'tag))
                         (tag (and chosen (nm--autotag-tag-of vocab chosen)))
                         (picked (jev-answer-choice reply 'kind))
                         (kind (and picked (nm--autotag-kind-of picked)))
                         (want (append (if tag (list tag) '())
                                       (if kind (list kind) '())
                                       (nm--autotag-flags-said-yes reply))))
                    (task-run! (lambda () (nm--autotag-apply! id want))
                      (lambda (ok? ops)
                        (when (and ok? (pair? ops)) (nm--autotag-publish! id ops))
                        (k (and ok? ops))))))))))))

(define (nm--autotag-publish! id ops)
  "(nm--autotag-publish! ID OPS) — the tags autotag gave and took from the thread ID
go on mail-tag:PROFILE as a tagged event, with the first message's From and Subject"
  (let ((profile notmuch-profile)
        (signed (lambda (sign) (map (lambda (o) (substring o 1 (string-length o)))
                                    (filter (lambda (o) (string-prefix? sign o)) ops)))))
    (task-run!
      (lambda ()
        (let ((msgs (nm--flatten-msgs
                      (or (nm--op 'show id #f) '()))))
          (if (null? msgs) '() (plist-get (car msgs) 'headers))))
      (lambda (ok? h)
        (let ((h (if ok? h '())))
          (event-publish! (string-append "mail-tag:" profile) 'tagged
            (list 'id id 'mailbox profile 'added (signed "+") 'removed (signed "-")
                  'from (or (plist-get h 'From) "") 'subject (or (plist-get h 'Subject) ""))))))))

(defcustom 'notmuch-autotag-parallel 8
  "How many threads notmuch-autotag classifies at the same time.")

(define (nm--autotag-run! buf ids vocab &optional done)
  ;; Up to notmuch-autotag-parallel threads in flight. Each callback runs on
  ;; the lane, so the counters need no lock. The refresh and the report come
  ;; once, after the last thread answers. BUF is #f when no list asked.
  ;; The addresses are read here, on the lane, so no task sets the global.
  (nm--autotag-addresses)
  ;; DONE gets how many threads failed.
  (let ((queue ids) (left (length ids)) (changed '()) (failed 0))
    (define (finish!)
      (when buf (nm--refresh! buf))
      (message (string-append
                 (if (null? changed)
                     "Autotag: nothing to change"
                     (string-append "Autotag: " (string-join (nm--uniq changed) " ")))
                 (if (= failed 0)
                     ""
                     (string-append "; " (number->string failed) " failed"))))
      (when done (done failed)))
    (define (next!)
      (unless (null? queue)
        (let ((id (car queue)))
          (set! queue (cdr queue))
          (nm--autotag-one! id vocab
            (lambda (ops)
              (if ops
                  (set! changed (append changed ops))
                  (set! failed (+ failed 1)))
              (set! left (- left 1))
              (if (= left 0)
                  (finish!)
                  (begin
                    (message (string-append "Autotag: " (number->string left) " to go"))
                    (next!))))))))
    (message (string-append "Autotag: " (number->string left) " to go"))
    (let loop ((n (max 1 notmuch-autotag-parallel)))
      (when (and (> n 0) (not (null? queue)))
        (next!)
        (loop (- n 1))))))

(define-command "notmuch-autotag"
  "Classify the marked threads, or the thread at point, with this mailbox's own tags; only untagged ones unless C-u"
  (lambda ()
    (let* ((buf (current-buffer))
           (vocab (nm--autotag-vocabulary))
           (ids (nm--autotag-targets buf (current-prefix-arg))))
      (cond ((null? ids) (message "Autotag: no unprocessed thread here (C-u to take all)"))
            ((null? vocab) (message "This mailbox has no tags to classify with"))
            ((not (jev-api-key)) (message "Autotag needs a JEV key: set TYPESAFE_API_KEY"))
            (else (nm--autotag-run! buf ids vocab)))))
  'modes '(notmuch-mode))
(catalog-meta! 'command "notmuch-autotag" 'domain 'mail
               'effects '(write external execute spend))

;; Autotag new mail: classify each thread that came in since the last run. The
;; cursor is the mailbox's lastmod revision, read after the run's own tag
;; writes, so those writes never bring a thread back. The date bound keeps a
;; tag change on old mail out of the set.
(defcustom 'notmuch-autotag-new-enabled #f
  "When true, notmuch-autotag-new runs every notmuch-autotag-new-seconds.")

(defcustom 'notmuch-autotag-new-seconds 300
  "How often the new-mail autotag looks for mail it has not classified.")

(defcustom 'notmuch-autotag-new-stale-seconds 900
  "How long a new-mail autotag run may hold the lock before the next tick takes it.")

;; The current run as its start time, or #f. A run that never answers (a hung
;; ssh trip, a JEV call that never calls back) must not stop new mail being
;; tagged for good, so a tick takes a lock older than the stale limit, and the
;; old run's late answers find their start time gone and do nothing.
(define nm--autotag-new-running #f)

(define (nm--autotag-new-key) (string-append "notmuch-autotag:" notmuch-host ":" notmuch-profile))

(define (nm--autotag-revision)
  ;; Blocks on the mail host: call it from a task.
  (nm--op 'revision))

(define (nm--autotag-new-ids after since)
  ;; Blocks on the mail host: call it from a task.
  (filter (lambda (l) (not (equal? l "")))
          (map nm--autotag-thread-id
               (nm--op 'changed-threads (+ after 1) (- since 86400)
                       (string-append
                        " and not tag:sent and not tag:draft"
                        " and not tag:spam and not tag:trash"
                        (if notmuch-autotag-only-unprocessed
                            (nm--autotag-unprocessed-clause)
                            ""))))))

(define (nm--autotag-new-save! rev now)
  (event-log-position-set! (nm--autotag-new-key) rev)
  (event-log-position-set! (string-append (nm--autotag-new-key) ":since") now))

(define (nm--autotag-new!)
  ;; One run at a time. The first run only saves the cursor.
  (unless (or (and nm--autotag-new-running
                   (< (- (current-time) nm--autotag-new-running)
                      notmuch-autotag-new-stale-seconds))
              (not (jev-api-key)))
    (let ((after (event-log-position (nm--autotag-new-key)))
          (since (event-log-position (string-append (nm--autotag-new-key) ":since")))
          (now (current-time)))
      (define (mine?) (eqv? nm--autotag-new-running now))
      (define (stop!) (when (mine?) (set! nm--autotag-new-running #f)))
      (define (save! rev) (when (mine?) (nm--autotag-new-save! rev now)))
      (define (save-after-writes! failed)
        ;; The cursor moves after the run's own writes, so read it then. A run
        ;; with a failed thread leaves it where it was, so the next tick tries
        ;; again; the threads it did tag are processed by then and drop out.
        (task-run! (lambda () (and (= failed 0) (nm--autotag-revision)))
          (lambda (ok? rev)
            (when (and ok? rev) (save! rev))
            (stop!))
          120000))
      (set! nm--autotag-new-running now)
      (task-run!
       (lambda ()
         (list (nm--autotag-revision)
               (if (and after since) (nm--autotag-new-ids after since) '())
               (nm--autotag-vocabulary)))
       (lambda (ok? value)
         (let ((rev (and ok? (mine?) (car value)))
               (ids (if ok? (cadr value) '()))
               (vocab (if ok? (caddr value) '())))
           (cond ((not rev) (stop!))
                 ((or (null? ids) (null? vocab))
                  (save! rev)
                  (stop!))
                 (else (nm--autotag-run! #f ids vocab save-after-writes!)))))
       120000))))

(define-command "notmuch-autotag-new"
  "Classify each thread that came in since the last run, with this mailbox's own tags"
  (lambda () (nm--autotag-new!)))
(catalog-meta! 'command "notmuch-autotag-new" 'domain 'mail
               'effects '(write external execute spend))

(define (nm--autotag-new-tick _)
  (when notmuch-autotag-new-enabled
    (report-errors "notmuch: autotag new mail" (lambda () (nm--autotag-new!)))
    (debounce! 'notmuch-autotag-new (* 1000 notmuch-autotag-new-seconds)
               (lambda (x) (nm--autotag-new-tick x)) #f)))

(define (nm--autotag-new-boot!)
  (when notmuch-autotag-new-enabled (nm--autotag-new-tick #f)))
(after-init 'nm--autotag-new-boot!)



;;; --- jump & filter ---------------------------------------------------------------

;; ((key name query) ...) — personal jump table, set in init.scm; empty
;; falls back to the saved searches by name
(define notmuch-jump-searches '())

(define-command "notmuch-jump" "Jump to a saved search (j, then its key)"
  (lambda ()
    (if (null? notmuch-jump-searches)
        (let ((ss (nm--saved-searches)))
          (minibuffer-read "Jump: "
            (map (lambda (s) (list (car s) (cadr s))) ss)
            (lambda (name)
              (let ((e (assoc name ss)))
                (when e (nm--open-index! (cadr e)))))))
        (minibuffer-read "Jump: "
          (map (lambda (s) (list (car s) (string-append (cadr s) " · " (caddr s))))
               notmuch-jump-searches)
          (lambda (key)
            (let ((e (assoc key notmuch-jump-searches)))
              (when e (nm--open-index! (caddr e)))))))))

(define-command "notmuch-filter" "Narrow this search with more terms (and)"
  (lambda ()
    (let ((buf (current-buffer)))
      (minibuffer-read "Filter (and): " '()
        (lambda (terms)
          (let ((term (string-trim terms)))
            (unless (equal? term "")
              (nm--query-push! buf term)
              (nm--refresh! buf)
              (list-goto-first-entry buf)))))))
  'modes '(notmuch-mode))

(domain! 'mail)
(effects! '(write external execute))

(define-command "notmuch-filter-by-tag"
  "Choose a tag from the current results and add it to this search"
  (lambda ()
    (let* ((buf (current-buffer))
           (tags (nm--query-tags buf)))
      (if (null? tags)
          (message "No tags occur in this search")
          (minibuffer-read "Add tag filter: " tags
            (lambda (choice)
              (let ((tag (string-trim choice)))
                (unless (equal? tag "")
                  (nm--query-push! buf (string-append "tag:" tag))
                  (nm--refresh! buf)
                  (list-goto-first-entry buf)
                  (message (string-append "Added tag filter: " tag)))))))))
  'modes '(notmuch-mode))

(effects! '(unknown))

(define-command "notmuch-unfilter-last" "Remove the most recently added notmuch filter"
  (lambda ()
    (let* ((buf (current-buffer))
           (positions (or (buffer-local-value 'notmuch-query-positions buf) '()))
           (pos (if (null? positions) #f (car positions))))
      (if (not (nm--query-pop! buf))
          (message "No structured notmuch filters to remove")
          (begin
            (nm--after-change! buf pos)
            (message "Removed last notmuch filter")))))
  'modes '(notmuch-mode))

(define-command "notmuch-back" "Remove the last mail filter, or return to mailboxes"
  (lambda ()
    (if (pair? (nm--query-filters-of (current-buffer)))
        (run-command "notmuch-unfilter-last")
        (run-command "notmuch")))
  'modes '(notmuch-mode))
(catalog-meta! 'command "notmuch-back" 'domain 'mail 'effects '(write external execute))

(define-command "notmuch-filter-by-sender" "Narrow the search to this thread's sender"
  (lambda ()
    (let* ((buf (current-buffer))
           (thread-id (nm--thread-here))
           (email (and thread-id (nm--thread-sender thread-id))))
      (cond
        ((not thread-id) (message "No thread here"))
        ((equal? email "") (message "Could not extract the sender"))
        (else
          (nm--query-push-only! buf (string-append "from:" email))
          (nm--refresh! buf)
          (list-goto-first-entry buf)
          (message (string-append "from:" email))))))
  'modes '(notmuch-mode))

(define (nm--thread-here)
  "The thread the command acts on: an open notmuch-show buffer names its thread, a list row carries one."
  (let ((buf (current-buffer)))
    (if (buffer-derived-mode? buf "notmuch-show-mode")
        (buffer-local-value 'notmuch-thread buf)
        (let ((th (nm--thread-at buf))) (and th (nm--th-id th))))))

(define (nm--thread-sender thread-id)
  "The bare address of the thread's first message, or an empty string."
  (let* ((msgs (nm--flatten-msgs
                 (or (nm--op 'show thread-id #f)
                     '())))
         (from (if (null? msgs)
                   ""
                   (or (plist-get (plist-get (car msgs) 'headers) 'From) "")))
         (parts (string-split from "<")))
    (if (null? (cdr parts))
        (string-trim from)
        (car (string-split (cadr parts) ">")))))



(define (nm--host-cmd script)
  "SCRIPT as a command line that runs where the mail store lives: over ssh when
notmuch-host names another machine, here when it is empty."
  (if (or (equal? notmuch-host "") (symbol? (car (nm--current-mailbox))))
      script
      (string-append notmuch-ssh-program " " (sh-quote notmuch-host) " " (sh-quote script))))

(define (nm--host-sh script)
  "Run SCRIPT where the mail store lives and answer its output."
  (shell-command->string (nm--host-cmd script)))



(define (nm--trash-sender! target)
  "Trash every message from TARGET, an address or a whole domain, block it, and
refresh the index. Answers how many matched."
  (let ((n (nm--count (string-append "from:" target))))
    (nm--op 'tag! "+trash +blocked -inbox -unread" (string-append "from:" target))
    (nm--op 'block-sender! target)
    (nm--after-change! *notmuch-search-buffer*)
    n))

(define (nm--trashed-label n email)
  (string-append "trashed " (number->string n) " message" (if (= n 1) "" "s")
                 " from " email "; blocked " email))

(define-command "notmuch-delete-sender"
  "Trash every message in the mailbox from this thread's sender, with no unsubscribe attempt (works on a *notmuch* list row or an open notmuch-show buffer)"
  (lambda ()
    (let ((thread-id (nm--thread-here)))
      (if (not thread-id)
          (message "No thread here")
          (let ((email (nm--thread-sender thread-id)))
            (if (equal? email "")
                (message "Could not extract the sender")
                (message (nm--trashed-label (nm--trash-sender! email) email))))))))
(catalog-meta! 'command "notmuch-delete-sender" 'domain 'mail 'effects '(destroy))

(define (nm--contains? haystack needle)
  (> (length (string-split (string-downcase haystack) (string-downcase needle))) 1))

(define (nm--looks-unsubscribed? html)
  (or (nm--contains? html "unsubscribed")
      (nm--contains? html "successfully removed")
      (nm--contains? html "been removed")
      (nm--contains? html "you're unsubscribed")
      (nm--contains? html "miss you")
      (nm--contains? html "no longer receive")))

;; RFC 2369/8058: List-Unsubscribe (and List-Unsubscribe-Post for the
;; one-click POST variant) are plain header text, never quoted-printable —
;; decoding them would corrupt hash params like "u=80fc49..." that happen
;; to look like =XX escapes. Read them raw; only the body fallback below
;; needs QP decoding, and only as a last resort for senders with no header.
(define (nm--unsubscribe-header-links msg-id)
  (let* ((hdr (string-trim (nm--op 'raw msg-id "grep -i '^list-unsubscribe:' | head -1")))
         (post (string-trim (nm--op 'raw msg-id "grep -i '^list-unsubscribe-post:' | head -1")))
         (https (let ((m (string-trim (shell-command->string
                    (string-append "printf '%s' " (sh-quote hdr)
                                   " | grep -oE '<https?://[^>]*>' | head -1")
                    (default-directory)))))
                  (if (equal? m "") #f (substring m 1 (- (string-length m) 1)))))
         (mailto (let ((m (string-trim (shell-command->string
                    (string-append "printf '%s' " (sh-quote hdr)
                                   " | grep -oE '<mailto:[^>]*>' | head -1")
                    (default-directory)))))
                   (if (equal? m "") #f (substring m 1 (- (string-length m) 1)))))
         (one-click? (nm--contains? post "one-click")))
    (list https mailto one-click?)))

;; last resort when there's no List-Unsubscribe header at all: naive
;; whole-message quoted-printable decode, then hunt for an "unsubscribe"
;; link in the body. No real MIME parsing, so it can find the wrong link
;; or nothing on an oddly-encoded message — acceptable as a fallback only.
(define (nm--unsubscribe-body-link msg-id)
  (let* ((cmd (string-append
                "perl -MMIME::QuotedPrint -0777 -ne '"
                "my $raw = $_; my $dec = eval { decode_qp($raw) }; $dec = $raw unless defined $dec; "
                "if ($dec =~ /(https?:\\/\\/[^\\s\"\\x27<>]*unsubscribe[^\\s\"\\x27<>]*)/i) { print \"$1\\n\"; exit } "
                "if ($raw =~ /(https?:\\/\\/[^\\s\"\\x27<>]*unsubscribe[^\\s\"\\x27<>]*)/i) { print \"$1\\n\"; exit }'"))
         (out (string-trim (nm--op 'raw msg-id cmd))))
    (if (equal? out "") #f out)))

(define (nm--curl-text url)
  (shell-command->string (string-append "curl -sL --max-time 15 " (sh-quote url)) (default-directory)))

(define (nm--unsubscribe-by-agent! url why)
  "Hand URL to a Haiku chat that unsubscribes in a browser tab. WHY is the verdict so
far; the answer says the agent took over, or WHY again when the browser is away."
  (if (not (browser-connected?))
      (string-append why " (no browser connected for a fallback agent)")
      (begin
        (execute*
          (string-append
            "Unsubscribe me from a mailing list in my own Chrome. The unsubscribe page is:\n"
            url "\n\n"
            "Use the compos editor through eval-scheme. Check (browser-connected?), open the page in a "
            "background tab with (tab-open URL #f #t), find it with (tab-list K), read it with (tab-read TAB K) "
            "and click with (tab-eval TAB JS K). These are async: let K write into a scratch buffer and read it "
            "on the next call. Press whatever unsubscribes from everything (an unsubscribe or opt-out button, "
            "an 'all emails' choice, a confirm). Never sign in, never type a password or card, never subscribe "
            "to anything. When the page confirms, or you give up, close the tab with (tab-close TAB), kill your "
            "scratch buffer, and say the outcome with (message \"unsubscribe: ...\"). Reply in one line.")
          (list 'connector "claude-code" 'model "haiku" 'title "Unsubscribe in browser"))
        (string-append why "; a Haiku agent is unsubscribing in a browser tab"))))

(define (nm--purge-unsubscribe! msg-id)
  (let* ((links (and msg-id (nm--unsubscribe-header-links msg-id)))
         (https (and links (car links)))
         (mailto (and links (cadr links)))
         (one-click? (and links (caddr links))))
    (cond
      ((and https one-click?)
       (let ((code (string-trim (shell-command->string
                      (string-append "curl -sL --max-time 15 -X POST "
                                     "-H 'Content-Type: application/x-www-form-urlencoded' "
                                     "-d 'List-Unsubscribe=One-Click' -o /dev/null -w '%{http_code}' "
                                     (sh-quote https))
                      (default-directory)))))
         (if (member code '("200" "202" "204"))
             (string-append "unsubscribed (RFC 8058 one-click, " code ")")
             (nm--unsubscribe-by-agent! https (string-append "one-click unsubscribe got HTTP " code)))))
      (https
       (if (nm--looks-unsubscribed? (nm--curl-text https))
           "unsubscribed (confirmed)"
           (nm--unsubscribe-by-agent! https "couldn't confirm the unsubscribe link")))
      (mailto
       (string-append "unsubscribe is by email only, not sent: " mailto))
      (else
        (let ((body-link (and msg-id (nm--unsubscribe-body-link msg-id))))
          (cond
            ((not body-link) "no unsubscribe link found")
            ((nm--looks-unsubscribed? (nm--curl-text body-link))
             "unsubscribed (found in body, confirmed)")
            (else (nm--unsubscribe-by-agent! body-link "couldn't confirm the link in the body"))))))))

(define (nm--purge-phish? thread-id)
  "#t when this thread is a fake. A phisher's unsubscribe link is bait: it tells
them the address is live, so a purge never follows one."
  (> (nm--count (string-append "thread:" thread-id " AND \\(tag:phishing OR tag:spam\\)")) 0))

(define (nm--purge-run! thread-id email)
  "Unsubscribe, trash every message from EMAIL, and block that one address."
  (let* ((verdict (if (nm--purge-phish? thread-id)
                      "no unsubscribe: the sender is a fake"
                      (nm--purge-unsubscribe! (nm--newest-msg-id thread-id))))
         (n (nm--trash-sender! email)))
    (message (string-append (nm--trashed-label n email) "; " verdict))))

(define-command "notmuch-purge-sender"
  "Purge this thread's sender: unsubscribe, trash every message from them, and block that one address out of the inbox for good. The block is the address alone, never the domain, because one address at a shared or hijacked domain says nothing about the rest of it (works on a *notmuch* list row or an open notmuch-show buffer)"
  (lambda ()
    (let ((thread-id (nm--thread-here)))
      (if (not thread-id)
          (message "No thread here")
          (let ((email (nm--thread-sender thread-id)))
            (if (equal? email "")
                (message "Could not extract the sender")
                (nm--purge-run! thread-id email))))))
  'modes '(notmuch-mode))

(catalog-meta! 'command "notmuch-purge-sender" 'domain 'mail 'effects '(destroy external))

(define-command "notmuch-unsubscribe"
  "Try to unsubscribe from this thread's sender without deleting anything: RFC 8058 one-click POST when offered, else a plain GET, else a best-effort scan of the body (works on a *notmuch* list row or an open notmuch-show buffer)"
  (lambda ()
    (let* ((buf (current-buffer))
           (thread-id (if (buffer-derived-mode? buf "notmuch-show-mode")
                          (buffer-local-value 'notmuch-thread buf)
                          (let ((th (nm--thread-at buf))) (and th (nm--th-id th))))))
      (if (not thread-id)
          (message "No thread here")
          (let* ((msgs (nm--flatten-msgs
                         (or (nm--op 'show thread-id #f)
                             '())))
                 (from (if (null? msgs)
                           ""
                           (or (plist-get (plist-get (car msgs) 'headers) 'From) "")))
                 (email (let ((parts (string-split from "<")))
                          (if (null? (cdr parts))
                              (string-trim from)
                              (car (string-split (cadr parts) ">"))))))
            (if (equal? email "")
                (message "Could not extract the sender")
                (let* ((msg-id (nm--newest-msg-id thread-id))
                       (verdict (nm--purge-unsubscribe! msg-id)))
                  (message (string-append email ": " verdict)))))))))
(catalog-meta! 'command "notmuch-unsubscribe" 'domain 'mail 'effects '(write external))

;; Filing for the agent: `+liked` marks a thread as worth keeping and takes it
;; out of the inbox. The tag is the whole record, so any agent query can read it
;; back later with tag:liked.
(define-command "notmuch-like"
  "File this thread for the agent (+liked -inbox): the marked threads on a list row, or the thread at point, or the open thread"
  (lambda ()
    (let ((buf (current-buffer)))
      (cond
        ((buffer-derived-mode? buf "notmuch-show-mode")
         (let ((th (buffer-local-value 'notmuch-thread buf)))
           (if (not th)
               (message "No thread here")
               (begin
                 (nm--op 'tag! "+liked -inbox" (string-append "thread:" th))
                 (when (buffer-exists? *notmuch-search-buffer*)
                   (nm--refresh! *notmuch-search-buffer*))
                 (message "Liked")))))
        ((nm--any-marked? buf) (nm--confirm-marked buf "Like" "+liked -inbox"))
        (else (nm--tag! buf "+liked -inbox")))))
  'modes '(notmuch-mode notmuch-show-mode))

(catalog-meta! 'command "notmuch-like" 'domain 'mail 'effects '(write))

 ;;; --- local selection ---------------------------------------------------------

;; Selection is an editor operation, never a mail tag. ALL selects the query;
;; IDS then excludes individual threads. Otherwise IDS names selected threads.
;; The query is evaluated when the mail action runs, including unseen results.
(define (nm--selection buf)
  (let ((s (buffer-local-value 'notmuch-selection buf)))
    (if (and s (equal? (car s) (nm--db-key))
               (equal? (cadr s) (nm--query-of buf)))
        s
        (begin (buffer-set-local! buf 'notmuch-selection #f) #f))))

(define (nm--set-selection! buf all? ids)
  (desktop-skip! buf 'notmuch-selection)
  (buffer-set-local! buf 'notmuch-selection
    (list (nm--db-key) (nm--query-of buf) all? ids)))

(define (nm--row-marked? buf th)
  (let ((s (nm--selection buf)))
    (and s (if (caddr s)
               (not (member (nm--th-id th) (list-ref s 3)))
               (if (member (nm--th-id th) (list-ref s 3)) #t #f)))))

(define (nm--any-marked? buf)
  (let ((s (nm--selection buf)))
    (and s (or (caddr s) (pair? (list-ref s 3))))))

(define (nm--draw-marks! buf rows)
  (list-marks-set! buf
    (map (lambda (th) (list (nm--th-id th) *list-mark-char*))
         (filter (lambda (th) (nm--row-marked? buf th)) rows))))

(define (nm--redraw-marks! buf)
  (nm--draw-marks! buf (list-entries buf))
  ;; Plain list redraws fetch their source. Cached rendering keeps the rows,
  ;; exact count, point, and preview, with no external command.
  (when (pair? (list-entries buf)) (list-render! buf 'cached)))

(define (nm--marked-query buf)
  (let* ((s (nm--selection buf))
         (ids (if s (list-ref s 3) '()))
         (threads (string-join (map (lambda (id) (string-append "thread:" id)) ids) " or ")))
    (if (not (nm--any-marked? buf))
        "tag:inbox and not tag:inbox"
        (string-append "( " (cadr s) " )"
          (if (null? ids) ""
              (string-append (if (caddr s) " and not ( " " and ( ") threads " )"))))))

(define (nm--marked-tags buf)
  (nm--op 'tags (nm--marked-query buf)))

(define (nm--tag-marked! buf changes)
  (if (not (nm--any-marked? buf))
      (message "No selected messages")
      (begin
        (nm--op 'tag! changes (nm--marked-query buf))
        ;; Keep the local selection across the refresh. The selected thread IDs
        ;; remain valid even when their displayed tags change.
        (nm--after-change! buf))))

(define (nm--toggle-selection! buf id)
  (let* ((s (nm--selection buf))
         (ids (if s (list-ref s 3) '())))
    (nm--set-selection! buf (and s (caddr s))
      (if (member id ids)
          (filter (lambda (other) (not (equal? other id))) ids)
          (cons id ids)))
    (nm--redraw-marks! buf)))

(define-command "notmuch-mark-toggle" "Toggle local selection of this thread, move down"
  (lambda ()
    (let* ((buf (current-buffer)) (th (nm--thread-at buf)))
      (if th
          (begin (nm--toggle-selection! buf (nm--th-id th)) (list-move-in! buf 1))
          (message "No thread on this line"))))
  'modes '(notmuch-mode))

(define-command "notmuch-mark-all" "Select the entire filtered inbox or search locally; again clears selection"
  (lambda ()
    (let* ((buf (current-buffer)) (unmark? (nm--any-marked? buf)))
      (cond
        (unmark?
          (buffer-set-local! buf 'notmuch-selection #f)
          (nm--redraw-marks! buf)
          (message "Unmarked all"))
        ((and (or (equal? (nm--query-base-of buf) "tag:inbox")
                  (equal? (nm--query-base-of buf) notmuch-default-query))
              (null? (nm--query-filters-of buf)))
          (message "Add a filter before selecting all inbox messages"))
        ((null? (list-entries buf)) (message "No messages to select"))
        (else
          (nm--set-selection! buf #t '())
          (nm--redraw-marks! buf)
          (message "Selected entire search")))))
  'modes '(notmuch-mode))

(define-command "notmuch-unmark-all" "Clear local mail selection"
  (lambda ()
    (let ((buf (current-buffer)))
      (buffer-set-local! buf 'notmuch-selection #f)
      (nm--redraw-marks! buf)
      (message "Unmarked all")))
  'modes '(notmuch-mode))

(define-command "notmuch-filter-marked" "Open the selected messages as a search"
  (lambda ()
    (let ((buf (current-buffer)))
      (if (not (nm--any-marked? buf))
          (message "No selected messages")
          (begin
            (nm--query-push-only! buf (nm--marked-query buf))
            (nm--refresh! buf)
            (list-goto-first-entry buf)))))
  'modes '(notmuch-mode))

(for-each (lambda (name)
            (catalog-meta! 'command name 'domain 'mail 'effects '(read write display)))
  '("notmuch-mark-toggle" "notmuch-mark-all" "notmuch-unmark-all"))

(define (nm--confirm-marked buf verb changes)
  (minibuffer-read (string-append verb " all marked threads? ")
    (list "yes" "no")
    (lambda (ans)
      (if (equal? ans "yes")
          (begin (nm--tag-marked! buf changes) (message "Done"))
          (message "Cancelled")))))

(define-command "notmuch-archive-marked" "Archive all marked threads"
  (lambda () (nm--confirm-marked (current-buffer) "Archive"
                        "-inbox"))
  'modes '(notmuch-mode))
(define-command "notmuch-trash-marked" "Trash all marked threads"
  (lambda () (nm--confirm-marked (current-buffer) "Trash"
                        "+trash -inbox -unread"))
  'modes '(notmuch-mode))

(define-command "notmuch-tag-marked" "Apply tag changes to all marked threads"
  (lambda ()
    (let ((buf (current-buffer)))
      (minibuffer-read "Tag marked (+add -remove): " '()
        (lambda (changes) (nm--tag-marked! buf changes) (message changes)))))
  'modes '(notmuch-mode))

;;; --- thread (show) buffer -------------------------------------------------------

(define (nm--attachment-parts parts)
  (fold (lambda (acc part)
          (append acc
            (cond ((and (plist-get part 'filename) (number? (plist-get part 'id))) (list part))
                  ((pair? (plist-get part 'content))
                   (nm--attachment-parts (plist-get part 'content)))
                  (else '()))))
        '() parts))

;; the image parts with a Content-ID that sit straight under a multipart/related:
;; RFC 2387 makes them resources of the body. Gmail puts a quoted mail's images
;; there, marked attachment, under fresh ii_ ids that no cid: cites.
(define (nm--related-image-ids parts)
  (fold (lambda (acc part)
          (let* ((ct (string-downcase (or (plist-get part 'content-type) "")))
                 (kids (plist-get part 'content))
                 (kids (if (pair? kids) kids '()))
                 (own (if (string=? ct "multipart/related")
                          (map (lambda (p) (plist-get p 'id))
                               (filter (lambda (p) (and (nm--image-part? p)
                                                        (string? (plist-get p 'content-id))))
                                       kids))
                          '())))
            (append acc own (nm--related-image-ids kids))))
        '() parts))

;; the attachments a reader picks from: an image the body draws by cid: is
;; part of the body, not an attachment, and so is an image of a multipart/related
(define (nm--listed-attachment-parts msg &optional raw-html)
  (let* ((body (plist-get msg 'body))
         (raw (or raw-html (nm--parts-html body)))
         (related (nm--related-image-ids body))
         (inline? (lambda (p)
                    (let ((cid (plist-get p 'content-id)))
                      (and (nm--image-part? p)
                           (or (member (plist-get p 'id) related)
                               (and (string? raw) (string? cid)
                                    (string-search (string-append "cid:" cid) raw))))))))
    (filter (lambda (p) (not (inline? p)))
            (nm--attachment-parts body))))

(define (nm--attachment-text msg)
  (let ((parts (nm--listed-attachment-parts msg)))
    (if (null? parts) ""
        (string-append "Attachments (C-c a to open):\n"
          (string-join (map (lambda (p) (string-append "  " (plist-get p 'filename))) parts) "\n")
          "\n\n"))))

(define (nm--image-part? p)
  (let ((ct (string-downcase (or (plist-get p 'content-type) ""))))
    (and (string-prefix? "image/" ct) (not (string-contains? ct "svg")))))

;; A thumbnail is cache: a small JPEG, base64, keyed by message and part.
(define (nm--thumb-path msg p)
  (cache-path "mail-thumbs"
    (string-append (content-hash (string-append (or (plist-get msg 'id) "") "/"
                                                (number->string (plist-get p 'id))))
                   ".b64")))

(define (nm--thumb-html msg p)
  ;; an image shows its thumbnail; any other file the icon of the mode
  ;; that opens it, the same glyph the buffer lists wear
  (let ((path (nm--thumb-path msg p)))
    (if (and (nm--image-part? p) (file-exists? path))
        (string-append "<img alt=\"" (html-escape (plist-get p 'filename))
                       "\" src=\"data:image/jpeg;base64," (read-file path) "\">")
        (let ((glyph (file-icon (or (plist-get p 'filename) ""))))
          (if (and (string? glyph) (not (string=? glyph "")))
              (string-append "<i class=\"mail-file-icon\">" glyph "</i>")
              "")))))

;; inline images: the body names a part by cid:CONTENT-ID; draw its thumb there
(define (nm--inline-cids msg html)
  (fold (lambda (acc p)
          (let ((cid (plist-get p 'content-id)) (path (nm--thumb-path msg p)))
            (if (and (string? cid) (nm--image-part? p) (file-exists? path))
                (string-replace acc (string-append "cid:" cid)
                                (string-append "data:image/jpeg;base64," (read-file path)))
                acc)))
        html (nm--attachment-parts (plist-get msg 'body))))

(define (nm--attachment-html msg &optional raw-html)
  (let ((parts (nm--listed-attachment-parts msg raw-html)))
    (if (null? parts) ""
        (string-append "<mail-attachments><strong>Attachments</strong>"
          (string-join (map (lambda (p)
                             (string-append "<mail-attachment part-id=\"" (number->string (plist-get p 'id)) "\">"
                                            (nm--thumb-html msg p)
                                            "<span>" (html-escape (plist-get p 'filename)) "</span></mail-attachment>"))
                           parts) "")
          "<small>C-c a to open an attachment</small></mail-attachments>"))))

;; Make the missing thumbnails of MSGS off the editor lane, then DONE.
;; Bytes go notmuch -> file -> sips -> base64 file; never through Scheme.
(define (nm--make-thumbs! msgs done)
  (let ((jobs (fold (lambda (acc msg)
                      (append acc
                        (filter (lambda (x) x) (map (lambda (p)
                                      (let ((out (nm--thumb-path msg p)))
                                        (and (nm--image-part? p) (not (file-exists? out))
                                             (string-append
                                               "t=$(mktemp -d) && "
                                               (nm--op 'part-command (plist-get msg 'id) (plist-get p 'id))
                                               " > \"$t/in\" && sips -s format jpeg -Z 320 \"$t/in\" --out \"$t/t.jpg\" >/dev/null 2>&1"
                                               " && base64 -i \"$t/t.jpg\" | tr -d '\\n' > " (sh-quote (string-append out ".part"))
                                               " && mv " (sh-quote (string-append out ".part")) " " (sh-quote out)
                                               "; rm -rf \"$t\""))))
                                    (nm--attachment-parts (plist-get msg 'body))))))
                    '() msgs)))
    (unless (null? jobs)
      (shell-command->string (string-append (string-join jobs " ; ") " ; printf thumbs-done")
        (cache-path "mail-thumbs" "")
        (lambda (result) (done))))))

;; Capture the full fetch command while this message's account is active.
;; The user can switch accounts while the attachment picker is open.
(define (nm--attachment-options msgs)
  (fold (lambda (acc msg)
          (append acc
            (map (lambda (part)
                   (list (plist-get part 'filename)
                         (nm--op 'part-command (plist-get msg 'id) (plist-get part 'id))))
                 (nm--listed-attachment-parts msg))))
        '() msgs))

(define (nm--attachment-filename name)
  (let ((base (car (reverse (string-split
                             (string-join (string-split name "\\") "/") "/")))))
    (if (member base '("" "." "..")) "attachment" base)))

(define nm--pdf-unlock-js
  "var d=Document.openDocument(scriptArgs[0]);
if(!d.needsPassword()){print('plain')}
else{var p=null;try{p=readline()}catch(e){}
 if(p===null||!d.authenticatePassword(p)){print('locked')}
 else{d.save(scriptArgs[1],'decrypt');print('unlocked')}}
")

;; A PDF with a password asks for it, then opens an unlocked copy beside it.
;; The password reaches mutool on stdin from a file in the private part
;; directory, never on a command line, and the file is gone at once.
(define (nm--open-pdf! dir file name)
  (let* ((path (string-append dir "/" file))
         (js (string-append dir "/.unlock.js"))
         (pw (string-append dir "/.pw"))
         (out (string-append dir "/unlocked-" file))
         (run (lambda (stdin k)
                (shell-command->string
                  (string-append "mutool run " (sh-quote js) " " (sh-quote path) " " (sh-quote out)
                                 " < " stdin " 2>/dev/null; rm -f " (sh-quote pw))
                  (lambda (r) (k (string-trim r)))))))
    (write-file! js nm--pdf-unlock-js)
    (let ()
      (define (ask prompt)
                    (minibuffer-read prompt '()
                      (lambda (input)
                        (if (or (not (string? input)) (string=? input ""))
                            (message "Not opened: no password")
                            (begin
                              (write-file! pw input)
                              (run (sh-quote pw)
                                   (lambda (r)
                                     (cond ((equal? r "unlocked") (visit out))
                                           (else (ask (string-append "Wrong password for " name ": ")))))))))))
      (run "/dev/null"
           (lambda (r)
             (if (equal? r "locked")
                 (ask (string-append "Password for " name ": "))
                 (visit path)))))))

(define (nm--open-attachment! attachment)
  ;; Mail attachments can be fetched again from notmuch: they are cache.
  (let* ((root (cache-path "attachments" "")))
    (make-directory! root)
    (let ((dir (string-trim (shell-command->string
                 (string-append "mktemp -d " (sh-quote (string-append root "/part-XXXXXX")))))))
      (if (not (string-prefix? (string-append root "/part-") dir))
          (message "Could not create an attachment directory")
          (let ((path (string-append dir "/" (nm--attachment-filename (car attachment)))))
            (message (string-append "Downloading " (car attachment) "…"))
            ;; Redirect bytes locally: binary attachments must never pass
            ;; through Scheme strings or text-buffer decoding.
            (shell-command->string
              (string-append (cadr attachment) " > " (sh-quote path) " && printf attachment-ok")
              (lambda (result)
                (cond ((not (equal? result "attachment-ok"))
                       (when (file-exists? path) (delete-file! path))
                       (message "Attachment download failed"))
                      ((string-suffix? ".pdf" (string-downcase path))
                       (nm--open-pdf! dir (nm--attachment-filename (car attachment)) (car attachment)))
                      (else (visit path))))))))))

(define-command "notmuch-open-attachment" "Choose and open an attachment from the selected mail thread"
  (lambda ()
    (let* ((buf (current-buffer))
           (th (if (equal? buf *notmuch-search-buffer*)
                   (let ((row (nm--thread-at buf))) (and row (nm--th-id row)))
                   (buffer-local-value 'notmuch-thread buf)))
           (page (if (nm--show-buffer? buf) buf (nm--thread-buffer th)))
           (cached? (and page
                         (buffer-local-value 'nm-attachment-source page)
                         (or (equal? buf page)
                             (equal? (nm--db-key) (buffer-local-value 'nm-attachment-source page)))))
           (attachments (if cached?
                            (or (buffer-local-value 'notmuch-attachments page) '())
                            (if th (nm--attachment-options (nm--show-msgs th)) '())))
           (choices (let loop ((rest attachments) (i 1) (acc '()))
                      (if (null? rest) (reverse acc)
                          (loop (cdr rest) (+ i 1)
                            (cons (list (string-append (number->string i) ". " (car (car rest)))
                                        (car rest)) acc))))))
      (if (null? choices)
          (message "No attachments in this thread")
          (minibuffer-read "Open attachment: " (map car choices)
            (lambda (choice)
              (let ((hit (assoc choice choices)))
                (when hit (nm--open-attachment! (cadr hit)))))))))
  'modes '(notmuch-mode notmuch-show-mode))
(catalog-meta! 'command "notmuch-open-attachment" 'domain 'mail 'effects '(write external execute display))

;; Body text excludes attachments, which have their own visible list.
;; multipart/alternative prefers its text/plain child.
(define (nm--part-text part)
  (let ((ct (or (plist-get part 'content-type) ""))
        (content (plist-get part 'content)))
    (cond ((plist-get part 'filename) "")
          ((and (string-prefix? "multipart/alternative" ct) (pair? content))
           (let ((plains (filter (lambda (p)
                                   (string-prefix? "text/plain"
                                     (or (plist-get p 'content-type) "")))
                                 content)))
             (if (null? plains)
                 (nm--parts-text content)
                 (nm--part-text (car plains)))))
          ((pair? content) (nm--parts-text content))
          ((and (string-prefix? "text/plain" ct) (string? content)) content)
          ((and (string-prefix? "text/html" ct)) "")
          (else ""))))

(define (nm--parts-text parts)
  (fold (lambda (acc p) (string-append acc (nm--part-text p))) "" parts))

;; first text/html part's content, or #f
(define (nm--part-html part)
  (let ((ct (or (plist-get part 'content-type) ""))
        (content (plist-get part 'content)))
    (cond ((plist-get part 'filename) #f)
          ((and (string-prefix? "text/html" ct) (string? content)) content)
          ((pair? content) (nm--parts-html content))
          (else #f))))

(define (nm--parts-html parts)
  (let loop ((ps parts))
    (if (null? ps)
        #f
        (let ((h (nm--part-html (car ps))))
          (if h h (loop (cdr ps)))))))

;; notmuch show nests messages as [msg, [replies...]] pairs — flatten
(define (nm--flatten-msgs forest)
  (if (null? forest)
      '()
      (append
        (let ((entry (car forest)))
          (if (and (pair? entry) (plist-get (car entry) 'id))
              (cons (car entry) (nm--flatten-msgs (cadr entry)))
              (nm--flatten-msgs entry)))
        (nm--flatten-msgs (cdr forest)))))

(define (nm--show-msgs thread-id)
  (let ((msgs (nm--flatten-msgs
                (or (nm--op 'show thread-id #t)
                    '()))))
    (if notmuch-show-newest-first (reverse msgs) msgs)))

;; text body of one message; falls back to the html part through
;; notmuch-html-renderer when there is no text/plain
(define (nm--msg-body-text msg)
  (let ((plain (nm--parts-text (plist-get msg 'body))))
    (if (equal? (string-trim plain) "")
        (let ((html (nm--parts-html (plist-get msg 'body))))
          (if html (nm--html->text html) plain))
        plain)))

(define (nm--msg-render msg)
  (let ((h (plist-get msg 'headers)))
    (string-append
      "From: " (or (plist-get h 'From) "") "\n"
      "Date: " (or (plist-get h 'Date) "") "\n"
      (let ((to (plist-get h 'To)))
        (if to (string-append "To: " to "\n") ""))
      "\n"
      (nm--attachment-text msg)
      (nm--msg-body-text msg)
      "\n")))

;; -> (text ((byte-offset id filename) ...))
(define (nm--render-text subject msgs)
  (if (null? msgs)
      (list "" '())
      (let loop ((ms msgs) (n 1)
                 (text (string-append subject "\n"))
                 (offsets '()))
        (if (null? ms)
            (list text (reverse offsets))
            (let ((header (string-append
                            "\n── message " (number->string n) " of "
                            (number->string (length msgs)) " ──\n")))
              (loop (cdr ms) (+ n 1)
                    (string-append text header (nm--msg-render (car ms)))
                    (cons (list (string-byte-length text)
                                (plist-get (car ms) 'id)
                                (plist-get (car ms) 'filename))
                          offsets)))))))

;; the whole thread as one HTML document for the sandboxed iframe:
;; our headers, their bodies (plain text becomes <pre>)
;; A reply carries the whole history it answers. The thread page folds
;; that history behind a toggle: everything from the first quote marker
;; on goes into a closed <details>. The text view and the mail tools keep
;; the full body.
(define nm--quote-tag-markers
  '("class=\"gmail_quote" "class=\"x_gmail_quote" "id=\"appendonsend"
    "id=\"x_appendonsend" "id=\"divRplyFwdMsg" "id=\"x_divRplyFwdMsg"
    "border-top:solid #E1E1E1" "border-top:solid #B5C4DF"
    "OLK_SRC_BODY_SECTION" "<blockquote type=\"cite\""))

(define nm--quote-text-markers
  '("-----Original Message-----" "-----Original message-----"))

(define (nm--last-before needle s end)
  (let loop ((from 0) (last #f))
    (let ((i (string-search needle s from)))
      (if (and i (< i end)) (loop (+ i 1) i) last))))

(define (nm--min-index xs)
  (fold (lambda (acc x) (if (and x (or (not acc) (< x acc))) x acc)) #f xs))

;; where the quoted history of an html body starts: the opening tag of
;; the element that holds the first marker, or #f
(define (nm--html-quote-start html)
  (nm--min-index
    (append
      (map (lambda (m)
             (let ((i (string-search m html)))
               (and i (nm--last-before "<" html (+ i 1)))))
           nm--quote-tag-markers)
      (map (lambda (m)
             (let ((i (string-search m html)))
               (and i (let ((p (nm--last-before "<p" html i))
                             (d (nm--last-before "<div" html i)))
                         (if (and p d) (max p d) (or p d))))))
           nm--quote-text-markers))))

;; #t when HTML shows no words: tags, style, script and spaces only
(define (nm--html-blank? html)
  (let* ((b (string-search "<body" html))
         (s (if b (substring html b (string-length html)) html))
         (n (string-length s)))
    (let loop ((i 0) (in-tag #f))
      (cond ((>= i n) #t)
            ((char=? (string-ref s i) #\<) (loop (+ i 1) #t))
            ((char=? (string-ref s i) #\>) (loop (+ i 1) #f))
            (in-tag (loop (+ i 1) #t))
            ((char-whitespace? (string-ref s i)) (loop (+ i 1) #f))
            ((and (char=? (string-ref s i) #\&)
                  (string-prefix? "&nbsp;" (substring s i (min n (+ i 6)))))
             (loop (+ i 6) #f))
            (else #f)))))

(define (nm--quote-details open? inner)
  (string-append "<details class=\"mail-quoted\"" (if open? " open" "") ">"
                 "<summary>··· quoted text</summary>" inner "</details>"))

(define (nm--fold-html-quote html)
  (let ((i (nm--html-quote-start html)))
    (if (not i)
        html
        (let ((head (substring html 0 i)))
          (string-append head
                         (nm--quote-details (nm--html-blank? head)
                                            (substring html i (string-length html))))))))

(define (nm--quote-line? line)
  (or (string-prefix? ">" line)
      (re-match? "^On .+ wrote:\\s*$" line)
      (string-prefix? "-----Original" line)
      (string-prefix? "________________________________" line)))

;; a text body as html, with the history from the first quote line folded
(define (nm--fold-text-quote text)
  (let loop ((ls (string-split text "\n")) (head '()))
    (cond ((null? ls)
           (html-escape text))
          ((nm--quote-line? (car ls))
           (let ((h (string-join (reverse head) "\n")))
             (string-append (html-escape h)
                            (nm--quote-details (string=? (string-trim h) "")
                                               (html-escape (string-join ls "\n"))))))
          (else (loop (cdr ls) (cons (car ls) head))))))

(define (nm--msg-html msg)
  (let* ((h (plist-get msg 'headers))
         (raw (nm--parts-html (plist-get msg 'body)))
         (html (and raw (nm--inline-cids msg raw)))
         (body (if html
                   (nm--fold-html-quote html)
                   (string-append "<pre style=\"white-space:pre-wrap;font:inherit\">"
                                  (nm--fold-text-quote (nm--parts-text (plist-get msg 'body)))
                                  "</pre>"))))
    ;; a newline before each message: it starts a line of its own, where
    ;; imenu's jump to the line lands inside it
    (string-append
      "\n<mail-message id=\"" (html-escape (nm--msg-anchor-name msg)) "\""
      " message-id=\"" (html-escape (or (plist-get msg 'id) "")) "\">"
      "<header><mail-from>" (html-escape (or (plist-get h 'From) "")) "</mail-from> · "
      "<mail-date>" (html-escape (or (plist-get h 'Date) "")) "</mail-date>"
      (let ((to (plist-get h 'To)))
        (if to (string-append " · to <mail-to>" (html-escape to) "</mail-to>") ""))
      (let ((cc (plist-get h 'Cc)))
        (if (and cc (not (equal? cc ""))) (string-append " · cc <mail-cc>" (html-escape cc) "</mail-cc>") ""))
      "</header><mail-body>" body "</mail-body>" (nm--attachment-html msg raw) "</mail-message>")))

(define (nm--msg-anchor-name msg)
  ;; a message's name in both views: the id of its <mail-message> in the
  ;; html page, and the anchor of its part in either
  (string-append "message:" (url-encode (or (plist-get msg 'id) ""))))

;; -> (HTML RANGES): the page, and each message's (START END), the bytes of
;; its <mail-message> element in HTML
(define (nm--thread-page subject msgs)
  (let* ((head (nm--thread-html-head subject))
         (parts (map nm--msg-html msgs))
         (ranges (let loop ((ps parts) (at (string-byte-length head)) (acc '()))
                   (if (null? ps)
                       (reverse acc)
                       (let ((n (string-byte-length (car ps))))
                         ;; each part opens with its newline; the element follows it
                         (loop (cdr ps) (+ at n) (cons (list (+ at 1) (+ at n)) acc)))))))
    (list (string-append head (apply string-append parts) "</mail-thread></body></html>")
          ranges)))

(define (nm--thread-html-head subject)
  (string-append
    "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>"
    (html-escape subject)
    "</title><style>mail-thread,mail-message,mail-body,mail-attachments,mail-attachment{display:block}"
    ;; each message is a card; the rules mix the text colour, so they follow the theme
    "mail-thread{max-width:760px}"
    "mail-message{margin:14px 0;padding:14px 18px;border:1px solid color-mix(in srgb,currentColor 14%,transparent);border-radius:10px}"
    ;; the header: sender left, date right, recipients below. font-size:0 drops
    ;; the bare ' · ' separators the markup carries for the text tools
    "mail-message>header{display:grid;grid-template-columns:1fr auto;gap:2px 12px;align-items:baseline;font-size:0;"
    "padding:0 0 10px;margin:0 0 12px;border-bottom:1px solid color-mix(in srgb,currentColor 10%,transparent)}"
    "mail-from{font:600 14px system-ui;color:var(--default-fg,#141310);overflow-wrap:anywhere}"
    "mail-date{grid-column:2;grid-row:1;font:12px system-ui;color:#888;white-space:nowrap}"
    "mail-to{grid-column:1/-1;font:12px system-ui;color:#888;overflow-wrap:anywhere}"
    "mail-cc{grid-column:1/-1;font:12px system-ui;color:#888;overflow-wrap:anywhere}"
    "mail-to::before{content:'to '}mail-cc::before{content:'cc '}"
    ;; the message at point: anchor-mode's part, marked by the preview
    "mail-message{transition:border-color .15s,box-shadow .15s,background-color .15s}"
    ;; literal colours: the page's --accent-fg is empty, and an empty var
    ;; drops the whole declaration instead of taking its fallback
    "mail-message.anchor-current{border-color:#2f6fd6;background-color:rgba(47,111,214,.07);"
    "box-shadow:inset 4px 0 0 #2f6fd6,0 0 0 3px rgba(47,111,214,.22),0 6px 18px rgba(47,111,214,.18)}"
    "mail-body{font-size:14px;line-height:1.55;overflow-wrap:anywhere}"
    "mail-body img{max-width:100%;height:auto}"
    ;; a newsletter lays itself out in fixed-width tables: every box in the
    ;; body fits the card, and what still will not scrolls inside the card
    "mail-body{overflow-x:auto;min-width:0}"
    "mail-body table,mail-body tbody,mail-body tr,mail-body td,mail-body th,mail-body div,mail-body center{max-width:100%!important;box-sizing:border-box}"
    "mail-body td,mail-body th{overflow-wrap:anywhere}"
    "mail-body pre{margin:0}"
    "details.mail-quoted{margin:10px 0 0}"
    "details.mail-quoted>summary{display:inline-block;cursor:pointer;color:#888;font:12px system-ui;list-style:none;"
    "padding:1px 10px;border-radius:999px;background:color-mix(in srgb,currentColor 7%,transparent)}"
    "details.mail-quoted[open]>summary{margin-bottom:8px}"
    "details.mail-quoted blockquote,details.mail-quoted .gmail_attr{opacity:.75}"
    "mail-attachments{margin:14px 0 0;padding:10px 12px;border:1px solid color-mix(in srgb,currentColor 14%,transparent);border-radius:8px;font:12px system-ui}"
    "mail-attachments>strong{display:block;margin-bottom:8px}"
    "mail-attachments>small{display:block;margin-top:8px;color:#888}"
    "mail-attachment{display:inline-block;vertical-align:top;max-width:160px;margin:0 10px 6px 0;word-break:break-all}"
    "mail-attachment img{display:block;max-width:160px;max-height:160px;border-radius:4px;margin-bottom:4px}"
    ;; a file that is not an image is its icon, then its name, which wraps
    ;; only where the pane runs out of room
    "mail-attachment:has(.mail-file-icon){display:inline-flex;align-items:center;gap:6px;max-width:100%;word-break:normal;overflow-wrap:anywhere}"
    "i.mail-file-icon{flex:none;font:normal 22px/1 'Symbols Nerd Font Mono','JetBrainsMonoNL Nerd Font Mono','JetBrainsMono Nerd Font Mono','Hack Nerd Font Mono';color:#a83a2b}"
    ;; the title of the thread, and it reads as one: bold, full contrast,
    ;; and clear of the first message card
    "mail-subject{display:block;font:700 21px/1.3 system-ui;"
    "color:var(--default-fg,#141310);margin:0 0 4px}</style></head>"
    "<body style=\"margin:14px;font-family:system-ui\"><mail-thread>"
    "<mail-subject>" (html-escape subject) "</mail-subject>"))

;; The plain-text view keeps the text buffer's message offsets for commands,
;; but renders records rather than asking the client to infer mail from lines.
(define (nm--msg-composml msg)
  (let ((h (plist-get msg 'headers)))
    (list 'tag "mail-message" 'class "semantic-document-section"
          'attrs (list (list "message-id" (or (plist-get msg 'id) "")))
          'children
          (list (nm--field "mail-from" (or (plist-get h 'From) ""))
                (nm--field "mail-date" (or (plist-get h 'Date) ""))
                (nm--field "mail-to" (or (plist-get h 'To) ""))
                (list 'tag "mail-attachments" 'children
                      (map (lambda (part)
                             (list 'tag "mail-attachment"
                                   'attrs (list (list "part-id" (plist-get part 'id))
                                                (list "content-type" (or (plist-get part 'content-type) "")))
                                   'text (plist-get part 'filename)))
                           (nm--listed-attachment-parts msg)))
                (list 'tag "mail-body" 'children
                      (list (list 'tag "pre" 'text (nm--msg-body-text msg))))))))

(define (nm--thread-composml! buf subject msgs offsets)
  (desktop-skip! buf 'render-blocks)
  (buffer-set-local! buf 'render-blocks
    (list (list 'tag "mail-thread" 'class "semantic-document"
                'attrs (list (list "record-id" (buffer-local-value 'notmuch-thread buf)))
                'children (cons (component 'ui/section (list 'title subject 'level 1))
                                (let loop ((ms msgs) (offsets offsets) (out '()))
                                  (if (null? ms) (reverse out)
                                    (let* ((start (car (car offsets)))
                                           (stop (if (pair? (cdr offsets)) (car (cadr offsets)) (buffer-size buf)))
                                           (first (length (string-split (substring-bytes (buffer-text buf) 0 start) "\n")))
                                           (last (length (string-split (substring-bytes (buffer-text buf) 0 stop) "\n"))))
                                      (loop (cdr ms) (cdr offsets)
                                        (cons (append (list 'anchor (string-append "message:" (url-encode (or (plist-get (car ms) 'id) "")))
                                                            'lines (list first (max first (- last 1)))
                                                            'mark "current-message")
                                                      (nm--msg-composml (car ms))) out)))))))))
  (buffer-set-local! buf 'render-mode "blocks"))

(define (nm--any-html? msgs)
  (let loop ((ms msgs))
    (cond ((null? ms) #f)
          ((nm--parts-html (plist-get (car ms) 'body)) #t)
          (else (loop (cdr ms))))))

;; Each message is an anchor over its bytes, in either view, named by
;; nm--msg-anchor-name. imenu lists them as "3. Sender · date" and jumps
;; by moving point; anchor-mode says which anchor holds point, and the html
;; preview scrolls to the element of that id.
(define (nm--imenu-sender from)
  (let ((name (string-trim (car (string-split from "<")))))
    (string-trim (re-replace-all "\"" (if (equal? name "") from name) ""))))

(define (nm--imenu-date date)
  ;; "Mon, 19 Oct 2026 23:40:22 +0530" -> "19 Oct 2026 23:40"
  (let* ((s (let ((p (string-split date ", "))) (if (pair? (cdr p)) (cadr p) date)))
         (w (string-split s " ")))
    (if (>= (length w) 4)
        (string-append (list-ref w 0) " " (list-ref w 1) " " (list-ref w 2) " "
                       (let ((t (list-ref w 3))) (if (> (string-length t) 5) (substring t 0 5) t)))
        date)))

;; RANGES are the (START END) of each of MSGS in BUF's text
(define (nm--anchor-thread! buf msgs ranges)
  (anchor-index! buf
    (let loop ((ms msgs) (rs ranges) (i 1) (acc '()))
      (if (or (null? ms) (null? rs))
          (reverse acc)
          (let* ((h (plist-get (car ms) 'headers))
                 (from (or (plist-get h 'From) ""))
                 (date (or (plist-get h 'Date) "")))
            (loop (cdr ms) (cdr rs) (+ i 1)
                  (cons (list (nm--msg-anchor-name (car ms)) (car (car rs)) (cadr (car rs)) #f
                              (list 'kind "message"
                                    'title (string-append (number->string i) ". " (nm--imenu-sender from)
                                                          " · " (nm--imenu-date date))
                                    'doc (string-append from " · " date)))
                        acc))))))
  (buffer-set-local! buf 'anchor-imenu-index 'nm--show-imenu-index)
  (unless (minor-mode-on? buf "anchor-mode") (enable-minor-mode! buf "anchor-mode")))

;; imenu's rows, data only: (LINE KIND TITLE DOC)
(define (nm--show-imenu-index buf)
  (map (lambda (a)
         (list (with-current-buffer buf (lambda () (line-number-at-pos (anchor-start a))))
               "message" (plist-get (anchor-data a) 'title) (plist-get (anchor-data a) 'doc)))
       (anchor-list buf)))

;; the html page into BUF: its text, the messages' real offsets as
;; notmuch-msgs (START ID FILENAME), and their anchors
(define (nm--show-html! buf subject msgs)
  (let* ((page (nm--thread-page subject msgs))
         (ranges (cadr page)))
    (buffer-delete-range! buf 0 (buffer-size buf))
    (buffer-append! buf (car page))
    (buffer-set-local! buf 'notmuch-msgs
      (let loop ((ms msgs) (rs ranges) (acc '()))
        (if (or (null? ms) (null? rs))
            (reverse acc)
            (loop (cdr ms) (cdr rs)
                  (cons (list (car (car rs)) (plist-get (car ms) 'id) (plist-get (car ms) 'filename)) acc)))))
    (nm--anchor-thread! buf msgs ranges)))

;; the text view's offsets are where each message's header line begins,
;; less its newline; a message runs to the next one
(define (nm--text-ranges offsets size)
  (let loop ((os offsets) (acc '()))
    (if (null? os)
        (reverse acc)
        (loop (cdr os)
              (cons (list (+ (car (car os)) 1) (if (pair? (cdr os)) (car (cadr os)) size)) acc)))))

(mode-doc! "notmuch-show-mode"
  "One mail thread, read. `a` archives it, `L` files it for the agent (+liked -inbox) and `r` starts a reply. `C-c a` opens an attachment. `v` changes between HTML and plain text. imenu (`M-g i`) jumps to a message of the thread. `q` goes back to the search.")

(mode-icon! "notmuch-show-mode" "")

;; THUNK now when a window shows BUF, else once a window does. Not
;; buffer-when-shown!: it counts the current buffer as shown, and a mode
;; hook always runs with its page current, hidden or not.
(define (nm--when-shown! buf thunk)
  (if (buffer-shown? buf)
      (thunk)
      (buffer-request-refresh! buf 'nm-render thunk)))

(define-mode "notmuch-show-mode"
  (lambda ()
    (let ((buf (current-buffer)))
      (buffer-set-read-only! buf #t)
      ;; a mail's buttons and links need the reader's login and cookies:
      ;; they open in the group's own browser window, not as fetched text
      (buffer-set-local! buf 'preview-web-links 'browser)
      (when (buffer-known? *notmuch-search-buffer*)
        (buffer-child! *notmuch-search-buffer* buf))
      (let ((th (buffer-local-value 'notmuch-thread buf)))
        ;; not one instruction for a hidden page: the fetch and the render
        ;; wait until a window shows it (a restored desktop holds many)
        (when th
          (nm--when-shown! buf (lambda () (with-current-buffer buf (lambda ()
          (let* ((subject (or (buffer-local-value 'notmuch-subject buf) ""))
                 (msgs (nm--show-msgs th))
                 (html? (and notmuch-prefer-html
                             (not (equal? (buffer-local-value 'notmuch-view buf) "text")))))
            (desktop-skip! buf 'notmuch-attachments)
            (desktop-skip! buf 'nm-attachment-source)
            (buffer-set-local! buf 'notmuch-attachments (nm--attachment-options msgs))
            (buffer-set-local! buf 'nm-attachment-source (nm--db-key))
            (buffer-set-local! buf 'notmuch-msg-heads
              (map (lambda (m) (let ((h (plist-get m 'headers)))
                                 (list (or (plist-get h 'From) "") (or (plist-get h 'Date) ""))))
                   msgs))
            (buffer-set-local! buf 'imenu-index-function 'nm--show-imenu-rows)
            (buffer-delete-range! buf 0 (buffer-size buf))
            (if html?
                (begin
                  (buffer-set-local! buf 'render-mode "html")
                  (nm--show-html! buf subject msgs)
                  ;; image attachments draw as thumbnails once they are made
                  (nm--make-thumbs! msgs
                    (lambda ()
                      (when (and (buffer-known? buf)
                                 (equal? (buffer-local-value 'notmuch-thread buf) th)
                                 (equal? (buffer-local-value 'render-mode buf) "html"))
                        (nm--show-html! buf subject msgs))))
                  ;; authored colors assume a white canvas — by default the
                  ;; theme repaints the document instead (dark mode stays
                  ;; readable); customize notmuch-html-original-colors to
                  ;; get the untouched rendering back
                  (buffer-set-local! buf 'preview-authored
                    notmuch-html-original-colors))
                (let ((rendered (nm--render-text subject msgs)))
                  (buffer-append! buf (car rendered))
                  (nm--thread-composml! buf subject msgs (cadr rendered))
                  (buffer-set-local! buf 'notmuch-msgs (cadr rendered))
                  (nm--anchor-thread! buf msgs
                    (nm--text-ranges (cadr rendered) (string-byte-length (car rendered))))))
            (goto-char! 0)))))))))))

(mode-keys! "notmuch-show-mode"
  '(
    ("a" "notmuch-show-archive")
    ("r" "notmuch-show-reply")
    ("v" "notmuch-show-toggle-view")
    ("L" "notmuch-like")
    ("j" "notmuch-jump")
    ("C-c a" "notmuch-open-attachment")
    ("A" "notmuch-open-attachment")
    ("q" "notmuch-show-quit")))

;; ONE show buffer, reused — it is a view, not a document. The subject
;; lives in the modeline; 'special says so (the mode re-renders from
;; 'notmuch-thread on restore).
(define (nm--show-buffer? b)
  (equal? (buffer-local-value 'mode-name b) "notmuch-show-mode"))

(define (nm--show-buffers) (filter nm--show-buffer? (buffer-list-mru)))

;; the page of a thread in the selected database, or #f
(define (nm--thread-buffer thread-id)
  (and thread-id
       (find (lambda (b) (and (equal? (buffer-local-value 'notmuch-thread b) thread-id)
                              (equal? (buffer-local-value 'nm-db b) (nm--db-key))))
             (nm--show-buffers))))

;; the page on screen, else the one read last
(define (nm--show-buffer)
  (let ((all (nm--show-buffers)))
    (or (find window-showing all) (and (pair? all) (car all)))))

(define (nm--thread-buffer-name subject)
  (let* ((s (if (equal? subject "") "(no subject)" subject))
         (base (string-append "*mail: "
                              (if (> (string-length s) 60) (string-append (substring s 0 60) "...") s)
                              "*")))
    (let loop ((n 1))
      (let ((name (if (= n 1) base (string-append base "<" (number->string n) ">"))))
        (if (buffer-known? name) (loop (+ n 1)) name)))))

;; a page read this recently is drawn from what it holds
(define nm--page-fresh-ms 60000)

(define (nm--open-thread! thread-id subject &rest opts)
  ;; One page per thread, like every app's rows: C-` walks the threads read
  ;; from this list, and going back to one draws it at once.
  (let* ((old (nm--thread-buffer thread-id))
         (buf (or old (let ((name (nm--thread-buffer-name subject))) (buffer-create name) name)))
         (read-at (buffer-local-value 'nm-read-at buf))
         (fresh? (and old (number? read-at) (< (- (monotonic-ms) read-at) nm--page-fresh-ms))))
    (buffer-set-local! buf 'notmuch-thread thread-id)
    (buffer-set-local! buf 'notmuch-subject subject)
    (buffer-set-local! buf 'nm-db (nm--db-key))
    ;; the thread view is the same app as the index, so it carries the same
    ;; app id and joins the group the index is in. Membership is 'group-id
    ;; and joining is buffer-add-group!: writing the legacy 'group local here
    ;; left the view ungrouped, because the reader clears that local the
    ;; moment a buffer has a real group.
    (nm--join-group! buf 'detail)
    ;; render into the buffer and into no window at all. Placement belongs
    ;; to the caller (nm--show-pane!): a switch here takes whichever window
    ;; happens to be current, which is the index's own window.
    (unless fresh?
      (with-current-buffer buf (lambda () (set-mode! "notmuch-show-mode")))
      (buffer-set-local! buf 'nm-read-at (monotonic-ms)))
    ;; reading marks read, like every mail client
    (when (null? opts)
      (nm--op 'tag! "-unread" (string-append "thread:" thread-id)))
    buf))

(define-command "notmuch-open-thread" "Open the thread at point beside the index and go to it"
  (lambda ()
    (let ((buf (current-buffer)))
      (if (nm--thread-at buf)
          ;; The thread never takes the index's window: it opens beside the
          ;; list exactly as a move previews it. RET then goes to that page,
          ;; so `a`, `r` and `v` act on the thread that was opened.
          (let ((id (nm--id-at buf)))
            (nm--preview! buf)
            (let* ((page (nm--thread-buffer id))
                   (pane (and page (window-showing page))))
              (when pane (select-window! pane))))
          (message "No thread on this line"))))
  'modes '(notmuch-mode))

(define-command "notmuch-show-toggle-view" "Switch between the HTML and text views"
  (lambda ()
    (let ((buf (current-buffer)))
      (buffer-set-local! buf 'notmuch-view
        (if (equal? (buffer-local-value 'notmuch-view buf) "text") "html" "text"))
      (set-mode! "notmuch-show-mode")))
  'modes '(notmuch-show-mode))

(define-command "notmuch-show-quit" "Close this thread's pane; it stays closed until you move to another thread"
  (lambda ()
    (set! *nm-dismissed* (or (and (buffer-exists? *notmuch-search-buffer*)
                                  (nm--id-at *notmuch-search-buffer*))
                             (buffer-local-value 'notmuch-thread (current-buffer))))
    (run-command "quit-window"))
  'modes '(notmuch-show-mode))

(define-command "notmuch-show-archive" "Archive this thread and go back"
  (lambda ()
    (let ((th (buffer-local-value 'notmuch-thread (current-buffer))))
      (when th
        (nm--op 'tag! "-inbox" (string-append "thread:" th))
        (run-command "quit-window")
        (when (buffer-exists? *notmuch-search-buffer*)
          (nm--refresh! *notmuch-search-buffer*))
        (message "Archived"))))
  'modes '(notmuch-show-mode))

;; the message the point is in: last offset <= point (above the first
;; message — on the subject line — it means the first message)
(define (nm--msg-at buf)
  (let ((ms (or (buffer-local-value 'notmuch-msgs buf) '())))
    (let loop ((rest ms) (found (if (null? ms) #f (car ms))))
      (cond ((null? rest) found)
            ((<= (car (car rest)) (buffer-point buf)) (loop (cdr rest) (car rest)))
            (else found)))))

;;; --- compose / reply ------------------------------------------------------------

(mode-icon! "mail-compose-mode" "")

(define-mode "mail-compose-mode"
  (lambda ()
    ))

(mode-keys! "mail-compose-mode"
  '(
    ("C-c C-c" "mail-send")
    ("C-c C-k" "mail-abort")))

(mode-doc! "mail-compose-mode"
  "A message you are writing. The headers sit above the separator line and the body below it. `C-c C-c` sends the message, and `C-c C-k` abandons it.")

;; message-mode layout: headers, the separator, an empty line for the
;; reply (point lands there), attribution, the original quoted as text
(define *mail-header-separator* "--text follows this line--")

(defface! 'nm-hdr 'fg "#26356b" 'weight "600")
(defface! 'nm-sep 'fg "#9a9a72")

(define (nm--quote-text text)
  (string-append "> "
    (string-join (string-split (string-trim text) "\n") "\n> ")
    "\n"))

;; face the header names and the separator — they sit above point, so
;; typing in the body never shifts them
(define (nm--compose-overlays! buf head)
  (let loop ((lines (string-split head "\n")) (off 0) (ovs '()))
    (if (null? lines)
        (overlay-set! buf 'compose (reverse ovs))
        (let* ((line (car lines))
               (len (string-byte-length line))
               (parts (string-split line ": ")))
          (loop (cdr lines) (+ off len 1)
                (cond ((equal? line *mail-header-separator*)
                       (cons (list off (+ off len) "nm-sep") ovs))
                      ((and (> len 0) (pair? (cdr parts)))
                       (cons (list off (+ off (string-byte-length (car parts)) 1) "nm-hdr")
                             ovs))
                      (else ovs)))))))

(define (nm--compose-reply! msg-id)
  (let* ((j (nm--op 'reply msg-id))
         (rh (and j (plist-get j 'reply-headers)))
         (orig (and j (plist-get j 'original))))
    (if (not rh)
        (message "notmuch reply failed")
        (let* ((buf "*compose*")
               (hdr (lambda (name key)
                      (let ((v (plist-get rh key)))
                        (if v (string-append name ": " v "\n") ""))))
               (head (string-append
                       (hdr "From" 'From) (hdr "To" 'To) (hdr "Cc" 'Cc)
                       (hdr "Subject" 'Subject)
                       (hdr "In-Reply-To" 'In-reply-to)
                       (hdr "References" 'References)
                       *mail-header-separator* "\n"))
               (attrib (let ((h (and orig (plist-get orig 'headers))))
                         (if (and h (plist-get h 'From))
                             (string-append (plist-get h 'From) " writes:\n\n")
                             "")))
               ;; quote the RENDERED text (nm--msg-body-text goes through the
               ;; html renderer when there is no text/plain) — never raw html
               (quoted (if orig (nm--quote-text (nm--msg-body-text orig)) "")))
          (unless (buffer-exists? buf) (buffer-create buf))
          (buffer-delete-range! buf 0 (buffer-size buf))
          (buffer-append! buf (string-append head "\n" attrib quoted))
          (switch-to-buffer! buf)
          (set-mode! "mail-compose-mode")
          (nm--compose-overlays! buf head)
          (goto-char! (string-byte-length head))
          (message "C-c C-c sends, C-c C-k aborts")))))

(define-command "notmuch-show-reply" "Reply to the message at point"
  (lambda ()
    (let ((msg (nm--msg-at (current-buffer))))
      (if msg
          (nm--compose-reply! (cadr msg))
          (message "No message at point"))))
  'modes '(notmuch-show-mode))

;; newest message id of a thread (search sorts newest-first)
(define (nm--newest-msg-id thread-id)
  (nm--op 'newest-message thread-id))

(define-command "notmuch-reply" "Reply to the newest message of the thread at point"
  (lambda ()
    (let ((th (nm--thread-at (current-buffer))))
      (if (not th)
          (message "No thread on this line")
          (let ((id (nm--newest-msg-id (nm--th-id th))))
            (if id
                (nm--compose-reply! id)
                (message "No message found in thread"))))))
  'modes '(notmuch-mode))

(define (nm--from-header text)
  "The From address of a composed message, read from the header block alone."
  (let loop ((lines (string-split text "\n")))
    (cond ((null? lines) "")
          ((equal? (string-trim (car lines)) *mail-header-separator*) "")
          ((equal? (string-trim (car lines)) "") "")
          ((string-prefix? "From:" (car lines))
           (string-trim (substring (car lines) 5 (string-length (car lines)))))
          (else (loop (cdr lines))))))

(define (nm--send-route text)
  "The send command for TEXT, matched against its From address only. A route
key of \"\" matches anything, so it belongs last. Matching the whole message
would let a word in the body pick the account the mail goes out from."
  (let ((from (nm--from-header text)))
    (let loop ((rs notmuch-send-routes))
      (cond ((null? rs) #f)
            ((or (equal? (car (car rs)) "")
                 (string-contains? from (car (car rs))))
             (cadr (car rs)))
            (else (loop (cdr rs)))))))

(define (nm--mime-type path)
  "The MIME type of PATH, from its extension; application/octet-stream when unknown."
  (let* ((m (re-match "\\.([A-Za-z0-9]+)$" path))
         (ext (if m (string-downcase (cadr m)) "")))
    (cond ((equal? ext "pdf") "application/pdf")
          ((member ext '("png" "gif" "webp")) (string-append "image/" ext))
          ((member ext '("jpg" "jpeg")) "image/jpeg")
          ((member ext '("txt" "md" "org")) "text/plain")
          ((equal? ext "csv") "text/csv")
          ((equal? ext "html") "text/html")
          ((equal? ext "zip") "application/zip")
          ((equal? ext "xlsx") "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
          ((equal? ext "docx") "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
          (else "application/octet-stream"))))

(define (nm--base64-lines s)
  "S as base64, broken into lines of 76."
  (let* ((b (base64-encode s)) (n (string-length b)))
    (let loop ((i 0) (acc '()))
      (if (>= i n)
          (string-join (reverse acc) "\n")
          (loop (+ i 76) (cons (substring b i (min n (+ i 76))) acc))))))

(define (nm--header-word s)
  "S as a header value: as it is when ASCII, else an RFC 2047 encoded word."
  (if (= (string-length s) (string-byte-length s))
      s
      (string-append "=?UTF-8?B?" (base64-encode s) "?=")))

(define (nm--default-from)
  "The From of the mailbox notmuch reads: its user.name and user.primary_email."
  (let ((name (string-trim (nm--op 'config "user.name")))
        (email (string-trim (nm--op 'config "user.primary_email"))))
    (if (equal? name "") email (string-append name " <" email ">"))))

(define (mail-message headers body &optional attachments)
  "(mail-message HEADERS BODY [ATTACHMENTS]) — the RFC 822 text of a mail.
HEADERS is a plist of from, to, cc, bcc, subject, in-reply-to and references;
from defaults to the mailbox's own address. BODY is plain text. Each of
ATTACHMENTS is a file path, or (PATH NAME) to send it under another name."
  (let* ((from (or (plist-get headers 'from) (nm--default-from)))
         (head (lambda (name key)
                 (let ((v (plist-get headers key)))
                   (if (and v (not (equal? v "")))
                       (string-append name ": " (nm--header-word v) "\n")
                       ""))))
         (top (string-append "From: " (nm--header-word from) "\n"
                             (head "To" 'to) (head "Cc" 'cc) (head "Bcc" 'bcc)
                             (head "Subject" 'subject)
                             (head "In-Reply-To" 'in-reply-to) (head "References" 'references)
                             "MIME-Version: 1.0\n"))
         (files (or attachments '())))
    (if (null? files)
        (string-append top "Content-Type: text/plain; charset=UTF-8\n"
                       "Content-Transfer-Encoding: 8bit\n\n" body "\n")
        (let ((boundary (string-append "compos-" (number->string (abs (monotonic-ms)))
                                       "-" (number->string (random 1000000)))))
          (string-append
            top "Content-Type: multipart/mixed; boundary=\"" boundary "\"\n\n"
            "--" boundary "\n"
            "Content-Type: text/plain; charset=UTF-8\n"
            "Content-Transfer-Encoding: 8bit\n\n" body "\n"
            (apply string-append
                   (map (lambda (a)
                          (let* ((path (expand-path (if (pair? a) (car a) a)))
                                 (name (if (pair? a) (cadr a) (file-name-nondirectory path))))
                            (if (not (file-exists? path))
                                (error (string-append "mail-message: no file " path)))
                            (string-append
                              "\n--" boundary "\n"
                              "Content-Type: " (nm--mime-type name) "; name=\"" name "\"\n"
                              "Content-Disposition: attachment; filename=\"" name "\"\n"
                              "Content-Transfer-Encoding: base64\n\n"
                              (nm--base64-lines (read-file path)) "\n")))
                        files))
            "\n--" boundary "--\n")))))

;; The mailbox of notmuch-mailboxes whose backend sends mail itself and
;; whose address is TEXT's From, or #f.
(define (nm--send-mailbox text)
  (let ((from (string-downcase (nm--from-header text))))
    (let loop ((ms (map nm--entry->pair notmuch-mailboxes)))
      (if (null? ms)
          #f
          (let ((mb (nm--pair->mailbox (car (car ms)) (cadr (car ms)))))
            (if (and (symbol? (car mb))
                     (nm--backend-has? 'send mb)
                     (string-contains? from (string-downcase (cadr mb))))
                mb
                (loop (cdr ms))))))))

(define (mail-send-text! text)
  "(mail-send-text! TEXT) — send the RFC 822 TEXT through the mailbox or route its From picks; #t when sent, else the error as a string"
  (let ((mb (nm--send-mailbox text)))
    (if mb
        (nm--op-on mb 'send text)
        (nm--send-by-route text))))

(define (nm--send-by-route text)
  (let ((route (nm--send-route text))
        (tmp (string-append (expand-path "~") "/.compos/outgoing.eml")))
    (if (not route)
        "No send route matches — set notmuch-send-routes"
        (begin
          (write-file! tmp text)
          (let ((out (shell-command->string
                       (string-append "cat " (sh-quote tmp) " | "
                                      (nm--host-cmd (string-append route " && echo SENT-OK"))))))
            (delete-file! tmp)
            (if (string-contains? out "SENT-OK") #t (string-trim out)))))))

(define (mail-send! headers body &optional attachments)
  "(mail-send! HEADERS BODY [ATTACHMENTS]) — build the mail as mail-message does and send it; #t when sent, else the error as a string"
  (mail-send-text! (mail-message headers body attachments)))

(define (mail-reply! target body &optional opts)
  "(mail-reply! MSG-ID BODY [OPTS]) — reply to the message MSG-ID (id: and <> optional) with BODY, in its thread. It finds the mailbox that holds MSG-ID (the current one, then each of mail-feed-profiles), and notmuch fills From, To, Cc, Subject, In-Reply-To and References from it, so the reply goes out from the address the mail came to; the From picks the send route. OPTS: 'sender-only #t answers the sender alone (default all), 'cc adds addresses, 'no-quote #t leaves the original out, 'mailbox (HOST PROFILE) skips the search, 'dry-run #t answers the RFC 822 text instead of sending. #t when sent, else the error as a string"
  (let* ((t0 (string-trim target))
         (t1 (if (string-prefix? "id:" t0) (substring t0 3 (string-length t0)) t0))
         (id (if (and (string-prefix? "<" t1) (string-suffix? ">" t1)) (substring t1 1 (- (string-length t1) 1)) t1))
         (holds? (lambda (mb) (let ((n (string-trim (nm--run (string-append "count -- id:" (sh-quote id)) mb))))
                                (and (not (equal? n "")) (not (equal? n "0")) (string->number n)))))
         (mb (or (plist-get opts 'mailbox)
                 (let loop ((ms (cons (list notmuch-host notmuch-profile) (map mail-feed--mailbox mail-feed-profiles))))
                   (cond ((null? ms) #f) ((holds? (car ms)) (car ms)) (else (loop (cdr ms)))))))
         (j (and mb (json-parse (nm--run (string-append "reply --format=json --reply-to="
                                                        (if (plist-get opts 'sender-only) "sender" "all")
                                                        " id:" (sh-quote id)) mb))))
         (rh (and j (plist-get j 'reply-headers)))
         (orig (and j (plist-get j 'original))))
    (if (not rh)
        (string-append "mail-reply!: no mailbox holds message " id)
        (let* ((extra (plist-get opts 'cc))
               (cc0 (plist-get rh 'Cc))
               (cc (if (and cc0 extra) (string-append cc0 ", " extra) (or cc0 extra)))
               (oh (and orig (plist-get orig 'headers)))
               (quoted (if (or (not orig) (plist-get opts 'no-quote)) ""
                           (string-append "\n\n" (if (and oh (plist-get oh 'From)) (string-append (plist-get oh 'From) " writes:\n\n") "")
                                          (nm--quote-text (nm--msg-body-text orig)))))
               (msg (mail-message (list 'from (plist-get rh 'From) 'to (plist-get rh 'To) 'cc cc
                                        'subject (plist-get rh 'Subject)
                                        'in-reply-to (plist-get rh 'In-reply-to)
                                        'references (plist-get rh 'References))
                                  (string-append body quoted))))
          (if (plist-get opts 'dry-run) msg (mail-send-text! msg))))))

(define-command "mail-send" "Send this buffer as an email"
  (lambda ()
    ;; the separator line becomes the RFC822 blank line
    (let* ((text (string-join
                   (string-split (buffer-text (current-buffer))
                                 (string-append "\n" *mail-header-separator* "\n"))
                   "\n\n"))
           (sent (mail-send-text! text)))
      (if (equal? sent #t)
          (begin (run-command "quit-window") (message "Sent"))
          (message (string-append "Send failed: " sent)))))
  'modes '(mail-compose-mode))

(define-command "mail-abort" "Abandon this compose buffer"
  (lambda ()
    (let* ((buf (current-buffer))
           (others (filter (lambda (b) (not (equal? b buf))) (buffer-list-mru))))
      (buffer-kill! buf)
      (switch-to-buffer! (if (null? others) "*scratch*" (car others)))
      (message "Aborted")))
  'modes '(mail-compose-mode))

;;; --- targets & actions: the email at point, embark-style -------------------------

;; C-. and the model's act tool both land here — one real tag-and-verify
;; (mail-tag!), not a second copy that discards nm--run's outcome the way
;; this used to (echoing CHANGES back via message regardless of whether
;; anything was actually tagged)
(define (nm--email-tag-act changes)
  (lambda (id)
    (let ((result (mail-tag! id changes)))
      (message result)
      result)))

(register-target-provider! "notmuch-mode"
  (lambda (buf)
    (let ((th (nm--thread-at buf)))
      (and th (list 'email (nm--th-id th) (nm--th-subject th))))))

(register-target-provider! "notmuch-show-mode"
  (lambda (buf)
    (let ((th (buffer-local-value 'notmuch-thread buf)))
      (and th (list 'email th (or (buffer-local-value 'notmuch-subject buf) ""))))))

(register-actions! 'email
  (list (list "archive"  (nm--email-tag-act "-inbox"))
        (list "trash"    (nm--email-tag-act "+trash -inbox -unread"))
        (list "unread"   (nm--email-tag-act "+unread"))
        (list "mark"     (lambda (id)
                           (nm--toggle-selection! *notmuch-search-buffer* id)))
        (list "read"     (lambda (id)
                           (nm--show-pane!
                             (nm--open-thread! id
                               (let ((th (nm--thread-at (current-buffer))))
                                 (if th (nm--th-subject th) ""))))))
        (list "reply"    (lambda (id)
                           (let ((mid (nm--newest-msg-id id)))
                             (if mid
                                 (nm--compose-reply! mid)
                                 (message "no message in thread")))))))

;;; --- context: "this" in a chat means the selected email --------------------------

(register-context-provider! "notmuch-mode"
  (lambda (buf)
    (let ((th (nm--thread-at buf)))
      (and th
           (string-append "the email thread selected in the mail list: \""
                          (nm--th-subject th) "\" from " (nm--th-authors th)
                          " (notmuch thread:" (nm--th-id th) ")")))))

(register-context-provider! "notmuch-show-mode"
  (lambda (buf)
    (let ((th (buffer-local-value 'notmuch-thread buf))
          (msg (nm--msg-at buf)))
      (and th
           (string-append "the open email thread \""
                          (or (buffer-local-value 'notmuch-subject buf) "") "\""
                          " (notmuch thread:" th ")"
                          (if msg (string-append ", message id:" (cadr msg)) ""))))))

;; a mail row can carry a task: the thread is its source, and the task's
;; page opens the thread beside it
(define (nm--todo-item buf)
  (let ((th (nm--thread-at buf)))
    (and th
         (list 'title (nm--th-subject th)
               'source (string-append "mail:thread:" (nm--th-id th))
               'notes (string-append "Mail from " (nm--th-authors th) ": \"" (nm--th-subject th) "\"")))))

(define (nm--todo-item-show buf)
  (let ((th (buffer-local-value 'notmuch-thread buf))
        (subject (or (buffer-local-value 'notmuch-subject buf) "")))
    (and th
         (list 'title subject
               'source (string-append "mail:thread:" th)
               'notes (string-append "Mail: \"" subject "\"")))))

(define (nm--todo-source-open! key)
  (let* ((id (substring key 12 (string-length key)))
         (row (string-split (string-trim (mail-search (string-append "thread:" id))) " | "))
         (subject (if (>= (length row) 3) (list-ref row 2) "mail")))
    (display-buffer-other-window! (nm--open-thread! id subject 'keep-unread))))

(define (nm--register-todo-items!)
  ;; older todo packages only; the current one has neither call
  (when (boundp 'todo-item-register!)
    (todo-item-register! "notmuch-mode" nm--todo-item)
    (todo-item-register! "notmuch-show-mode" nm--todo-item-show))
  (when (boundp 'todo-source-opener-register!)
    (todo-source-opener-register! "mail:thread:" nm--todo-source-open!)))
(if (featurep 'todo) (nm--register-todo-items!) (with-eval-after-load 'todo nm--register-todo-items!))

;;; --- mail for the model -------------------------------------------------------
;;; No per-domain tools: mail is reached through eval-scheme + act. Search
;;; and read are public functions over the same code the UI uses.

(define (mail-search query)
  (let ((threads (nm--search-json query 20)))
    (if (null? threads)
        "no matches"
        (fold (lambda (acc th)
                (string-append acc
                  (plist-get th 'date_relative) " | "
                  (plist-get th 'authors) " | "
                  (plist-get th 'subject) " | "
                  (string-join (plist-get th 'tags) ",") " | thread:"
                  (plist-get th 'thread) "\n"))
              "" threads))))

(define (mail-read-thread raw)
  (let* ((id (if (string-prefix? "thread:" raw)
                 (substring raw 7 (string-length raw))
                 raw))
         (msgs (nm--show-msgs id))
         (text (car (nm--render-text "" msgs))))
    (cond ((equal? (string-trim text) "") "no such thread")
          ;; char-based cut — a byte cut could split utf-8 and poison
          ;; the json encoder
          ((> (string-length text) 8000)
           (string-append (substring text 0 8000) "\n[...truncated]"))
          (else text))))

;; how many messages match QUERY — ground truth for whether a tag change
;; actually landed, instead of trusting a shell call whose exit status
;; nm--run already throws away. #f (NOT 0) when notmuch's output can't be
;; parsed as a number — a genuinely empty result and a surprising one
;; (a warning line, a hiccup) must not collapse into the same "zero", or
;; a parse failure reads as "no such thread" for one that exists: the
;; same blind-trust shape mail-tag! exists to fix, one level down.
(define (nm--count query)
  (nm--op 'count query))

(define (mail-tag! raw changes)
  (let* ((id (if (string-prefix? "thread:" raw)
                 (substring raw 7 (string-length raw))
                 raw))
         (n (nm--count (string-append "thread:" id))))
    (cond
      ((not n)
       (string-append "couldn't verify thread " id
                      " — notmuch count gave an unexpected answer"))
      ((= n 0) (string-append "no such thread: " id))
      (else
        (nm--op 'tag! changes (string-append "thread:" id))
        (when (buffer-exists? *notmuch-search-buffer*)
          (nm--refresh! *notmuch-search-buffer*))
          (string-append "tagged " (number->string n) " message"
                         (if (= n 1) "" "s") " in thread " id
                         " (" changes ")")))))

(define (notmuch-sender-count query limit)
  (let* ((raw (nm--op 'senders query))
         (lines (filter (lambda (l) (> (string-length l) 0)) (string-split raw "\n")))
         (rows (map (lambda (l)
                      (let ((parts (string-split l "\t")))
                        (list (string->number (car parts)) (cadr parts))))
                    lines))
         (top (list-head (reverse (sort rows)) (min limit (length rows)))))
    (if (null? top)
        "no matches"
        (fold (lambda (acc row)
                (string-append acc (number->string (car row)) "\t" (cadr row) "\n"))
              "" top))))

;; the raw CLI, for whatever mail-search/mail-tag! don't cover — bulk
;; tag/archive by QUERY ("tag -inbox -- from:luma.com") in one call
;; instead of enumerating thread ids and tagging them one at a time, or
;; "count -- QUERY" to check a result instead of trusting a blind "done".
;; notmuch's own syntax is public, stable, and already in every model's
;; training data — better to let it speak that directly than force
;; everything through a bespoke per-thread wrapper. Same shell nm--run
;; always used; refreshes the search buffer since ARGS may have mutated
;; tags, same as mail-tag!.
(define (notmuch args)
  (let ((out (nm--run args)))
    (when (buffer-exists? *notmuch-search-buffer*)
      (nm--refresh! *notmuch-search-buffer*))
    (if (equal? (string-trim out) "") "(no output)" out)))

(category! 'mail)
(public! 'mail-search
  "(mail-search QUERY) — notmuch search (from:, to:, subject:, tag:, dates, free text); one thread per line with its thread:ID")
(public! 'mail-read-thread
  "(mail-read-thread THREAD-ID) — full text of an email thread, thread: prefix optional")
(effects! '(read))
(public! 'mail-message "(mail-message HEADERS BODY [ATTACHMENTS]) — the RFC 822 text of a mail; HEADERS is a plist of from to cc bcc subject in-reply-to references, each attachment a path or (PATH NAME)")
(effects! '(write external))
(public! 'mail-send! "(mail-send! HEADERS BODY [ATTACHMENTS]) — send a mail with files attached; #t when sent, else the error")
(public! 'mail-send-text! "(mail-send-text! TEXT) — send RFC 822 TEXT through the route its From picks; #t when sent, else the error")
(public! 'mail-reply! "(mail-reply! MSG-ID BODY [OPTS]) — reply in thread to a message, from whichever mailbox holds it; notmuch fills every header and the From picks the route. OPTS 'sender-only 'cc 'no-quote 'mailbox 'dry-run; #t when sent, else the error")
(effects! '(unknown))
(public! 'mail-tag!
  "(mail-tag! THREAD-ID CHANGES) — apply space-separated +tag/-tag changes to a thread; returns how many messages it actually matched (a real count, not a blind \"done\") — 0 means the thread id was wrong")
(public! 'notmuch-sender-count
  "(notmuch-sender-count QUERY LIMIT) — biggest senders matching QUERY (from: addresses, deduplicated), top LIMIT as \"COUNT\\tSENDER\" lines, most first")
(public! 'notmuch
  "(notmuch ARGS) — the raw notmuch CLI, ARGS is everything after `notmuch` as one string, e.g. \"tag -inbox -- from:luma.com\" or \"count -- tag:inbox from:luma.com\"; prefer this for bulk ops by query (archive/tag many at once) and for verifying a change actually happened, instead of enumerating thread ids one at a time")

;;; The mail feed: every message a mailbox indexes becomes a
;;; "mail:<profile>" event in the event log, headers only; the body stays
;;; in notmuch. The mail host's post-new hook posts each batch of tag:new
;;; to the feed webhook's /mail. A slow sweep asks each mailbox for what
;;; changed after its saved lastmod revision and adds what a push dropped;
;;; a received message only the sweep found is a "feed:mail:<profile>"
;;; missed event.

(domain! 'mail)
(effects! '(write external))

(defcustom 'mail-feed-enabled #f
  "Start the mail feed when this package loads."
  'group 'notmuch 'type 'boolean)

(defcustom 'mail-feed-host "marilyn"
  "The machine whose notmuch mailboxes the feed reads."
  'group 'notmuch 'type 'string)

(defcustom 'mail-feed-profiles '("svs.io" "recruiting" "vidura")
  "The notmuch profiles the feed reads, one per mailbox. \"gmail:ADDRESS\" reads that Gmail account; mail-feed-gmail-seconds paces it."
  'group 'notmuch 'type 'list)

(defcustom 'mail-feed-sweep-seconds 900
  "Seconds between two sweeps of the mailboxes."
  'group 'notmuch 'type 'integer)

;; the message ids the log holds, newest first; seeded from the log once
(defvar '*mail-feed-seen* #f)

(effects! '(pure))

(define (mail-feed--messages tree)
  "(mail-feed--messages TREE) — every matched message in notmuch show JSON, whose threads nest replies in lists"
  (cond ((not (pair? tree)) '())
        ((symbol? (car tree))
         (if (and (plist-get tree 'id) (plist-get tree 'match)) (list tree) '()))
        (else (apply append (map mail-feed--messages tree)))))

(define (mail-feed--address from)
  "(mail-feed--address FROM) — the bare address of a From header, lower case"
  (let ((parts (string-split (or from "") "<")))
    (string-downcase
     (string-trim (if (> (length parts) 1) (car (string-split (cadr parts) ">")) (car parts))))))

(define (mail-feed--event profile msg)
  "(mail-feed--event PROFILE MSG) — the topic, kind and data of one notmuch message"
  (let ((h (plist-get msg 'headers))
        (tags (or (plist-get msg 'tags) '())))
    (list (string-append "mail:" profile)
          ;; a mail to yourself is both: it notifies like any inbox mail
          (if (and (member "sent" tags) (not (member "inbox" tags))) 'sent 'received)
          (list 'id (plist-get msg 'id)
                'mailbox profile
                'from (plist-get h 'From)
                'address (mail-feed--address (plist-get h 'From))
                'to (plist-get h 'To)
                'cc (plist-get h 'Cc)
                'subject (plist-get h 'Subject)
                'at (plist-get msg 'timestamp)
                'tags tags))))

(define (mail-feed--mailbox profile)
  "(mail-feed--mailbox PROFILE) — the mailbox the feed reads for PROFILE: (HOST PROFILE), or (gmail ADDRESS) for a PROFILE \"gmail:ADDRESS\""
  (let ((mb (nm--pair->mailbox profile "")))
    (if (symbol? (car mb)) mb (list mail-feed-host profile))))

(effects! '(write))

(define (mail-feed-publish! profile msg)
  "(mail-feed-publish! PROFILE MSG) — log one notmuch message unless the log has it; its seq, or #f"
  (unless *mail-feed-seen*
    (set! *mail-feed-seen*
          (map (lambda (e) (plist-get (plist-get e 'data) 'id))
               (event-log-newest "mail:*" 5000))))
  (let ((id (plist-get msg 'id)))
    (if (or (not id) (member id *mail-feed-seen*))
        #f
        (let ((e (mail-feed--event profile msg)))
          (set! *mail-feed-seen* (take (cons id *mail-feed-seen*) 5000))
          (event-publish! (car e) (cadr e) (caddr e))))))

(define (mail-feed--handle request)
  "(mail-feed--handle REQUEST) — the feed webhook's /mail: one post-new batch, notmuch show JSON, its profile in x-notmuch-profile"
  (let* ((h (assoc "x-notmuch-profile" (plist-get request 'headers)))
         (profile (and h (cadr h)))
         (tree (json-parse (plist-get request 'body))))
    (cond ((not (equal? (plist-get request 'method) "POST")) (event-feed-reply 405 "POST only"))
          ((not (member profile mail-feed-profiles)) (event-feed-reply 400 "unknown mailbox"))
          ((not tree) (event-feed-reply 400 "bad json"))
          (else
           (for-each (lambda (m) (mail-feed-publish! profile m)) (mail-feed--messages tree))
           (event-feed-reply 200 "ok")))))

(effects! '(write external))

(define (mail-feed--revision profile)
  "(mail-feed--revision PROFILE) — the mailbox's lastmod revision, or #f; it blocks, so call it from a task"
  (nm--op-on (mail-feed--mailbox profile) 'revision))

(define (mail-feed--sweep-one! profile)
  "(mail-feed--sweep-one! PROFILE) — read what changed in PROFILE after its saved revision and log what the log lacks. The first sweep only saves the revision."
  (let ((after (event-log-position (string-append "mail-feed:" profile)))
        (since (event-log-position (string-append "mail-feed-since:" profile))))
    (task-run!
     (lambda ()
       (let ((rev (mail-feed--revision profile)))
         (list rev
               (and rev after since
                    (nm--op-on (mail-feed--mailbox profile) 'changed
                               after (max since (- (current-time) 172800)))))))
     (lambda (ok? value)
       (when (and ok? (car value))
         (let ((missed 0))
           (for-each (lambda (m)
                       (when (and (mail-feed-publish! profile m)
                                  (not (member "sent" (or (plist-get m 'tags) '()))))
                         (set! missed (+ missed 1))))
                     (mail-feed--messages (or (cadr value) '())))
           (unless since
             (event-log-position-set! (string-append "mail-feed-since:" profile) (current-time)))
           (event-log-position-set! (string-append "mail-feed:" profile) (car value))
           (when (> missed 0)
             (event-publish! (string-append "feed:mail:" profile) 'missed (list 'count missed)))))))))

(define (mail-feed-sweep!)
  "(mail-feed-sweep!) — sweep every mailbox of mail-feed-profiles"
  (for-each mail-feed--sweep-one! mail-feed-profiles))

(define (mail-feed--tick _)
  (report-errors "mail-feed: sweep" (lambda () (mail-feed-sweep!)))
  (debounce! 'mail-feed-sweep (* 1000 mail-feed-sweep-seconds)
             (lambda (x) (mail-feed--tick x)) #f))

(defcustom 'mail-feed-gmail-seconds 5
  "Seconds between two looks at the Gmail mailboxes of mail-feed-profiles. Gmail
pushes nothing here, so this is how soon its new mail becomes an event; a
look with nothing new costs one history.list call. It runs whether or not
mail-feed-enabled starts the rest of the feed."
  'group 'notmuch 'type 'integer)

(define (mail-feed--gmail-profiles)
  (filter (lambda (p) (symbol? (car (mail-feed--mailbox p)))) mail-feed-profiles))

(define (mail-feed--gmail-tick _)
  (report-errors "mail-feed: gmail" (lambda () (for-each mail-feed--sweep-one! (mail-feed--gmail-profiles))))
  (unless (null? (mail-feed--gmail-profiles))
    (debounce! 'mail-feed-gmail (* 1000 mail-feed-gmail-seconds)
               (lambda (x) (mail-feed--gmail-tick x)) #f)))

(define (mail-feed-start!)
  "(mail-feed-start!) — take the mail host's pushes and start the sweeps"
  (event-feed-route! "/mail" (lambda (request) (mail-feed--handle request)))
  (event-feed-start!)
  (mail-feed--tick #f)
  (mail-feed--gmail-tick #f))

(define (mail-feed-stop!)
  "(mail-feed-stop!) — stop taking the mail host's pushes and stop the sweep"
  (set! *event-feed-routes* (events--without "/mail" *event-feed-routes*))
  (debounce-cancel! 'mail-feed-sweep)
  (debounce-cancel! 'mail-feed-gmail))

(define-command "mail-feed-start" "Turn new mail into events: the webhook and the sweep"
  (lambda () (mail-feed-start!) (message "Mail feed on")))

(define-command "mail-feed-stop" "Stop turning new mail into events"
  (lambda () (mail-feed-stop!) (message "Mail feed off")))

;; after the load, not in it: a start inside the loader does not take
(define (mail-feed--boot!)
  (when mail-feed-enabled
    (debounce! 'mail-feed-boot 0 (lambda (_) (mail-feed-start!)) #f)))
(after-init 'mail-feed--boot!)

;; Gmail mailboxes notify on their own: nothing pushes their mail, so the
;; poll is what makes a notice at all
(unless (or mail-feed-enabled (null? (mail-feed--gmail-profiles)))
  (debounce! 'mail-feed-gmail-boot 0 (lambda (_) (mail-feed--gmail-tick #f)) #f))

;;; notices: inbox mail that arrives passes in the corner

(domain! 'mail)
(effects! '(write display))

(defcustom 'mail-notify #t
  "Show a notice for each inbox mail that arrives.")

(defcustom 'mail-notify-skip-tags
  '("spam" "promotions" "social" "forums" "updates" "newsletter" "transactional")
  "Mail with one of these notmuch tags makes no notice.")

(define (mail-notify--name from)
  "the name of a From header: \"Air India\" <x@y> is Air India, a bare address stays"
  (let* ((name (string-trim (car (string-split from " <"))))
         (n (string-length name)))
    (cond ((= n 0) from)
          ((and (> n 1) (string-prefix? "\"" name) (string-suffix? "\"" name))
           (substring name 1 (- n 1)))
          (else name))))

(define (mail-notify-wanted? e)
  "#t when the mail E is in the inbox and wears none of mail-notify-skip-tags"
  (let ((tags (or (plist-get (plist-get e 'data) 'tags) '())))
    (and (member "inbox" tags)
         (null? (filter (lambda (t) (member t mail-notify-skip-tags)) tags)))))

(define (mail-notify--saw e)
  "a received inbox mail makes a notice: the sender's name, then the subject"
  (let ((d (plist-get e 'data)))
    (when (and mail-notify (eq? (plist-get e 'kind) 'received) (mail-notify-wanted? e))
      (notify! "mail"
               (mail-notify--name (or (plist-get d 'from) "Mail"))
               (or (plist-get d 'subject) "")
               (list 'topic (plist-get e 'topic) 'event (plist-get e 'seq)
                     'mailbox (plist-get d 'mailbox) 'id (plist-get d 'id))))))

(event-subscribe! "mail-notify" "mail:*" 'mail-notify--saw)

(defcustom 'mail-notify-autotags '("ephemeral")
  "Tags that make a notice when notmuch-autotag gives them to a thread.")

(define (mail-notify--tagged e)
  "a thread autotag gave one of mail-notify-autotags makes a notice: the tag and sender, then the subject"
  (let* ((d (plist-get e 'data))
         (hit (filter (lambda (t) (member t mail-notify-autotags)) (or (plist-get d 'added) '()))))
    (when (and mail-notify (eq? (plist-get e 'kind) 'tagged) (pair? hit))
      (notify! "mail"
               (string-append (string-join hit " ") " · "
                              (string-trim (mail-notify--name (let ((f (plist-get d 'from))) (if (equal? f "") "Mail" f)))))
               (or (plist-get d 'subject) "")
               (list 'topic (plist-get e 'topic) 'event (plist-get e 'seq)
                     'mailbox (plist-get d 'mailbox) 'thread (plist-get d 'id))))))

(event-subscribe! "mail-notify-tagged" "mail-tag:*" 'mail-notify--tagged)

;; effects stamps for the names declared above without one, so
;; eval-scheme-read takes or refuses them by what they do
(for-each (lambda (n) (catalog-meta! 'function (symbol->string n) 'effects '(read execute)))
  '(notmuch-sender-count mail-search mail-read-thread))
(for-each (lambda (n) (catalog-meta! 'function (symbol->string n) 'effects '(write execute)))
  '(notmuch mail-tag!))
(for-each (lambda (n) (catalog-meta! 'function (symbol->string n) 'effects '(write display)))
  '(mail-open!))


;; 1-3 take the tabs, / picks one by name (tab-bars.scm)
(tab-bars-declare! "notmuch-mode" (lambda (buf) (list (list "Tabs" (nm--tabs buf)))))

;; <left> and <right> step the tabs, wrapping at either end
(define (nm--tab-step! buf delta)
  (let* ((tabs (nm--tabs buf))
         (n (length tabs))
         (at (let loop ((ts tabs) (i 0)) (cond ((null? ts) #f) ((caddr (car ts)) i) (else (loop (cdr ts) (+ i 1))))))
         (next (modulo (+ (or at (if (> delta 0) -1 0)) delta) n))
         (id (car (list-ref tabs next))))
    (nm--take-tab! buf (substring id 12 (string-length id)))))

(define-command "notmuch-tab-next" "Show the next tab of the mail list"
  (lambda () (nm--tab-step! (current-buffer) 1))
  'modes '(notmuch-mode))
(define-command "notmuch-tab-prev" "Show the previous tab of the mail list"
  (lambda () (nm--tab-step! (current-buffer) -1))
  'modes '(notmuch-mode))
(define-key "notmuch-mode-map" "<right>" "notmuch-tab-next")
(define-key "notmuch-mode-map" "<left>" "notmuch-tab-prev")
