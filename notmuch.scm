;;; notmuch.scm --- the notmuch package: the mail UI over a notmuch index.
;;;
;;; The mail UI itself is mail.scm: the mailboxes, the index, the thread
;;; view, tagging, compose and send, the feed, notices and autotag. It reads
;;; mail only through the operations of a backend (see "mail backends" in
;;; mail.scm). This file is the notmuch backend: the notmuch CLI, here or
;;; over ssh. mail-gmail.scm is the Gmail backend.

(require 'mail)

(defcustom 'notmuch-program "notmuch"
  "The notmuch executable." 'group 'notmuch)

(define (nm--cmd args &optional mailbox)
  (unless mailbox (nm--mailbox-check!))
  (let* ((host (if mailbox (car mailbox) notmuch-host))
         (profile (if mailbox (cadr mailbox) notmuch-profile))
         (here (string-append
                 (if (equal? profile "")
                     ""
                     (string-append "NOTMUCH_PROFILE=" (sh-quote profile) " "))
                 notmuch-program " " args)))
    (if (equal? host "")
        here
        ;; the remote shell reads the whole call as one word
        (string-append notmuch-ssh-program " " (sh-quote host)
                       " " (sh-quote here)))))

(define (nm--tag-result output)
  (let* ((lines (string-split output "\n"))
         (status (car lines))
         (body (string-join (cdr lines) "\n")))
    (if (equal? status "0") body
        (error (string-append "Notmuch tag failed: "
                 (if (equal? (string-trim body) "") output (string-trim body)))))))

(define (nm--run args &optional mailbox)
  (if (string-prefix? "tag " args)
      (nm--tag-result
        (shell-command->string
          (string-append "compos_tag_output=$(" (nm--cmd args mailbox)
            " 2>&1); compos_tag_status=$?; printf '%s\\n%s' \"$compos_tag_status\" \"$compos_tag_output\"")))
      (shell-command->string (nm--cmd args mailbox))))

(define (nm--json args)
  (json-parse (nm--run args)))

(define (nm--lines s)
  (filter (lambda (l) (not (equal? l ""))) (string-split (string-trim (or s "")) "\n")))

(define (nm--q query)
  (if (re-match? "^[A-Za-z0-9:._@+=-]+$" query) query (sh-quote query)))

(define (nm--run-on mb args)
  (if mb (nm--run args mb) (nm--run args)))

(define (nm--json-on mb args)
  (json-parse (nm--run-on mb args)))

(define (nm--changes-text changes)
  (if (string? changes) changes (string-join (map sh-quote changes) " ")))

(define (nm--lastmod-of out)
  (let ((f (string-split (string-trim out) "\t")))
    (and (= (length f) 3) (string->number (caddr f)))))

(mail-backend-register! 'notmuch
  (list
   'search (lambda (mb query limit)
             (nm--json-on mb (string-append "search --format=json --limit="
                                            (number->string limit) " -- " (sh-quote query))))
   'show (lambda (mb thread html?)
           (nm--json-on mb (string-append "show --format=json "
                                          (if html? "--include-html" "--body=false")
                                          " thread:" thread)))
   'changed (lambda (mb after since)
              (nm--json-on mb (string-append
                               "show --format=json --body=false --entire-thread=false "
                               (sh-quote (string-append "lastmod:" (number->string after) ".."
                                                        " and date:@" (number->string since) "..")))))
   'changed-threads (lambda (mb after since rest)
                      (nm--lines (nm--run-on mb (string-append
                                                 "search --output=threads -- "
                                                 (sh-quote (string-append
                                                            "lastmod:" (number->string after) ".."
                                                            " and date:@" (number->string since) ".."
                                                            rest))))))
   'revision (lambda (mb) (nm--lastmod-of (nm--run-on mb "count --lastmod '*'")))
   'tag! (lambda (mb changes query)
           (nm--run-on mb (string-append "tag " (nm--changes-text changes) " -- " (nm--q query))))
   'count (lambda (mb query)
            (string->number (string-trim (nm--run-on mb (string-append "count -- " (sh-quote query))))))
   'count-batch (lambda (mb queries k)
                  (shell-command->string (nm--batch-count-command queries) k))
   'tags (lambda (mb query)
           (nm--lines (nm--run-on mb (if query
                                         (string-append "search --output=tags -- " (nm--q query))
                                         "search --output=tags --exclude=false -- '*'"))))
   'threads (lambda (mb query)
              (nm--lines (nm--run-on mb (string-append "search --output=threads -- " (sh-quote query)))))
   'newest-message (lambda (mb thread)
                     (let ((out (string-trim (nm--run-on mb (string-append
                                                             "search --output=messages --limit=1 -- thread:" thread)))))
                       (and (string-prefix? "id:" out) (substring out 3 (string-length out)))))
   'raw (lambda (mb msg-id filter)
          (nm--run-on mb (string-append "show --format=raw -- " (sh-quote (string-append "id:" msg-id))
                                        (if (equal? filter "") "" (string-append " | " filter)))))
   'part-command (lambda (mb msg-id part)
                   (nm--cmd (string-append "show --format=raw --part=" (number->string part) " -- "
                                           (sh-quote (string-append "id:" msg-id)))
                            mb))
   'reply (lambda (mb msg-id)
            (nm--json-on mb (string-append "reply --format=json id:" (sh-quote msg-id))))
   'config (lambda (mb key) (nm--run-on mb (string-append "config get " key)))
   'saved-searches (lambda (mb) (nm--parse-saved-searches (nm--run-on mb "config list")))
   'senders (lambda (mb query)
              (nm--run-on mb (string-append
                              "address --output=sender --output=count --deduplicate=address -- "
                              (sh-quote query))))
   'block-sender! (lambda (mb email) (nm--block-sender! email))))

(define (nm--batch-count-command queries)
  (string-append "printf '%s\\n' "
                 (string-join (map sh-quote queries) " ")
                 " | " (nm--cmd "count --batch")))

(define *notmuch-blocked-file* "$HOME/.notmuch/blocked.txt")

(define (nm--block-sender! email)
  "Record EMAIL in the blocked-sender database the notmuch post-new hook reads."
  (nm--host-sh
   (string-append "mkdir -p \"$HOME/.notmuch\" && touch \"" *notmuch-blocked-file* "\" && "
                  "grep -qxF " (sh-quote email) " \"" *notmuch-blocked-file* "\" || "
                  "printf '%s\\n' " (sh-quote email) " >> \"" *notmuch-blocked-file* "\""))
  email)

;; the Gmail backend: a mailbox (gmail ADDRESS) reads that Google account
(require 'mail-gmail)
