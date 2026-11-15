;;; notmuch-test.scm --- the mail client, against a stub notmuch.
;;;
;;; The test builds its own fixture: a shell script that answers the
;;; queries notmuch would, and the JSON it prints. notmuch-program is a
;;; Scheme seam, so nothing here needs Elixir — write-file! makes the
;;; stub, shell-command->string makes it executable and removes it after.
;;;
;;; Nothing here presses a key. A prompt is answered with
;;; minibuffer-change! and minibuffer-confirm, which is the path a typed
;;; answer takes, and every row verb is a named command.

(domain! 'testing)
(effects! '(write))

;; *notmuch*, *mail*, *mailboxes* and *compose* are the package's own
;; names, in a test and in a person's editor alike. The setup below resets
;; them, so this file must never run against a live one: it already cost
;; somebody their open mail and the chat beside it.
(tests-need-a-disposable-editor!
  "resets *notmuch*, *mail*, *mailboxes* and *compose*, which a live editor is using")

(define t--nm-dir (string-append (compos-home) "/zz-nm-stub"))
(define t--nm-program (string-append t--nm-dir "/notmuch"))

;; The seams a test may replace, as the package defined them. Setup and
;; teardown put every one back, so a test that fails half way through
;; leaves no stub behind for the next one.
(define t--nm-originals
  (list nm--run nm--search-json nm--show-msgs nm--open-attachment! nm--open-thread!
        nm--saved-searches nm--all-tags nm--db-path visit shell-command->string
        *nm-db-paths*))
(define (t--nm-restore!)
  (let ((o t--nm-originals))
    (set! nm--run (list-ref o 0))
    (set! nm--search-json (list-ref o 1))
    (set! nm--show-msgs (list-ref o 2))
    (set! nm--open-attachment! (list-ref o 3))
    (set! nm--open-thread! (list-ref o 4))
    (set! nm--saved-searches (list-ref o 5))
    (set! nm--all-tags (list-ref o 6))
    (set! nm--db-path (list-ref o 7))
    (set! visit (list-ref o 8))
    (set! shell-command->string (list-ref o 9))
    (set! *nm-db-paths* (list-ref o 10))))

;; A shell callback runs on the lane the test holds, so it would land after
;; the test. This runs it at once, with the real command's output.
(define (t--nm-shell-at-once!)
  (let ((shell (list-ref t--nm-originals 9)))
    (set! shell-command->string
      (lambda (cmd . more)
        (if (and (pair? more) (procedure? (list-ref more (- (length more) 1))))
            ((list-ref more (- (length more) 1)) (shell cmd))
            (apply shell cmd more))))))

;; the JSON the stub prints, and the script that chooses between them
(define t--nm-files
  (list
    (list "search.json" "  [{\"thread\": \"0001\", \"timestamp\": 1786065644, \"date_relative\": \"Today 06:50\", \"matched\": 1, \"total\": 1, \"authors\": \"Alice\", \"subject\": \"Hello world\", \"query\": [\"id:m1\", null], \"tags\": [\"inbox\", \"unread\"]},\n  {\"thread\": \"0002\", \"timestamp\": 1786037671, \"date_relative\": \"Yest. 23:04\", \"matched\": 1, \"total\": 2, \"authors\": \"Bob| Carol\", \"subject\": \"Quarterly report\", \"query\": [\"id:m2\", \"id:m3\"], \"tags\": [\"inbox\"]}]")
    (list "sender-search.json" "  [{\"thread\": \"0001\", \"timestamp\": 1786065644, \"date_relative\": \"Today 06:50\", \"matched\": 1, \"total\": 1, \"authors\": \"Alice\", \"subject\": \"Hello world\", \"query\": [\"id:m1\", null], \"tags\": [\"inbox\", \"unread\"]}]")
    (list "show.json" "  [[[{\"id\": \"m1\", \"match\": true, \"excluded\": false, \"filename\": [\"/Users/svs/Mail/svsrecruiting/mail/cur/abc:2,S\"], \"timestamp\": 1786065644, \"date_relative\": \"Today 06:50\", \"tags\": [\"inbox\", \"unread\"], \"duplicate\": 1, \"body\": [{\"id\": 1, \"content-type\": \"multipart/alternative\", \"content\": [{\"id\": 2, \"content-type\": \"text/plain\", \"content\": \"Hi there, this is the body.\\n\"}, {\"id\": 3, \"content-type\": \"text/html\", \"content-charset\": \"utf-8\", \"content-length\": 100}]}], \"headers\": {\"Subject\": \"Hello world\", \"From\": \"Alice <alice@example.com>\", \"To\": \"svs@svsrecruiting.com\", \"Date\": \"Thu, 07 Aug 2026 06:50:44 +0530\"}}, []]]]")
    (list "show-html.json" "  [[[{\"id\": \"m2\", \"match\": true, \"excluded\": false, \"filename\": [\"/Users/svs/Mail/svsrecruiting/mail/cur/def:2,S\"], \"timestamp\": 1786037671, \"date_relative\": \"Yest. 23:04\", \"tags\": [\"inbox\"], \"duplicate\": 1, \"body\": [{\"id\": 1, \"content-type\": \"text/html\", \"content\": \"<p>Hello <b>HTML</b> world</p>\"}], \"headers\": {\"Subject\": \"Quarterly report\", \"From\": \"Bob <bob@example.com>\", \"To\": \"svs@svsrecruiting.com\", \"Date\": \"Wed, 06 Aug 2026 23:04:31 +0530\"}}, []]]]")
    (list "reply.json" "  {\"reply-headers\": {\"Subject\": \"Re: Hello world\", \"From\": \"SVS <svs@svsrecruiting.com>\", \"To\": \"Alice <alice@example.com>\", \"In-reply-to\": \"<m1>\", \"References\": \"<m1>\"}, \"original\": {\"id\": \"m1\", \"match\": false, \"excluded\": false, \"filename\": [\"/Users/svs/Mail/svsrecruiting/mail/cur/abc:2,S\"], \"timestamp\": 1786065644, \"date_relative\": \"Today 06:50\", \"tags\": [\"inbox\"], \"body\": [{\"id\": 1, \"content-type\": \"multipart/alternative\", \"content\": [{\"id\": 2, \"content-type\": \"text/plain\", \"content\": \"Hi there, this is the body.\\n\"}, {\"id\": 3, \"content-type\": \"text/html\", \"content-length\": 100}]}], \"headers\": {\"Subject\": \"Hello world\", \"From\": \"Alice <alice@example.com>\", \"To\": \"svs@svsrecruiting.com\", \"Date\": \"Thu, 07 Aug 2026 06:50:44 +0530\"}}}")))

(define t--nm-script "#!/bin/sh\ndir=\"$(dirname \"$0\")\"\necho \"$@\" >> \"$dir/calls.log\"\ncase \"$1\" in\n  search)\n    case \"$*\" in\n      *thread:{from:alice@example.com}*) printf '[]\\n';;\n      *from:alice@example.com*) cat \"$dir/sender-search.json\";;\n      *--output=messages*) echo \"id:m1\";;\n      *--output=tags*) printf 'important\\ninbox\\nunread\\n';;\n      *) cat \"$dir/search.json\";;\n    esac;;\n  show)\n    case \"$*\" in\n      *thread:0002*) cat \"$dir/show-html.json\";;\n      *) cat \"$dir/show.json\";;\n    esac;;\n  reply) cat \"$dir/reply.json\";;\n  config)\n    case \"$*\" in\n      *database.path*) printf '/test/mail\\n';;\n      *) printf 'query.inbox.query=tag:inbox\\nquery.unread.query=tag:unread\\n';;\n    esac;;\n  # two different shapes: `count --batch` reads queries on stdin and\n  # answers one line each (the mailbox list), while a plain `count`\n  # must NOT touch stdin — draining a pipe nobody writes to blocks\n  # forever and wedges the Session for every later eval.\n  count)\n    case \"$*\" in\n      *--batch*)\n        while read -r q; do\n          case \"$q\" in *unread*) printf '2\\n';; *) printf '5\\n';; esac\n        done;;\n      *no-such*) printf '0\\n';;\n      *) printf '5\\n';;\n    esac;;\nesac")

;; Every test starts from a clean stub and a clean editor: the call log is
;; what most of them read, and a stale one answers for the wrong test.
(define (t--nm-setup!)
  (t--nm-restore!)
  (shell-command->string (string-append "rm -rf " t--nm-dir))
  (make-directory! t--nm-dir)
  (for-each (lambda (f) (write-file! (string-append t--nm-dir "/" (car f)) (cadr f)))
            t--nm-files)
  (write-file! t--nm-program t--nm-script)
  (shell-command->string (string-append "chmod +x " t--nm-program))
  (set! notmuch-program t--nm-program)
  ;; an unset mailbox raises (notmuch.scm); the fake store is this machine's
  (set! notmuch-host "")
  (set! notmuch-profile "")
  (set! notmuch-auto-preview #f)
  ;; a test asserts what a preview DOES, not when: no rest timer between
  ;; the move and the assertion
  (set! notmuch-preview-delay 0)
  (set! notmuch-html-renderer "cat")
  (switch-to-buffer! "*scratch*")
  (for-each (lambda (b)
              (when (or (equal? b "*notmuch*") (equal? b "*mailboxes*")
                        (string-prefix? "*mail" b)
                        (equal? b "*compose*"))
                (buffer-kill! b)))
            (buffer-list))
  ;; the suite runs in a live editor, whose frame may already be split;
  ;; the window assertions below count what these commands opened. The
  ;; panes model goes too: a layout another suite left (a blank pane) does
  ;; not know the index's window, and the preview then lands on the index
  (panes-layout-forget!)
  (delete-other-windows!)
  t--nm-dir)

(define (t--nm-calls) (or (read-file (string-append t--nm-dir "/calls.log")) ""))

(define (t--nm-done!)
  (t--nm-restore!)
  (for-each (lambda (b)
              (when (or (equal? b "*notmuch*") (equal? b "*mailboxes*")
                        (string-prefix? "*mail" b) (equal? b "*compose*"))
                (buffer-kill! b)))
            (buffer-list))
  (shell-command->string (string-append "rm -rf " t--nm-dir)))

;; The row verbs, by the command each key runs. NOT wrapped in
;; with-current-buffer: that restores the buffer when the thunk exits, and
;; half of these commands are meant to leave you somewhere else.
;;
;; A keystroke runs the command and then the post-command pass, which is
;; where mail policy decides what the pane shows. Scheme's own run-command
;; is only half of that, so this seam supplies the other half.
(define (t--nm-run! cmd) (run-command cmd) (post-command!))

;;; --- the search listing --------------------------------------------------------

(deftest 'notmuch-opens-a-search-listing-of-threads
  "the query, the count, and one row per thread"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (check-equal! (current-buffer) "*notmuch*" "the listing is current")
    (let ((text (buffer-text "*notmuch*")))
      (check-contains! text "Mail" "the title")
      (check-contains! text "5 messages" "the message count")
      (check-contains! text "Mail · Inbox" "the account, then the query by name")
      (check-contains! text "Hello world" "the first subject")
      (check-contains! text "Quarterly report" "the second")
      (check-contains! text "Alice" "and an author"))
    (t--nm-done!)))

(deftest 'the-search-buffer-carries-column-and-status-overlays
  "the columns are faces, so a theme can move them"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (let ((faces (value->string (buffer-overlays "*notmuch*"))))
      (check-contains! faces "nm-date" "the date column")
      (check-contains! faces "nm-author" "the author column")
      (check-contains! faces "nm-subject" "the subject column")
      ;; thread 0001 is unread, 0002 is not
      (check-contains! faces "nm-unread" "and the unread mark"))
    (t--nm-done!)))

(deftest 'the-index-gives-each-thread-two-lines
  "the subject owns the first line; the author and the tags share the second"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (let* ((lines (string-split (buffer-text "*notmuch*") "\n"))
           (subj (filter (lambda (l) (string-contains? l "Hello world")) lines))
           (auth (filter (lambda (l) (string-contains? l "Alice")) lines)))
      (check-equal! (length subj) 1 "the subject is on one line")
      (check-equal! (length auth) 1 "the author is on another")
      (check-false! (string-contains? (car subj) "Alice") "the two do not share a line")
      (check-contains! (car subj) "Today 06:50" "the date rides with the subject")
      ;; inbox and unread are hidden tags, so this row draws none
      (check-false! (string-contains? (car auth) "inbox") "a hidden tag stays off the row"))
    ;; a row is two lines, and one move is one thread
    (t--nm-run! "notmuch-next")
    (check-equal! (nm--th-subject (nm--thread-at "*notmuch*")) "Quarterly report"
                  "one move goes to the next thread")
    (t--nm-done!)))

(deftest 'every-tag-reads-in-full-until-the-column-narrows
  "which tags a thread carries is what the column is for"
  (lambda ()
    (check-equal! (nm--short-tag "attachment") "att.." "a long tag keeps three letters")
    (check-equal! (nm--short-tag "inbox") "inbox" "five characters still fit")
    (check-equal! (nm--short-tag "sent") "sent" "and a short one is itself")
    (let ((tags '("attachment" "important" "inbox" "personal" "sent")))
      (check-equal! (nm--tags-text (list "0001" "s" "a" tags "d"))
                    "attachment important personal sent"
                    "every tag reads but a hidden one")
      ;; the column takes what the busiest row needs
      (check-equal! (nm--fit-tags "attachment important inbox personal sent" 40)
                    "attachment important inbox personal sent"
                    "the names stand when the column holds them")
      ;; narrower than the names: the short forms
      (check-equal! (nm--fit-tags "attachment important inbox personal sent" 28)
                    "att.. imp.. inbox per.. sent"
                    "the short forms stand when the names will not fit")
      ;; too narrow even for those: one letter each, and the count reads
      (check-equal! (nm--fit-tags "attachment important inbox personal sent" 6)
                    "aiips" "no room: the initials, all five of them"))
    (check-equal! (nm--fit-tags "" 24) "" "no tags, no text")))

(deftest 'opening-a-thread-renders-text-plain-only-and-marks-it-read
  "the HTML alternative is not the one a reader wants"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (t--nm-run! "notmuch-open-thread")
    (check-equal! (current-buffer) (nm--thread-buffer "0001") "the thread's page is current")
    (let ((text (buffer-text (current-buffer))))
      (check-contains! text "From: Alice <alice@example.com>" "the header")
      (check-contains! text "Hi there, this is the body." "the text/plain body")
      (check-false! (string-contains? text "content-length") "and not the html part"))
    (let ((log (t--nm-calls)))
      (check-contains! log "show --format=json" "it asked for the thread")
      (check-contains! log "tag -unread -- thread:0001" "and marked it read"))
    (t--nm-done!)))

(deftest 'an-html-message-renders-as-an-html-document-and-toggles-to-text
  "a message with no text/plain is shown as the document it is"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (t--nm-run! "notmuch-next")
    (t--nm-run! "notmuch-open-thread")

    (check-equal! (current-buffer) (nm--thread-buffer "0002") "the thread's page is current")
    (check-equal! (buffer-local-value 'render-mode (current-buffer)) "html" "rendered as html")
    (let ((text (buffer-text (current-buffer))))
      (check-contains! text "<!DOCTYPE html" "a whole document")
      (check-contains! text "Hello <b>HTML</b> world" "with the body")
      (check-contains! text "Quarterly report" "and the subject"))

    (t--nm-run! "notmuch-show-toggle-view")
    (check-equal! (buffer-local-value 'render-mode (current-buffer)) "blocks" "the toggle renders semantic plain text")
    ;; the renderer is stubbed to cat, so the text view carries the raw html
    (check-contains! (buffer-text (current-buffer)) "Hello <b>HTML" "which is the raw html here")

    (t--nm-run! "notmuch-show-toggle-view")
    (check-equal! (buffer-local-value 'render-mode (current-buffer)) "html" "and back again")
    (t--nm-done!)))

(deftest 'every-thread-opens-its-own-page
  "a list's rows are its pages: two threads read are two pages, and going back reuses the first"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (t--nm-run! "notmuch-open-thread")
    (let ((first (nm--thread-buffer "0001")))
      (check-contains! (buffer-text first) "Hi there, this is the body." "the first thread")

      (switch-to-buffer! "*notmuch*")
      (list-goto-first-entry "*notmuch*")
      (t--nm-run! "notmuch-next")
      (t--nm-run! "notmuch-open-thread")
      (let ((second (nm--thread-buffer "0002")))
        (check-true! (and second (not (equal? first second))) "the second thread has a page of its own")
        (check-contains! (buffer-text second) "Quarterly report" "the second thread")
        (check-contains! (buffer-text first) "Hi there" "and the first page still holds the first"))
      (check-equal! (length (nm--show-buffers)) 2 "two threads read, two pages")

      (switch-to-buffer! "*notmuch*")
      (list-goto-first-entry "*notmuch*")
      (t--nm-run! "notmuch-open-thread")
      (check-equal! (nm--thread-buffer "0001") first "going back opens the same page")
      (check-equal! (length (nm--show-buffers)) 2 "and makes no new one"))
    (t--nm-done!)))

;;; --- the row verbs -------------------------------------------------------------

(deftest 'archive-tags-the-thread-at-point
  "one row, one thread, one tag change"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (t--nm-run! "notmuch-archive")
    (check-contains! (t--nm-calls) "tag -inbox -- thread:0001" "the archive call")
    (t--nm-done!)))

(deftest 'smart-untag-removes-the-tag-the-search-is-on
  "on a simple tag search, u means -that-tag"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (t--nm-run! "notmuch-smart-untag")
    (check-contains! (t--nm-calls) "tag -inbox -- thread:0001" "the untag call")
    (t--nm-done!)))

(deftest 'mark-toggles-local-selection
  "selection survives refresh without writing a mail tag"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (t--nm-run! "notmuch-mark-toggle")
    (check-equal! (list-marks "*notmuch*") '(("0001" "*")) "selected locally")
    (nm--refresh! "*notmuch*")
    (check-equal! (list-marks "*notmuch*") '(("0001" "*")) "still selected")
    (check-equal! (string-contains? (t--nm-calls) "tag +compos-mark") #f "no mark write")
    (t--nm-done!)))

(deftest 'reply-composes-from-the-threads-newest-message
  "the index replies to the thread, not to whatever is open"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (t--nm-run! "notmuch-reply")
    (check-equal! (current-buffer) "*compose*" "a compose buffer")
    (check-contains! (t--nm-calls) "search --output=messages --limit=1 -- thread:0001"
                     "it asked for the newest message")
    (check-contains! (buffer-text "*compose*") "Subject: Re: Hello world" "with the reply headers")
    (t--nm-done!)))

(deftest 'first-and-last-thread-jump-to-the-ends-of-the-list
  "the ends are rows, not buffer positions"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (t--nm-run! "notmuch-last-thread")
    (check-equal! (car (nm--thread-at (current-buffer))) "0002" "the last thread")
    (t--nm-run! "notmuch-first-thread")
    (check-equal! (car (nm--thread-at (current-buffer))) "0001" "and the first")
    (t--nm-done!)))

;;; --- the mailboxes and quitting -------------------------------------------------

(deftest 'notmuch-starts-at-the-mailboxes-and-a-row-opens-its-search
  "the counts come from one batched call, not one per mailbox"
  (lambda ()
    (t--nm-setup!)
    ;; a callback cannot land while the test holds the lane: the test
    ;; holds the batch and answers it with the stub's real output
    (t--nm-hold-counts!)
    (run-command "notmuch")
    (check-equal! (current-buffer) "*mailboxes*" "the mailbox list")
    (let ((r (car t--nm-requests)))
      ((cadr r) ((list-ref t--nm-originals 9) (car r))))
    (check-true! (and (equal? (buffer-local-value 'nm-hello-status "*mailboxes*") "")
                      (not (string-contains? (buffer-text "*mailboxes*") "loading counts")))
      "the counts arrive")
    (let ((text (buffer-text "*mailboxes*")))
      (check-contains! text "Mailboxes" "the title")
      (check-contains! text "inbox" "a mailbox")
      (check-contains! text "5" "and its count"))
    (check-contains! (t--nm-calls) "count --batch" "one batched count")

    (t--nm-run! "notmuch-hello-open")
    (check-equal! (current-buffer) "*notmuch*" "the row opened its search")
    (check-equal! (nm--query-of "*notmuch*") "tag:inbox" "on that mailbox's query")

    (t--nm-run! "notmuch")
    (check-equal! (current-buffer) "*mailboxes*" "and back to the mailboxes")
    (t--nm-done!)))

(deftest 'notmuch-quit-kills-the-mail-views-and-lands-on-work
  "leaving mail returns you to what you were doing"
  (lambda ()
    (t--nm-setup!)
    (let ((work (string-append (compos-home) "/zz-nm-work.txt")))
      (write-file! work "work\n")
      ;; visit loads quietly; you were working in it, so show it
      (switch-to-buffer! (visit work))
      (run-command "notmuch")
      (run-command "notmuch-inbox")
      (t--nm-run! "notmuch-open-thread")
      (run-command "notmuch-quit")
      (check-false! (buffer-exists? "*notmuch*") "the listing is gone")
      (check-false! (buffer-exists? "*mailboxes*") "the mailboxes are gone")
      (check-true! (null? (nm--show-buffers)) "the thread pages are gone")
      (check-equal! (current-buffer) work "and the work buffer is current")

      (when (buffer-known? work) (buffer-kill! work))
      (delete-file! work))
    (t--nm-done!)))

;;; --- what a chat is told --------------------------------------------------------

(deftest 'context-providers-explain-the-selection-to-chat-and-agents
  "the list says what is selected; the thread view says what is open"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")

    (let ((ctx (editor-context "*zz-nm-chat*")))
      (check-contains! ctx "Hello world" "the selected subject")
      (check-contains! ctx "thread:0001" "and its id"))
    (check-contains! (editor-context-preamble "*zz-nm-chat*") "Editor context" "the preamble")

    (t--nm-run! "notmuch-open-thread")
    (let ((ctx (editor-context "*zz-nm-chat*")))
      (check-contains! ctx "thread:0001" "the open thread")
      (check-contains! ctx "id:m1" "and the message in it"))

    ;; a buffer with no provider still gets its line: name, mode and line.
    ;; close mail first, so no mail view is left to provide a line
    (run-command "notmuch-quit")
    (switch-to-buffer! "*scratch*")
    (let ((pre (editor-context-preamble "*zz-nm-chat*")))
      (check-contains! pre "- *scratch* (" "the snapshot names it")
      (check-contains! pre ", line " "and its line"))
    ;; with the snapshot off, only providers speak
    (let ((was editor-context-snapshot))
      (set! editor-context-snapshot #f)
      (check-equal! (editor-context-preamble "*zz-nm-chat*") "" "no provider, no preamble")
      (set! editor-context-snapshot was))
    (t--nm-done!)))


;;; --- the eval-scheme surface ----------------------------------------------------

(deftest 'the-mail-functions-search-and-read-through-the-same-renderer
  "what an agent calls is what the buffers show"
  (lambda ()
    (t--nm-setup!)
    (let ((out (mail-search "tag:inbox")))
      (check-contains! out "thread:0001" "the thread id")
      (check-contains! out "Hello world" "the subject")
      (check-contains! out "Alice" "and the author"))
    (check-contains! (mail-read-thread "thread:0001") "Hi there, this is the body." "the body")
    (let ((out (mail-tag! "0002" "+important")))
      (check-contains! out "tagged" "it reports the change")
      (check-contains! out "0002" "on the thread")
      (check-contains! out "+important" "with the tag"))
    (t--nm-done!)))


;;; --- the prompts ----------------------------------------------------------------

(define (t--nm-answer! text)
  (minibuffer-change! text)
  (run-command "minibuffer-confirm"))

(deftest 'jump-goes-to-a-saved-search-by-name
  "the mailbox names are the completion set"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (run-command "notmuch-jump")
    (t--nm-answer! "unread")
    (check-equal! (nm--query-of "*notmuch*") "tag:unread" "the saved search opened")
    (t--nm-done!)))

(deftest 'filter-narrows-the-current-search
  "the base query and the filter stack, never a derived string"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (run-command "notmuch-filter")
    (t--nm-answer! "from:alice")
    (check-equal! (nm--query-of "*notmuch*") "( tag:inbox ) and from:alice"
                  "the filter rides on the base")
    (t--nm-done!)))

(deftest 'tag-filter-menu-adds-a-filter-inside-the-inbox
  "the menu offers tags from the current result and preserves the inbox base"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (run-command "notmuch-filter-by-tag")
    (check-equal! (minibuffer-selected) "important"
                  "the menu contains a tag from the inbox result")
    (t--nm-answer! "unread")
    (check-equal! (nm--query-of "*notmuch*") "( tag:inbox ) and tag:unread"
                  "the selected tag narrows the inbox")

    (run-command "notmuch-unfilter-last")
    (check-equal! (nm--query-of "*notmuch*") "tag:inbox"
                  "removing the filter restores the inbox")
    (t--nm-done!)))

(deftest 'add-tag-completes-from-the-database
  "the tags come from notmuch, not from a list we keep"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (run-command "notmuch-add-tag")
    (t--nm-answer! "important")
    (let ((log (t--nm-calls)))
      (check-contains! log "search --output=tags" "it asked the database")
      (check-contains! log "tag +important -- thread:0001" "and tagged the thread"))
    (t--nm-done!)))

(deftest 'structured-filters-are-removed-without-parsing-query-text
  "the stack is data, so a pop needs no parser"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (run-command "notmuch-search")
    (t--nm-answer! "tag:inbox and from:alice")
    (run-command "notmuch-filter")
    (t--nm-answer! "subject:report")
    (check-equal! (nm--query-of "*notmuch*") "( tag:inbox and from:alice ) and subject:report"
                  "the filter rides on the search")

    (run-command "notmuch-unfilter-last")
    (check-equal! (nm--query-of "*notmuch*") "tag:inbox and from:alice"
                  "and popping leaves the search alone")
    (t--nm-done!)))

(deftest 'unfilter-restores-the-originating-list-position
  "backslash returns to the row where the filter was entered"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (list-goto-index! "*notmuch*" 1)

    (run-command "notmuch-filter")
    (t--nm-answer! "from:alice")
    (check-equal! (list-index "*notmuch*") 0 "the filter starts at its first row")

    (run-command "notmuch-unfilter-last")
    (check-equal! (nm--query-of "*notmuch*") "tag:inbox" "backslash removes the filter")
    (check-equal! (list-index "*notmuch*") 1 "the inbox returns to the originating row")
    (t--nm-done!)))

(deftest 'a-saved-position-names-a-thread-not-a-row-number
  "filter to a sender, trash every mail they sent, come back: row 0 is a stranger"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (list-goto-index! "*notmuch*" 0)
    (let ((pos (nm--position-of "*notmuch*"))
          (saved nm--search-json))
      (check-equal! (car pos) 0 "an index")
      (check-equal! (cdr pos) '("0001" "0002") "and the threads from that row down")
      ;; while the filter was on, 0001 was trashed and 0003 arrived. Row 0
      ;; is now a thread the reader has never seen; 0002 is the one that
      ;; followed the row they left.
      (set! nm--search-json
        (lambda (query limit)
          (list (list 'thread "0003" 'subject "New arrival" 'authors "Dave"
                      'date_relative "Now" 'tags '("inbox"))
                (list 'thread "0002" 'subject "Quarterly report" 'authors "Bob"
                      'date_relative "Yest. 23:04" 'tags '("inbox")))))
      (nm--refresh! "*notmuch*")
      (nm--goto-position! "*notmuch*" pos)
      (check-equal! (nm--th-id (nm--thread-at "*notmuch*")) "0002"
                    "it lands on the next thread that survived, not on row 0")
      (set! nm--search-json saved))
    (t--nm-done!)))

(deftest 'replacement-filters-are-removed-in-order
  "by-sender and by-marked replace each other; backslash walks back"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (run-command "notmuch-filter-by-sender")
    (check-contains! (t--nm-calls) "show --format=json --body=false thread:0001"
                     "it read the sender off the thread")
    (check-equal! (length (list-entries "*notmuch*")) 1 "one row matches")
    (check-equal! (nm--query-of "*notmuch*") "from:alice@example.com" "the sender filter")

    (run-command "notmuch-mark-toggle")
    (run-command "notmuch-filter-marked")
    (check-equal! (nm--query-of "*notmuch*") "( from:alice@example.com ) and ( thread:0001 )" "the selected search replaced it")

    (run-command "notmuch-unfilter-last")
    (check-equal! (nm--query-of "*notmuch*") "from:alice@example.com" "and back to the sender")
    (run-command "notmuch-unfilter-last")
    (check-equal! (nm--query-of "*notmuch*") "tag:inbox" "and back to the mailbox")
    (check-equal! (length (list-entries "*notmuch*")) 2 "with every row again")
    (t--nm-done!)))

(deftest 'opening-a-mailbox-refreshes-rows-cached-by-a-filter
  "the cached rows are the filter's, not the mailbox's"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (run-command "notmuch-filter-by-sender")
    (check-equal! (length (list-entries "*notmuch*")) 1 "the filter cut the rows")

    (run-command "notmuch")
    (run-command "notmuch-hello-open")
    (check-equal! (nm--query-of "*notmuch*") "tag:inbox" "the mailbox query is back")
    (check-equal! (length (list-entries "*notmuch*")) 2 "and all its rows")
    (t--nm-done!)))

;; the stub answers every search from search.json; a test that needs
;; marked rows rewrites that file with the mark tag on some threads
(define (t--nm-replace s from to) (string-join (string-split s from) to))
(define (t--nm-search-json-with! from to)
  (write-file! (string-append t--nm-dir "/search.json")
    (t--nm-replace (cadr (assoc "search.json" t--nm-files)) from to)))

(deftest 'a-marked-thread-shows-the-list-mark-on-its-row
  "local selection draws a mark and ignores legacy server marks"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (t--nm-search-json-with! "\"tags\": [\"inbox\", \"unread\"]"
                             "\"tags\": [\"inbox\", \"compos-mark\", \"unread\"]")
    (nm--refresh! "*notmuch*")
    (check-equal! (list-marks "*notmuch*") '() "old tags are not selection")
    (run-command "notmuch-mark-toggle")
    (check-equal! (list-marks "*notmuch*") '(("0001" "*")) "selected row")
    (let ((row (car (list-row-lines "*notmuch*" (car (list-entries "*notmuch*"))))))
      (check-equal! (string-prefix? "* " (car row)) #t "visible mark"))
    (t--nm-done!)))

(deftest 'mark-all-again-unmarks-the-search
  "one selected thread is enough for star to clear selection"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (run-command "notmuch-mark-toggle")
    (check-equal! (nm--any-marked? "*notmuch*") #t "one selected")
    (run-command "notmuch-mark-all")
    (check-equal! (nm--any-marked? "*notmuch*") #f "cleared")
    (t--nm-done!)))

(deftest 'mark-all-under-a-filter-selects-only-the-narrowed-query
  "the selection names the base plus filters without a tag write"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (run-command "notmuch-filter")
    (t--nm-answer! "from:alice")
    (run-command "notmuch-mark-all")
    (check-equal! (nm--marked-query "*notmuch*") "( ( tag:inbox ) and from:alice )" "full query")
    (run-command "notmuch-mark-all")
    (check-equal! (nm--any-marked? "*notmuch*") #f "cleared")
    (check-equal! (string-contains? (t--nm-calls) "compos-mark") #f "no mark writes")
    (t--nm-done!)))

(deftest 'mark-all-then-archive-marked-asks-before-it-acts
  "archive applies to the full filtered query and keeps the selection"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (nm--query-push! "*notmuch*" "from:alice")
    (nm--refresh! "*notmuch*")
    (run-command "notmuch-mark-all")
    (run-command "notmuch-archive-marked")
    (t--nm-answer! "yes")
    (check-contains! (t--nm-calls) "tag -inbox -- ( ( tag:inbox ) and from:alice )" "archive query")
    ;; the marked thread ids stay valid when their tags change
    (check-true! (nm--any-marked? "*notmuch*") "the selection stays across the refresh")
    (t--nm-done!)))

(deftest 'embark-acts-on-the-email-at-point
  "the target is the row, and the act tool drives the same table"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (let ((target (value->string (target-at "*notmuch*"))))
      (check-contains! target "email" "the target is an email")
      (check-contains! target "0001" "and names the thread"))

    (run-command "embark-act")
    (t--nm-answer! "archive")
    (check-contains! (t--nm-calls) "tag -inbox -- thread:0001" "the action ran")

    ;; the tool reports what the ACTION did, never a blind "done" — a model
    ;; told an archive succeeded when it did not will keep going
    (let ((out (llm-tool-call "act" (list 'type "email" 'id "thread:0002" 'action "trash"))))
      (check-contains! out "0002" "it names the thread")
      (check-contains! out "+trash -inbox -unread" "and the tags it set"))
    (check-contains! (t--nm-calls) "tag +trash -inbox -unread -- thread:0002" "which really ran")

    (check-contains! (llm-tool-call "act" (list 'type "email" 'id "no-such-thread" 'action "trash"))
                     "no such thread" "a thread that is not there says so")
    (let ((out (llm-tool-call "act" (list 'type "email" 'id "0002" 'action "explode"))))
      (check-contains! out "no such action" "and an action that is not there")
      (check-contains! out "archive" "naming the ones there are"))
    (t--nm-done!)))

;;; --- the two-pane reading flow ---------------------------------------------------

(deftest 'preview-fetches-the-thread-at-point-with-its-html
  "the preview verb asks notmuch for the thread at point, html included"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (run-command "notmuch-preview")
    (check-contains! (t--nm-calls) "show --format=json --include-html thread:0001" "the fetch")
    (t--nm-done!)))

(deftest 'mail-opens-in-the-group-you-are-in
  "mail is an app: it lays out where you stand, its buffers join that group, and opening keeps the index focused"
  (lambda ()
    (t--nm-setup!)
    (let ((group (frame-group)))
      (run-command "mail")
      (check-equal! (frame-group) group "mail did not move the frame to a group of its own")
      (let ((index (window-showing "*notmuch*")))
        (check-true! (and index #t) "the index is on screen")
        (check-equal! (active-window) index "and focused")
        (when group
          (check-true! (buffer-in-group? "*notmuch*" group) "the index joined the group"))
        (run-command "notmuch-open-thread")
        (let ((page (nm--thread-buffer "0001")))
          (check-contains! (buffer-text page) "Hi there" "the thread opened as its page")
          (check-equal! (window-buffer index) "*notmuch*" "the index stayed put")
          (check-equal! (app-id page) "mail" "the page is part of the mail app")
          (when group
            (check-true! (buffer-in-group? page group) "and joined the group too")))))
    (t--nm-done!)))

(deftest 'moving-the-highlight-updates-the-shown-mail-and-marks-it-read
  "with auto-preview on, the next row is fetched and read"
  (lambda ()
    (t--nm-setup!)
    (let ((saved notmuch-auto-preview))
      (set! notmuch-auto-preview #t)
      (t--nm-run! "notmuch-inbox")
      (t--nm-run! "notmuch-next")
      (let ((log (t--nm-calls)))
        (check-contains! log "show --format=json --include-html thread:0002" "the second thread")
        (check-contains! log "tag -unread -- thread:0002" "and it was marked read"))
      (set! notmuch-auto-preview saved))
    (t--nm-done!)))

;;; --- composing a reply -----------------------------------------------------------

(deftest 'reply-is-a-message-mode-buffer-that-sends
  "headers, a separator, the quote, and point on the body"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (run-command "notmuch-open-thread")
    ;; the message view has its own reply: the index replies to the
    ;; thread's newest message, this one to the message on screen
    (run-command "notmuch-show-reply")
    (check-equal! (current-buffer) "*compose*" "a compose buffer")

    (let ((text (buffer-text "*compose*")))
      (check-contains! text "From: SVS <svs@svsrecruiting.com>" "the From header")
      (check-contains! text "To: Alice <alice@example.com>" "the To header")
      (check-contains! text "Subject: Re: Hello world" "the Subject")
      (check-contains! text "In-Reply-To: <m1>" "the In-Reply-To")
      (check-contains! text "--text follows this line--" "the separator")
      (check-contains! text "Alice <alice@example.com> writes:" "the attribution")
      (check-contains! text "> Hi there, this is the body." "and the quote"))
    (check-contains! (t--nm-calls) "reply --format=json id:" "it asked notmuch for the reply")

    ;; point sits on the empty line right after the separator
    (check-contains! (substring-bytes (buffer-text "*compose*") 0
                                      (with-current-buffer "*compose*" (lambda () (point))))
                     "line--\n" "point is past the separator")

    ;; header names and the separator carry faces
    (let ((faces (value->string (buffer-overlays "*compose*"))))
      (check-contains! faces "nm-hdr" "the header face")
      (check-contains! faces "nm-sep" "and the separator face"))

    ;; sending turns the separator into the RFC822 blank line
    (let ((sent (string-append t--nm-dir "/sent.eml"))
          (saved notmuch-send-routes))
      (set! notmuch-send-routes (list (list "" (string-append "cat > " sent))))
      (with-current-buffer "*compose*" (lambda () (run-command "mail-send")))
      (let ((body (or (read-file sent) "")))
        (check-false! (string-contains? body "text follows this line") "the separator is gone")
        (check-contains! body "Subject: Re: Hello world" "the headers survived")
        (check-contains! body "References: <m1>\n\n" "and a blank line divides them"))
      (set! notmuch-send-routes saved))
    (t--nm-done!)))

(deftest 'one-rule-keeps-the-mail-pane-on-the-thread-at-point
  "no verb says preview: the pane follows point after every command and every landing"
  (lambda ()
    (t--nm-setup!)
    (let ((saved notmuch-auto-preview)
          (shown (lambda () (let ((p (find window-showing (nm--show-buffers))))
                              (and p (buffer-local-value 'notmuch-thread p))))))
      (set! notmuch-auto-preview #t)
      (t--nm-run! "notmuch-inbox")
      (t--nm-run! "notmuch-next")
      (check-equal! (shown) "0002"
                    "a move previews, so the pane holds the thread at point")
      ;; the state a return lands in: point is on a row nothing previewed
      (list-goto-first-entry "*notmuch*")
      (check-false! (nm--pane-at-point? "*notmuch*")
                    "point moved on its own, so the pane is stale")
      (check-true! (and (member 'nm--follow-point!
                                (hook-functions 'post-command-hook))
                        #t)
                   "the rule runs after every command")
      (check-true! (and (member 'nm--follow-point!
                                (hook-functions 'window-configuration-change-hook))
                        #t)
                   "and on every landing, which is not a command")
      (nm--follow-point!)
      (check-equal! (shown) "0001"
                    "and it fills the pane from point")
      ;; q closes the pane on this thread. A rule that reopened it would
      ;; make the dismissal unusable; a move to another thread reopens it.
      (with-current-buffer (nm--thread-buffer "0001")
        (lambda () (run-command "notmuch-show-quit")))
      (nm--follow-point!)
      (check-false! (nm--pane-at-point? "*notmuch*")
                    "a dismissed pane stays dismissed")
      (t--nm-run! "notmuch-next")
      (check-equal! (shown) "0002" "and a move brings it back")
      (set! notmuch-auto-preview saved))
    (t--nm-done!)))

(deftest 'a-move-rests-before-the-mail-pane-fetches
  "a fetch is a round trip to the mail host, so only the row the highlight rests on costs one"
  (lambda ()
    (t--nm-setup!)
    (let ((saved notmuch-auto-preview))
      (set! notmuch-auto-preview #t)
      (t--nm-run! "notmuch-inbox")
      ;; the open fetched the first thread; from here a move must wait
      (set! notmuch-preview-delay 500)
      (let ((before (t--nm-calls)))
        (t--nm-run! "notmuch-next")
        (check-equal! (t--nm-calls) before
                      "a move schedules the fetch and calls nothing yet"))
      ;; and with the rest turned off the same move fetches at once
      (set! notmuch-preview-delay 0)
      (t--nm-run! "notmuch-prev")
      (check-contains! (t--nm-calls) "thread:0001"
                       "0 means every move fetches")
      (set! notmuch-auto-preview saved))
    (t--nm-done!)))

;;; --- attachments -----------------------------------------------------------------

(define t--nm-attach-msg
  '(id "message-one" headers (Subject "Report" From "Sender" Date "Today")
    body ((id 1 content-type "multipart/mixed" content
           ((id 2 content-type "multipart/alternative" content
             ((id 3 content-type "text/plain" content "Plain body")
              (id 4 content-type "text/html" content "<p>HTML body</p>")))
            (id 5 content-type "application/pdf" filename "report & <q>.pdf")
            (id 6 content-type "text/plain" filename "letter.txt" content "Attached text")
            (id 7 content-type "text/html" filename "page.html" content "Attached HTML"))))))
(define t--nm-attach-chosen #f)

;; a thread page on one message with three attachments, read under one account
(define (t--nm-attach-setup!)
  (t--nm-setup!)
  (set! notmuch-profile "attachment-account")
  (set! t--nm-attach-chosen #f)
  (set! nm--show-msgs (lambda (thread) (list t--nm-attach-msg)))
  (set! nm--open-attachment! (lambda (a) (set! t--nm-attach-chosen a)))
  (buffer-create "*mail*")
  (switch-to-buffer! "*mail*")
  (buffer-set-local! "*mail*" 'notmuch-thread "thread-one")
  (buffer-set-local! "*mail*" 'notmuch-subject "Report")
  (set-mode! "notmuch-show-mode"))

(deftest 'nested-attachments-show-in-html-and-text-beside-the-body
  "an attachment is named, never rendered in place of the body"
  (lambda ()
    (t--nm-attach-setup!)
    (let ((html (nm--msg-html t--nm-attach-msg)))
      (check-contains! html "report &amp; &lt;q&gt;.pdf" "the escaped pdf name")
      (check-contains! html "letter.txt" "the text attachment")
      (check-contains! html "page.html" "the html attachment")
      (check-contains! html "HTML body" "the html body")
      (check-false! (string-contains? html "Attached HTML") "not the attached html in its place"))
    (let ((text (nm--msg-render t--nm-attach-msg)))
      (check-contains! text "report & <q>.pdf" "the pdf name")
      (check-contains! text "letter.txt" "the text attachment")
      (check-contains! text "Plain body" "the plain body")
      (check-false! (string-contains? text "Attached text") "not the attached text in its place"))
    (t--nm-done!)))

(deftest 'the-attachment-picker-uses-the-part-and-the-account-of-the-message
  "switching account after the page opened does not change where the part comes from"
  (lambda ()
    (t--nm-attach-setup!)
    (set! notmuch-profile "another-account")
    (run-command "notmuch-open-attachment")
    (t--nm-answer! "2. letter.txt")
    (let ((chosen (value->string t--nm-attach-chosen)))
      (check-contains! chosen "letter.txt" "the file")
      (check-contains! chosen "--part=6" "its exact part")
      (check-contains! chosen "id:message-one" "of its message")
      (check-contains! chosen "attachment-account" "on the account the page was read on")
      (check-false! (string-contains? chosen "another-account") "not the current one"))
    (ignore-errors minibuffer-cancel!)
    (t--nm-done!)))

(deftest 'a-mode-reload-rebuilds-the-attachment-list
  "the list is derived state, so the mode makes it again"
  (lambda ()
    (t--nm-attach-setup!)
    (kill-local-variable! 'notmuch-attachments "*mail*")
    (with-current-buffer "*mail*" (lambda () (set-mode! "notmuch-show-mode")))
    (check-equal! (length (buffer-local-value 'notmuch-attachments "*mail*")) 3 "three attachments")
    (t--nm-done!)))

(deftest 'a-download-keeps-the-bytes-and-confines-the-file-name
  "binary stays binary, and ../ cannot leave the new directory"
  (lambda ()
    (t--nm-attach-setup!)
    (let ((opened #f))
      (set! visit (lambda (path . rest) (set! opened path)))
      (t--nm-shell-at-once!)
      ((list-ref t--nm-originals 3) (list "../payload.bin" "printf '\\000\\377ABC'"))
      (check-true! (and opened #t) "the download opened")
      (when opened
        (check-true! (string-suffix? "/payload.bin" opened) "under its own name")
        (check-false! (string-contains? opened "..") "inside the new directory")
        (check-equal! (string-trim (shell-command->string (string-append "od -An -tu1 " (sh-quote opened))))
                      "0 255  65  66  67" "the exact bytes")
        (shell-command->string (string-append "rm -rf \"$(dirname " (sh-quote opened) ")\""))))
    (t--nm-done!)))

;;; --- tag writes --------------------------------------------------------------------

;; the message of the error THUNK raises, or #f when it raises none
(define (t--nm-error-of thunk)
  (let ((before (messages-newest)))
    (report-errors "nm-test" thunk)
    (let ((after (messages-newest)))
      (and after (not (equal? after before)) (plist-get after 'text)))))

(deftest 'a-failed-tag-write-surfaces-stderr
  "a write that failed must never read as a success"
  (lambda ()
    (let ((err (t--nm-error-of
                 (lambda () (nm--tag-result "1\nCouldn't open lockfile: Operation not permitted")))))
      (check-contains! (or err "") "Notmuch tag failed" "it raises")
      (check-contains! (or err "") "Operation not permitted" "with the reason"))
    (check-equal! (nm--tag-result "0\n") "" "status 0 is a success")))

(deftest 'a-tag-write-reports-the-real-exit-status
  "also when the program fails without a word"
  (lambda ()
    (t--nm-setup!)
    (let ((script (string-append t--nm-dir "/fail.sh")))
      (write-file! script "#!/bin/sh\nexit 7\n")
      (set! notmuch-program (string-append "sh " script))
      (let ((err (or (t--nm-error-of (lambda () (nm--run "tag -inbox -- no-messages"))) "")))
        (check-contains! err "Notmuch tag failed" "a silent failure raises")
        (check-contains! err "7" "with its status"))
      (write-file! script "#!/bin/sh\nprintf 'write denied' >&2\nexit 1\n")
      (check-contains! (or (t--nm-error-of (lambda () (nm--run "tag -inbox -- no-messages"))) "")
                       "write denied" "and a loud one says what it said"))
    (t--nm-done!)))

;;; --- mailbox counts ----------------------------------------------------------------

;; Counts arrive in a callback. The stub holds each batch request, and a
;; test answers them in the order it needs, late or out of order.
(define t--nm-requests '())
(define (t--nm-hold-counts!)
  (set! t--nm-requests '())
  (let ((shell (list-ref t--nm-originals 9)))
    (set! shell-command->string
      (lambda (cmd . more)
        (if (and (pair? more) (string-contains? cmd "count --batch"))
            (set! t--nm-requests
              (append t--nm-requests (list (list cmd (list-ref more (- (length more) 1))))))
            (apply shell cmd more))))))
(define (t--nm-hello-setup! tags)
  (t--nm-setup!)
  (set! t--nm-requests '())
  (set! notmuch-host "test-host")
  (set! notmuch-profile "test-profile")
  (set! nm--all-tags tags)
  (set! nm--saved-searches (lambda () '(("inbox" "tag:inbox") ("unread" "tag:unread"))))
  (set! nm--db-path (lambda () "/test/mail"))
  (t--nm-hold-counts!)
  (buffer-create "*zz-nm-origin*")
  (switch-to-buffer! "*zz-nm-origin*"))
(define (t--nm-hello-done!)
  (t--nm-done!)
  (when (buffer-known? "*zz-nm-origin*") (buffer-kill! "*zz-nm-origin*")))
(define (t--nm-deliver! i out) ((cadr (list-ref t--nm-requests i)) out))
(define (t--nm-first-box) (car (list-entries "*mailboxes*")))

(deftest 'mailboxes-open-at-once-and-count-in-one-batch
  "opening and moving never wait for the counts"
  (lambda ()
    (t--nm-hello-setup! (lambda () '()))
    (run-command "notmuch")
    (check-equal! (current-buffer) "*mailboxes*" "the list is open")
    (check-equal! (length t--nm-requests) 1 "one request")
    (check-contains! (buffer-text "*mailboxes*") "loading counts" "while the counts load")
    (check-equal! (list-current "*mailboxes*") '("inbox" "tag:inbox" #f #f) "rows without counts")
    (check-contains! (car (car t--nm-requests)) "count --batch" "a batch")
    (check-equal! (nm--hello-count-queries (nm--saved-searches))
                  '("tag:inbox" "( tag:inbox ) and tag:unread" "tag:unread" "( tag:unread ) and tag:unread")
                  "total and unread of each mailbox")
    (run-command "list-next")
    (switch-to-buffer! "*zz-nm-origin*")
    (check-equal! (length t--nm-requests) 1 "moving and leaving fetch nothing")
    (t--nm-deliver! 0 "5\n2\n2\n2\n")
    (check-equal! (current-buffer) "*zz-nm-origin*" "the answer moves nobody")
    (check-equal! (list-current "*mailboxes*") '("unread" "tag:unread" #f #f) "the row moved to")
    ;; a hidden list draws when it is shown; its rows have the counts now
    (check-equal! (cadr (list-source-entries "*mailboxes*")) '("unread" "tag:unread" 2 2) "its counts arrived")
    (check-equal! (buffer-local-value 'nm-hello-status "*mailboxes*") "" "loading is over")
    (t--nm-hello-done!)))

(deftest 'the-latest-count-refresh-wins
  "answers that arrive out of order"
  (lambda ()
    (t--nm-hello-setup! (lambda () '()))
    (run-command "notmuch")
    (run-command "notmuch-hello-refresh")
    (t--nm-deliver! 1 "9\n3\n3\n3\n")
    (t--nm-deliver! 0 "5\n2\n2\n2\n")
    (check-equal! (t--nm-first-box) '("inbox" "tag:inbox" 9 3) "the newer answer stays")
    (t--nm-hello-done!)))

(deftest 'an-account-switch-rejects-old-counts
  "before and after the next fetch"
  (lambda ()
    (t--nm-hello-setup! (lambda () '()))
    (run-command "notmuch")
    (set! notmuch-profile "other-profile")
    (t--nm-deliver! 0 "5\n2\n2\n2\n")
    (check-equal! (t--nm-first-box) '("inbox" "tag:inbox" #f #f) "the old account's counts are refused")
    (run-command "notmuch-hello-refresh")
    (t--nm-deliver! 1 "8\n1\n1\n1\n")
    (t--nm-deliver! 0 "5\n2\n2\n2\n")
    (check-equal! (t--nm-first-box) '("inbox" "tag:inbox" 8 1) "also after the new ones came")
    (t--nm-hello-done!)))

(deftest 'failed-counts-stay-unknown-and-can-be-retried
  "an error or a short answer is not a count"
  (lambda ()
    (t--nm-hello-setup! (lambda () '()))
    (run-command "notmuch")
    (t--nm-deliver! 0 "ssh: connection failed\n")
    (check-contains! (buffer-text "*mailboxes*") "counts unavailable" "the failure says so")
    (check-equal! (t--nm-first-box) '("inbox" "tag:inbox" #f #f) "no count")
    (run-command "notmuch-hello-refresh")
    (t--nm-deliver! 1 "5\n2\n")
    (check-contains! (buffer-text "*mailboxes*") "counts unavailable" "a short answer too")
    (run-command "notmuch-hello-refresh")
    (t--nm-deliver! 2 "0\n0\n0\n0\n")
    (check-equal! (t--nm-first-box) '("inbox" "tag:inbox" 0 0) "a retry that works")
    (t--nm-hello-done!)))

(deftest 'a-mode-restore-starts-counts-again
  "the request lost with the restore is refused"
  (lambda ()
    (t--nm-hello-setup! (lambda () '()))
    (run-command "notmuch")
    (kill-local-variable! 'nm-hello-status "*mailboxes*")
    (kill-local-variable! 'nm-hello-request "*mailboxes*")
    (with-current-buffer "*mailboxes*" (lambda () (set-mode! "notmuch-hello-mode")))
    (check-equal! (length t--nm-requests) 2 "a new request")
    (t--nm-deliver! 0 "5\n2\n2\n2\n")
    (check-equal! (t--nm-first-box) '("inbox" "tag:inbox" #f #f) "the lost one is refused")
    (t--nm-deliver! 1 "7\n1\n1\n1\n")
    (check-equal! (t--nm-first-box) '("inbox" "tag:inbox" 7 1) "the new one is taken")
    (t--nm-hello-done!)))

(deftest 'a-recreated-mailbox-buffer-rejects-the-old-request
  "the request belongs to the buffer that made it"
  (lambda ()
    (t--nm-hello-setup! (lambda () '()))
    (run-command "notmuch")
    (buffer-kill! "*mailboxes*")
    (run-command "notmuch")
    (t--nm-deliver! 0 "5\n2\n2\n2\n")
    (check-equal! (t--nm-first-box) '("inbox" "tag:inbox" #f #f) "the old request is refused")
    (t--nm-deliver! 1 "6\n1\n1\n1\n")
    (check-equal! (t--nm-first-box) '("inbox" "tag:inbox" 6 1) "the new one is taken")
    (t--nm-hello-done!)))

(deftest 'account-tags-are-mailboxes-once-each
  "a tag without a saved search shows, and a tag with one is not repeated"
  (lambda ()
    (t--nm-hello-setup! (lambda () '("inbox" "unread" "investments" "invoice" "Follow up")))
    (run-command "notmuch")
    (check-equal! (map car (list-entries "*mailboxes*"))
                  '("inbox" "unread" "investments" "invoice" "Follow up") "each once")
    (t--nm-deliver! 0 "5\n2\n2\n2\n7\n3\n4\n1\n6\n0\n")
    (check-equal! (map (lambda (row) (list (car row) (list-ref row 2) (list-ref row 3)))
                       (list-entries "*mailboxes*"))
                  '(("inbox" 5 2) ("unread" 2 2) ("investments" 7 3) ("invoice" 4 1) ("Follow up" 6 0))
                  "and each with its counts")
    (t--nm-hello-done!)))

(deftest 'an-account-switch-shows-only-the-new-accounts-tags
  "tags are per account"
  (lambda ()
    (t--nm-hello-setup! (lambda () (if (equal? notmuch-profile "test-profile") '("investments") '("candidates"))))
    (run-command "notmuch")
    (set! notmuch-profile "recruiting")
    (run-command "notmuch-hello-refresh")
    (check-equal! (map car (list-entries "*mailboxes*")) '("inbox" "unread" "candidates") "the new account's tags")
    (t--nm-deliver! 0 "5\n2\n2\n2\n7\n3\n")
    (check-equal! (map car (list-entries "*mailboxes*")) '("inbox" "unread" "candidates") "the old answer brings none back")
    (t--nm-deliver! 1 "5\n2\n2\n2\n8\n1\n")
    (check-equal! (list-ref (list-entries "*mailboxes*") 2) (list "candidates" (nm--tag-query "candidates") 8 1)
                  "and the new answer counts them")
    (t--nm-hello-done!)))

(deftest 'a-tag-query-quotes-spaces-and-quotes
  "a tag name is literal"
  (lambda ()
    (check-equal! (nm--tag-query "Follow up") "tag:\"Follow up\"" "a space")
    (check-equal! (nm--tag-query "a\"b") "tag:\"a\"\"b\"" "an embedded quote")))

;;; --- the preview pane --------------------------------------------------------------

;; the thread the visible mail page holds, or #f
(define (t--nm-shown)
  (let ((p (find window-showing (nm--show-buffers))))
    (and p (buffer-local-value 'notmuch-thread p))))
(define (t--nm-preview-setup!)
  (t--nm-setup!)
  (set! notmuch-auto-preview #t))

(deftest 'opening-the-inbox-previews-its-first-thread-and-keeps-focus
  "the pane fills, the index keeps the keys"
  (lambda ()
    (t--nm-preview-setup!)
    (t--nm-run! "notmuch-inbox")
    (check-equal! (current-buffer) "*notmuch*" "the index is current")
    (check-equal! (list-index "*notmuch*") 0 "on the first row")
    (check-equal! (t--nm-shown) "0001" "the pane shows the first thread")
    (check-contains! (buffer-text (nm--thread-buffer "0001")) "Hi there, this is the body." "its body")
    (check-contains! (t--nm-calls) "tag -unread -- thread:0001" "and marks it read")
    (t--nm-done!)))

(deftest 'a-failed-read-tag-leaves-focus-in-the-index
  "the index takes focus back before the write that can fail"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (let ((run nm--run))
      (set! nm--run (lambda (args . more)
                      (if (string-prefix? "tag " args) (error "write denied") (apply run args more)))))
    (check-false! (ignore-errors (lambda () (nm--preview! "*notmuch*") #t)) "the write raised")
    (check-equal! (window-buffer (active-window)) "*notmuch*" "focus is in the index")
    (t--nm-done!)))

(deftest 'dismissal-closes-the-thread-before-mail-goes-back
  "the first q closes the pane, the next leaves the index"
  (lambda ()
    (t--nm-preview-setup!)
    (t--nm-run! "notmuch-inbox")
    (let ((page (nm--thread-buffer "0001"))
          (index (list-index "*notmuch*")))
      (check-equal! (buffer-parent page) "*notmuch*" "the page is the index's child")
      (t--nm-run! "dismiss-buffer")
      (check-false! (buffer-known? page) "the page is closed")
      (check-equal! (current-buffer) "*notmuch*" "the index is current")
      (check-equal! (list-index "*notmuch*") index "on the same row")
      (t--nm-run! "dismiss-buffer")
      (check-true! (buffer-derived-mode? (current-buffer) "notmuch-hello-mode") "then back to the mailboxes"))
    (t--nm-done!)))

(deftest 'dismissal-from-the-thread-keeps-the-search
  "closing the pane from inside it leaves the index open"
  (lambda ()
    (t--nm-preview-setup!)
    (t--nm-run! "notmuch-inbox")
    (let ((page (nm--thread-buffer "0001")))
      (select-window! (window-showing page))
      (t--nm-run! "dismiss-buffer")
      (check-false! (buffer-known? page) "the page is closed"))
    (check-true! (buffer-known? "*notmuch*") "the index stays")
    (check-true! (and (window-showing "*notmuch*") #t) "on screen")
    (t--nm-done!)))

(deftest 'reopening-the-inbox-previews-its-first-thread-again
  "the cached index goes back to the top, and the pane follows"
  (lambda ()
    (t--nm-preview-setup!)
    (t--nm-run! "notmuch-inbox")
    (t--nm-run! "notmuch-next")
    (check-equal! (t--nm-shown) "0002" "the pane holds the second thread")
    (t--nm-run! "notmuch-inbox")
    (check-equal! (current-buffer) "*notmuch*" "the index is current")
    (check-equal! (list-index "*notmuch*") 0 "on the first row")
    (check-equal! (t--nm-shown) "0001" "and the pane holds the first thread")
    (t--nm-done!)))

(deftest 'opening-without-auto-preview-opens-no-pane
  "no page, no window, no read mark"
  (lambda ()
    (t--nm-setup!)
    (t--nm-run! "notmuch-inbox")
    (check-true! (null? (nm--show-buffers)) "no page")
    (check-equal! (length (window-list)) 1 "one window")
    (check-false! (string-contains? (t--nm-calls) "tag ") "no tag write")
    (t--nm-done!)))

(deftest 'read-and-unread-toggles-stick-under-auto-preview
  "the preview does not mark read again a thread the reader marked unread"
  (lambda ()
    (t--nm-preview-setup!)
    (let ((unread #t) (run nm--run))
      (set! nm--run
        (lambda (args . more)
          (cond ((equal? args "tag +unread -- thread:zz-thread") (set! unread #t))
                ((equal? args "tag -unread -- thread:zz-thread") (set! unread #f)))
          (apply run args more)))
      (set! nm--search-json
        (lambda (query limit)
          (list (list 'thread "zz-thread" 'subject "Read state" 'authors "Alice"
                      'date_relative "Today"
                      'tags (if unread '("inbox" "unread") '("inbox"))))))
      (t--nm-run! "notmuch-inbox")
      (check-false! unread "the preview read it")
      (t--nm-run! "notmuch-toggle-unread")
      (check-true! unread "marked unread")
      (check-true! (and (member "unread" (nm--th-tags (nm--thread-at "*notmuch*"))) #t) "and the row says so")
      (t--nm-run! "notmuch-toggle-unread")
      (check-false! unread "marked read")
      (check-false! (member "unread" (nm--th-tags (nm--thread-at "*notmuch*"))) "and the row says so")
      (check-equal! (current-buffer) "*notmuch*" "from the index"))
    (t--nm-done!)))


;; the select overlay holds point and each of TEXTS
(define (t--nm-select-covers? . texts)
  (let* ((text (buffer-text "*notmuch*"))
         (spots (cons (buffer-point "*notmuch*")
                      (map (lambda (s) (string-index text s)) texts))))
    (pair? (filter (lambda (o)
                     (and (equal? (list-ref o 2) "select")
                          (null? (filter (lambda (x) (not (and x (<= (car o) x) (< x (cadr o)))))
                                         spots))))
                   (buffer-overlays "*notmuch*")))))

(deftest 'the-highlight-follows-the-row-and-survives-a-mode-reload
  "one highlight over both lines of the thread at point"
  (lambda ()
    (t--nm-setup!)
    (t--nm-run! "notmuch-inbox")
    (t--nm-run! "notmuch-next")
    (check-equal! (nm--th-id (nm--thread-at "*notmuch*")) "0002" "the second thread")
    (check-true! (t--nm-select-covers? "Quarterly report" "Bob") "highlighted, both lines")
    (with-current-buffer "*notmuch*" (lambda () (set-mode! "notmuch-mode")))
    (check-equal! (nm--th-id (nm--thread-at "*notmuch*")) "0002" "still the second thread")
    (check-true! (t--nm-select-covers? "Quarterly report" "Bob") "still highlighted")
    (t--nm-done!)))

(deftest 'unfilter-after-filter-marked-restores-the-row
  "the selected filter starts at the top; removing it goes back to the row"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (list-goto-index! "*notmuch*" 1)
    (run-command "notmuch-mark-toggle")
    (run-command "notmuch-filter-marked")
    (check-equal! (list-index "*notmuch*") 0 "the filter starts at its first row")
    (run-command "notmuch-unfilter-last")
    (check-equal! (nm--query-of "*notmuch*") "tag:inbox" "the filter is gone")
    (check-equal! (list-index "*notmuch*") 1 "and the row is back")
    (t--nm-done!)))

(deftest 'mail-back-pops-one-filter-and-restores-its-row
  "each back is one filter"
  (lambda ()
    (t--nm-setup!)
    (run-command "notmuch-inbox")
    (list-goto-index! "*notmuch*" 1)
    (nm--query-push! "*notmuch*" "from:alice")
    (nm--refresh! "*notmuch*")
    (list-goto-index! "*notmuch*" 0)
    (nm--query-push! "*notmuch*" "tag:unread")
    (nm--refresh! "*notmuch*")
    (run-command "notmuch-back")
    (check-equal! (nm--query-of "*notmuch*") "( tag:inbox ) and from:alice" "one filter off")
    (check-equal! (current-buffer) "*notmuch*" "in the index")
    (run-command "notmuch-back")
    (check-equal! (nm--query-of "*notmuch*") "tag:inbox" "the other off")
    (check-equal! (list-index "*notmuch*") 1 "on the row it started from")
    (t--nm-done!)))

;;; --- selection and bulk tags -------------------------------------------------------

;; A search of 10000 messages that loads two rows. The stub counts every
;; call and fetch: selection is local, so most verbs here must make none.
(define t--nm-tag-calls '())
(define t--nm-tag-fetches 0)
(define (t--nm-tag-quiet!) (set! t--nm-tag-calls '()) (set! t--nm-tag-fetches 0))
(define (t--nm-tag-setup!)
  (t--nm-setup!)
  (t--nm-tag-quiet!)
  (set! nm--search-json
    (lambda (query limit)
      (set! t--nm-tag-fetches (+ t--nm-tag-fetches 1))
      (map (lambda (id) (list 'thread id 'subject id 'authors "Test" 'date_relative "Today"
                              'tags '("inbox")))
           '("one" "two"))))
  (set! nm--run
    (lambda (args . more)
      (set! t--nm-tag-calls (append t--nm-tag-calls (list args)))
      (cond ((string-prefix? "count " args) "10000\n")
            ((equal? args "config get database.path") "/test/mail\n")
            ((string-prefix? "search --output=tags" args) "inbox\nunread\n")
            (else ""))))
  (run-command "notmuch-inbox"))
(define (t--nm-narrow!)
  (nm--query-push! "*notmuch*" "from:alice")
  (nm--refresh! "*notmuch*")
  (t--nm-tag-quiet!))
(define (t--nm-tag-writes) (filter (lambda (c) (string-prefix? "tag " c)) t--nm-tag-calls))
(define (t--nm-tag-done!) (ignore-errors minibuffer-cancel!) (t--nm-done!))

(deftest 'select-all-on-the-bare-inbox-is-refused-without-work
  "the whole inbox is not a selection"
  (lambda ()
    (t--nm-tag-setup!)
    (t--nm-tag-quiet!)
    (run-command "notmuch-mark-all")
    (check-equal! t--nm-tag-calls '() "no call")
    (check-equal! t--nm-tag-fetches 0 "no fetch")
    (check-false! (nm--any-marked? "*notmuch*") "nothing selected")
    (t--nm-tag-done!)))

(deftest 'select-and-clear-ten-thousand-messages-without-work
  "the selection is the query, not the rows"
  (lambda ()
    (t--nm-tag-setup!)
    (t--nm-narrow!)
    (run-command "notmuch-mark-all")
    (check-equal! (list-marks "*notmuch*") '(("one" "*") ("two" "*")) "the loaded rows show it")
    (check-equal! (nm--marked-query "*notmuch*") "( ( tag:inbox ) and from:alice )" "the query")
    (run-command "notmuch-unmark-all")
    (check-equal! (list-marks "*notmuch*") '() "cleared")
    (check-equal! t--nm-tag-calls '() "no call")
    (check-equal! t--nm-tag-fetches 0 "no fetch")
    (t--nm-tag-done!)))

(deftest 'single-marks-survive-a-refresh-and-select-all-clears-them
  "in the inbox, star clears a selection"
  (lambda ()
    (t--nm-tag-setup!)
    (run-command "notmuch-mark-toggle")
    (nm--refresh! "*notmuch*")
    (check-equal! (list-marks "*notmuch*") '(("one" "*")) "the mark stays")
    (run-command "notmuch-mark-all")
    (check-false! (nm--any-marked? "*notmuch*") "star clears it")
    (t--nm-tag-done!)))

(deftest 'bulk-add-tags-the-whole-selected-query
  "one write on the query"
  (lambda ()
    (t--nm-tag-setup!)
    (t--nm-narrow!)
    (run-command "notmuch-mark-all")
    (run-command "notmuch-add-tag")
    (t--nm-answer! "investments")
    (check-equal! (car (t--nm-tag-writes)) "tag '+investments' -- '( ( tag:inbox ) and from:alice )'" "the write")
    ;; the thread ids stay valid when their tags change, as for archive
    (check-true! (nm--any-marked? "*notmuch*") "the selection stays")
    (t--nm-tag-done!)))

(deftest 'unmarking-a-row-of-select-all-excludes-its-thread
  "without a round trip"
  (lambda ()
    (t--nm-tag-setup!)
    (t--nm-narrow!)
    (run-command "notmuch-mark-all")
    (run-command "notmuch-mark-toggle")
    (check-equal! t--nm-tag-calls '() "no call")
    (check-equal! t--nm-tag-fetches 0 "no fetch")
    (check-equal! (list-marks "*notmuch*") '(("two" "*")) "the other row stays marked")
    (run-command "notmuch-add-tag")
    (t--nm-answer! "invoice")
    (check-equal! (car (t--nm-tag-writes))
                  "tag '+invoice' -- '( ( tag:inbox ) and from:alice ) and not ( thread:one )'" "the write excludes it")
    (t--nm-tag-done!)))

(deftest 'remove-tag-offers-the-tags-of-the-whole-selection
  "not only the tags of the row at point"
  (lambda ()
    (t--nm-tag-setup!)
    (t--nm-narrow!)
    (run-command "notmuch-mark-all")
    (check-false! (member "unread" (nm--th-tags (nm--thread-at "*notmuch*"))) "point has no unread")
    (run-command "notmuch-remove-tag")
    (t--nm-answer! "unread")
    (check-equal! (car t--nm-tag-calls) "search --output=tags -- '( ( tag:inbox ) and from:alice )'" "it asked the selection")
    (check-equal! (cadr t--nm-tag-calls) "tag '-unread' -- '( ( tag:inbox ) and from:alice )'" "and wrote to it")
    (t--nm-tag-done!)))

(deftest 'a-single-mark-scopes-a-bulk-tag-to-its-thread-and-search
  "the marked thread inside the search"
  (lambda ()
    (t--nm-tag-setup!)
    (run-command "notmuch-mark-toggle")
    (run-command "notmuch-add-tag")
    (t--nm-answer! "invoice")
    (check-true! (and (member "tag '+invoice' -- '( tag:inbox ) and ( thread:one )'" t--nm-tag-calls) #t) "the write")
    (t--nm-tag-done!)))

(deftest 'without-a-selection-add-tag-tags-the-row-at-point
  "point is the default"
  (lambda ()
    (t--nm-tag-setup!)
    (run-command "notmuch-add-tag")
    (t--nm-answer! "invoice")
    (check-true! (and (member "tag '+invoice' -- thread:one" t--nm-tag-calls) #t) "the write")
    (t--nm-tag-done!)))

(deftest 'a-filter-change-clears-the-selection-for-good
  "going back does not bring it back"
  (lambda ()
    (t--nm-tag-setup!)
    (t--nm-narrow!)
    (run-command "notmuch-mark-all")
    (nm--query-push! "*notmuch*" "tag:unread")
    (check-false! (nm--any-marked? "*notmuch*") "a new filter clears it")
    (nm--query-pop! "*notmuch*")
    (check-false! (nm--any-marked? "*notmuch*") "and back does not restore it")
    (t--nm-tag-done!)))

(deftest 'filter-marked-shows-the-selection-and-back-restores-the-filter
  "the selected threads are a filter like any other"
  (lambda ()
    (t--nm-tag-setup!)
    (t--nm-narrow!)
    (run-command "notmuch-mark-toggle")
    (run-command "notmuch-filter-marked")
    (check-equal! (nm--query-of "*notmuch*") "( ( tag:inbox ) and from:alice ) and ( thread:one )" "the selection")
    (run-command "dismiss-buffer")
    (check-equal! (nm--query-of "*notmuch*") "( tag:inbox ) and from:alice" "back to the filter")
    (t--nm-tag-done!)))

(deftest 'a-reload-keeps-the-selection-and-an-account-switch-drops-it
  "a selection belongs to one account"
  (lambda ()
    (t--nm-tag-setup!)
    (t--nm-narrow!)
    (run-command "notmuch-mark-all")
    (with-current-buffer "*notmuch*" (lambda () (set-mode! "notmuch-mode")))
    (check-true! (nm--any-marked? "*notmuch*") "kept across the reload")
    (let ((old notmuch-profile))
      (set! notmuch-profile "other")
      (nm--selection "*notmuch*")
      (set! notmuch-profile old))
    (check-false! (nm--any-marked? "*notmuch*") "dropped by the switch")
    (t--nm-tag-done!)))

(deftest 'archive-follows-the-marks-after-a-yes
  "nothing is written before the confirmation"
  (lambda ()
    (t--nm-tag-setup!)
    (run-command "notmuch-mark-toggle")
    (check-equal! (list-marks "*notmuch*") '(("one" "*")) "one mark")
    (run-command "notmuch-archive")
    (check-equal! (t--nm-tag-writes) '() "it asks first")
    (t--nm-answer! "yes")
    (check-equal! (t--nm-tag-writes) '("tag -inbox -- '( tag:inbox ) and ( thread:one )'") "then archives the marks")
    (t--nm-tag-done!)))

(deftest 'the-header-counts-the-selection
  "also a selection larger than the loaded rows"
  (lambda ()
    (t--nm-tag-setup!)
    (run-command "notmuch-mark-toggle")
    (check-contains! (nm--search-meta "*notmuch*") "1 thread selected" "one thread")
    (run-command "notmuch-unmark-all")
    (check-false! (string-contains? (nm--search-meta "*notmuch*") "selected") "none")
    (t--nm-narrow!)
    (run-command "notmuch-mark-all")
    (check-contains! (nm--search-meta "*notmuch*") "All 10000 matching messages selected" "the whole query")
    (check-equal! t--nm-tag-fetches 0 "no fetch")
    (check-equal! t--nm-tag-calls '() "no call")
    (run-command "notmuch-mark-toggle")
    (check-contains! (nm--search-meta "*notmuch*") "All matching messages selected except 1 thread" "less one")
    (t--nm-tag-done!)))
