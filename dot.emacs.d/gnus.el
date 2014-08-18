; -*- Lisp -*-

;;;; NNTP

(setq gnus-select-method
      '(nntp "news.aioe.org"
	     (nntp-port-number 119))) ;; 563 443

;; Scoring
(setq gnus-summary-expunge-below -100)


;;;; Gmail support

;;; A lot of this comes from http://blog.binchen.org/posts/notes-on-using-gnus.html
(add-to-list 'gnus-secondary-select-methods
	     '(nnimap "gmail"
		      (nnimap-address "imap.gmail.com")
		      (nnimap-server-port 993)
		      (nnimap-stream ssl)))

(setq-default gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
	      gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
	      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)

;; Message sending stuff.
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587
				   "quokka@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq user-full-name "Rory Molinari"
      user-mail-address "quokka@gmail.com")

(add-hook 'gnus-group-mode-hook
	  'gnus-topic-mode)

;; (add-hook 'message-mode-hook
;;           '(lambda ()
;;              (bbdb-initialize 'message)
;;              (bbdb-initialize 'gnus)
;;              (local-set-key "\t" 'bbdb-complete-mail)))

;; BBDB
(when (require 'bbdb nil 'noerror)
  (bbdb-initialize 'message 'gnus 'mail)
  (setq bbdb-file "~/.emacs.d/bbdb.db")
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
  (setq bbdb/mail-auto-create-p t
        bbdb/news-auto-create-p t)
  (defvar bbdb-time-internal-format "%Y-%m-%d"
    "The internal date format.")
  ;;;###autoload
  (defun bbdb-timestamp-hook (record)
    "For use as a `bbdb-change-hook'; maintains a notes-field called `timestamp'
    for the given record which contains the time when it was last modified.  If
    there is such a field there already, it is changed, otherwise it is added."
    (bbdb-record-putprop record 'timestamp (format-time-string
                                             bbdb-time-internal-format
                                             (current-time)))))
