;; (add-to-list 'load-path "/usr/local/Cellar/mu/1.6.8/share/emacs/site-lisp/mu/mu4e/")
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e")
;;(require 'smtpmail)

;; (crafted-package-install-package 'async)
(add-to-list 'package-selected-packages 'async)
(require 'smtpmail-async)
;; (use-package async
;;   :ensure t
;;   :config (require 'smtpmail-async))
;; ;; (use-package smtpmail-async)

;; smtp
(setq
 ;;send-mail-function 'smtpmail-send-it
 ;;   message-send-mail-function 'smtpmail-send-it
 send-mail-function 'async-smtpmail-send-it
 message-send-mail-function 'async-smtpmail-send-it
 ;; message-send-mail-function 'smtpmail-send-it
 ;;      smtpmail-starttls-credentials
 ;;      '(("mx.ingo-richter.io" 587 nil nil))
 smtpmail-default-smtp-server "mail.ingo-richter.io"
 smtpmail-smtp-server "mail.ingo-richter.io"
 smtpmail-smtp-service 587
 smtpmail-stream-type  'starttls
 smtpmail-queue-mail t
 smtpmail-queue-dir (expand-file-name "~/Mail/queue/cur")
 smtpmail-debug-info t)

(defun async-smtpmail-send-queued-mail (sync-func &rest args)
  (message "Starting asynchronous smtpmail-send-queued-mail")
  (async-start
   `(lambda ()
      (require 'smtpmail)
      ;; see smtpmail-async.el - we inject the same variables
      ,(async-inject-variables
        "\\`\\(smtpmail\\|async-smtpmail\\|\\(user-\\)?mail\\)-\\|auth-sources\\|epg\\|nsm"
        nil
        "\\`\\(mail-header-format-function\\|smtpmail-address-buffer\\|mail-mode-abbrev-table\\)")
      ;; if we don't use the above inject we can pass in specific variables like this:
      ;; (setq smtpmail-queue-dir ,smtpmail-queue-dir)
      ;; (setq smtpmail-smtp-server ,smtpmail-smtp-server)
      (,sync-func))
   (lambda (&optional _unused)
     (message "Done sending queued mail in the background."))))

;; https://emacs.stackexchange.com/a/14827/8743 has more, err, advice.
;;(advice-add #'smtpmail-send-queued-mail :around #'async-smtpmail-send-queued-mail)

(require 'mu4e)
(require 'mu4e-org)

;; we installed this with homebrew
(setq mu4e-mu-binary (executable-find "mu"))

;; this is the directory we created before:
(setq mu4e-maildir (expand-file-name "~/Mail"))

;; this command is called to sync imap servers:
(setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))

;; how often to call it in seconds:
(setq mu4e-update-interval 300)

;; save attachment to desktop by default
;; or another choice of yours:
(setq mu4e-attachment-dir "~/Desktop")

;; mbsync avoid duplicate UIDs
;; https://www.tomica.net/blog/2020/03/replacing-offlineimap-with-mbsync-isync
(setq mu4e-change-filenames-when-moving t)

;; list of your email adresses:
(setq mu4e-user-mail-address-list '("ingorichter@mac.com"
                                    "ingo.richter@gmail.com"
                                    "ingo@ingo-richter.io"))

(setq mail-user-agent 'mu4e-user-agent)

;; get mail
(setq mu4e-get-mail-command "mbsync -a"
      mu4e-html2text-command "w3m -T text/html"
      mu4e-update-interval 120
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil)

;; check your ~/.maildir to see how the subdirectories are called
;; for the generic imap account:
;; e.g `ls ~/.maildir/example'
(setq mu4e-maildir-shortcuts
      '(("/icloud/INBOX" . ?i)
        ("/icloud/Sent Messages" . ?I)
        ("/ingo.richter@gmail.com/INBOX" . ?g)
        ("/ingo.richter@gmail.com/[Gmail]/Sent Mail" . ?G)
        ("/ingo-richter.io/INBOX" . ?e)
        ("/ingo-richter.io/Sent" . ?E)))

;; the following is to show shortcuts in the main view.
(mu4e-bookmark-define "maildir:/icloud/INBOX" "Inbox - iCloud" ?i)
(mu4e-bookmark-define "maildir:/ingo.richter@gmail.com/INBOX" "Inbox - Gmail" ?g)
(mu4e-bookmark-define "maildir:/ingo-richter.io/INBOX" "Inbox - ingo@ingo-richter.io" ?o)

;; Contexts
(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "icloud"
          :enter-func
          (lambda () (mu4e-message "Enter ingorichter@mac.com context"))
          :leave-func
          (lambda () (mu4e-message "Leave ingorichter@mac.com context"))
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches msg
                                                  :to "ingorichter@mac.com")))
          :vars '((user-mail-address . "ingorichter@mac.com" )
                  (user-full-name . "Ingo Richter")
                  (mu4e-drafts-folder . "/icloud/Drafts")
                  (mu4e-refile-folder . "/icloud/Archive")
                  (mu4e-sent-folder . "/icloud/Sent Messages")
                  (mu4e-trash-folder . "/icloud/Deleted Messages")))

        ,(make-mu4e-context
          :name "gmail"
          :enter-func
          (lambda () (mu4e-message "Enter ingo.richter@gmail.com context"))
          :leave-func
          (lambda () (mu4e-message "Leave ingo.richter@gmail.com context"))
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches msg
                                                  :to "ingo.richter@gmail.com")))
          :vars '((user-mail-address . "ingo.richter@gmail.com")
                  (user-full-name . "Ingo Richter")
                  (mu4e-drafts-folder . "/ingo.richter@gmail.com/Drafts")
                  (mu4e-refile-folder . "/ingo.richter@gmail.com/Archive")
                  (mu4e-sent-folder . "/ingo.richter@gmail.com/Sent")
                  (mu4e-trash-folder . "/ingo.richter@gmail.com/Trash")))

        ,(make-mu4e-context
          :name "ingo-richter.io"
          :enter-func
          (lambda () (mu4e-message "Enter ingo@ingo-richter.io context"))
          :leave-func
          (lambda () (mu4e-message "Leave ingo@ingo-richter.io context"))
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches msg
                                                  :to "ingo@ingo-richter.io")))
          :vars '((user-mail-address . "ingo@ingo-richter.io")
                  (user-full-name . "Ingo Richter")
                  ;; check your ~/.maildir to see how the subdirectories are called
                  ;; e.g `ls ~/.maildir/example'
                  (mu4e-drafts-folder . "/ingo-richter.io/Drafts")
                  (mu4e-refile-folder . "/ingo-richter.io/Archive")
                  (mu4e-sent-folder . "/ingo-richter.io/Sent")
                  (mu4e-trash-folder . "/ingo-richter.io/Trash")))))

(setq mu4e-context-policy 'pick-first) ;; start with the first (default) context;
(setq mu4e-compose-context-policy 'ask) ;; ask for context if no context matches;

;; show images
(setq mu4e-view-show-images t
      mu4e-show-images t
      mu4e-view-image-max-width 800)

(setq mail-capture-templates `(("m" "Email Workflow")
                               ("mf" "Follow Up" entry (file+olp "~/Nextcloud/org/Mail.org" "Follow Up")
                                "* TODO %a")
                               ("mr" "Read Later" entry (file+olp "~/Nextcloud/org/Mail.org" "Read Later")
                                "* TODO %a")))

;; modify org-capture-templates
(setq org-capture-templates (append org-capture-templates mail-capture-templates))

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; general emacs mail settings; used when composing e-mail
;; then non-mu4e-* stuff is inherited from emacs/message-mode
;; (setq mu4e-reply-to-address "ingo@ingo-richter.io"
;;       user-mail-address "ingo@ingo-richter.io"
;;       user-full-name "Ingo Richter")
(setq mu4e-compose-signature (concat "Cheers,\n" "Ingo\n\n" "PGP Key: https://ingo-richter.io/pgp.key | Fingerprint"))
;; don't save message to SendMessages, IMAP takes care of this
                                        ; (setq mu4e-sent-messages-behavior 'delete)

;; spell check
(add-hook 'mu4e-compose-mode-hook
          (defun my-do-compose-stuff ()
            "My settings for message composition."
            (set-fill-column 72)
            (flyspell-mode)))

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; ;; store link to message if in header view, not to header query:
(setq org-mu4e-link-query-in-headers-mode nil)
;; don't have to confirm when quitting:
(setq mu4e-confirm-quit nil)
;; number of visible headers in horizontal split view:
(setq mu4e-headers-visible-lines 20)
;; don't show threading by default:
(setq mu4e-headers-show-threads nil)
;; hide annoying "mu4e Retrieving mail..." msg in mini buffer:
(setq mu4e-hide-index-messages t)
;; customize the reply-quote-string:
(setq message-citation-line-format "%N @ %Y-%m-%d %H:%M :\n")
;; M-x find-function RET message-citation-line-format for docs:
(setq message-citation-line-function 'message-insert-formatted-citation-line)
;; by default do not show related emails:
(setq mu4e-headers-include-related nil)
;; by default do not show threads:
(setq mu4e-headers-show-threads t)
