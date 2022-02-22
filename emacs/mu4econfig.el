;; (add-to-list 'load-path "/usr/local/Cellar/mu/1.6.8/share/emacs/site-lisp/mu/mu4e/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
;;(require 'smtpmail)

(use-package async
  :ensure t
  :config (require 'smtpmail-async))
;; (use-package smtpmail-async)

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

(setq mu4e-mu-binary "/usr/local/bin/mu")
(setq mu4e-maildir (expand-file-name "~/Mail"))

(setq mu4e-drafts-folder "/ingo-richter.io/Drafts")
(setq mu4e-sent-folder "/ingo-richter.io/Sent")
(setq mu4e-trash-folder "/ingo-richter.io/Trash")
(setq message-signature-file "~/dotfiles/signature")

(setq mail-user-agent 'mu4e-user-agent)

;; get mail
(setq mu4e-get-mail-command "mbsync -a"
      mu4e-html2text-command "w3m -T text/html"
      mu4e-update-interval 120
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil)

(setq mu4e-maildir-shortcuts
      '( ("/INBOX" . ?i)
         ("/Sent Items" . ?s)
         ("/Trash" . ?t)
         ("/Drafts" . ?d)))

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

;; mbsync avoid duplicate UIDs
;; https://www.tomica.net/blog/2020/03/replacing-offlineimap-with-mbsync-isync
(setq mu4e-change-filenames-when-moving t)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; general emacs mail settings; used when composing e-mail
;; then non-mu4e-* stuff is inherited from emacs/message-mode
(setq mu4e-reply-to-address "ingo@ingo-richter.io"
      user-mail-address "ingo@ingo-richter.io"
      user-full-name "Ingo Richter")
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

