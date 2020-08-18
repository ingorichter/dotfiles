(add-to-list 'load-path "/usr/local/Cellar/mu/1.4.13/share/emacs/site-lisp/mu/mu4e/")
(require 'smtpmail)

;; (use-package async
;;   :ensure t
;;   :config (require 'smtpmail-async))

;; smtp
(setq message-send-mail-function 'smtpmail-send-it
;;      smtpmail-starttls-credentials
;;      '(("mx.ingo-richter.io" 587 nil nil))
      smtpmail-default-smtp-server "mx.ingo-richter.io"
      smtpmail-smtp-server "mx.ingo-richter.io"
      smtpmail-smtp-service 587
      smtpmail-stream-type  'starttls
      smtpmail-debug-info t)

(require 'mu4e)

(setq mu4e-mu-binary "/usr/local/bin/mu")
(setq mu4e-maildir (expand-file-name "~/Mail"))

(setq mu4e-drafts-folder "/ingo-richter.io/Drafts")
(setq mu4e-sent-folder "/ingo-richter.io/Sent")
(setq mu4e-trash-folder "/ingo-richter.io/Trash")
(setq message-signature-file "~/dotfiles/signature")

(setq mail-user-agent 'mu4e-user-agent)

; get mail
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
(setq mu4e-show-images t)

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

;; don't save message to SendMessages, IMAP takes care of this
                                        ; (setq mu4e-sent-messages-behavior 'delete)

;; spell check
(add-hook 'mu4e-compose-mode-hook
          (defun my-do-compose-stuff ()
            "My settings for message composition."
            (set-fill-column 72)
            (flyspell-mode)))

