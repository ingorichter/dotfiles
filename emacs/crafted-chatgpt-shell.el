;;; crafted-chatgpt-shell.el -*- lexical-binding: t; -*-

;; ChatGPT settings
;; https://github.com/xenodium/chatgpt-shell/blob/main/README.org

;; https://github.com/xenodium/chatgpt-shell
(crafted-package-install-package '(chatgpt-shell :type git :host github :repo "xenodium/chatgpt-shell"))

(defun read-password-from-keychain (service account)
  "Read password from the macOS keychain for a given service and account."
  (let* ((command (concat "security find-generic-password -s " service " -a " account " -w"))
         (password (shell-command-to-string command)))
    (string-trim-right password)))

(setq chatgpt-shell-openai-key
      (lambda ()
        (read-password-from-keychain "chatgpt" "ingo")))

(setq shell-maker-logging t)
(setq chatgpt-shell-streaming nil)

(provide 'crafted-chatgpt-shell)
;;; crafted-chatgpt-shell.el ends here
