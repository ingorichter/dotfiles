(add-hook 'emacs-startup-hook
	  (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq gc-cons-threshold (* 50 1000 1000))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (and (fboundp 'server-running-p)
	     (server-running-p))
  (server-start))

(org-babel-load-file (expand-file-name "~/.emacs.d/Config.org"))
;;(setenv "GPG_AGENT_INFO" nil)

;; https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs
;; (setq custom-file "~/.emacs.d/custom.el")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
;; reset gc threshold
(setq gc-cons-threshold (* 2 1000 1000))

