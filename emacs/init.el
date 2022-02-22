;;; init.el --- Emacs configuration file. -*- lexical-binding: t; -*-
;;; Commentary:
;; TODO
;; 36 code blocks tangled and written config.el 36x!!!

(setq vc-follow-symlinks nil)

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda ()
                               (setq gc-cons-threshold (* 32 1000 1000))))
;; (add-hook 'after-focus-change-function 'garbage-collect)
(run-with-idle-timer 5 t 'garbage-collect)

(add-hook 'emacs-startup-hook
	        (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; package management
(require 'package)

(add-to-list 'package-archives
	           '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq package-enable-at-startup nil)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(unless (and (fboundp 'server-running-p)
	           (server-running-p))
  (server-start))

(org-babel-load-file (expand-file-name "~/.emacs.d/Config.org"))
;;(setenv "GPG_AGENT_INFO" nil)

;; https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs
;; (setq custom-file "~/.emacs.d/custom.el")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
(load custom-file))
;; reset gc threshold
;; (setq gc-cons-threshold (* 2 1000 1000))
