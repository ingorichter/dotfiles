(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name "~/.emacs.d/Config.org"))
(setenv "GPG_AGENT_INFO" nil)

  ;; https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs
;; (setq custom-file "~/.emacs.d/custom.el")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

