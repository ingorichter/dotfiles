;;; init.el --- Crafted Emacs straight.el Example -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Example init.el when using the straight.el package manager.

;;; Code:

;;; Initial phase
;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; loading this file will also define the 'crafted-emacs-home' variable 
(load (expand-file-name craftedEmacsInitConfig
                        user-emacs-directory))

;; add my custom modules and load the
(add-to-list 'load-path (concat user-emacs-directory "custom-modules"))

;;; Packages phase
(require 'crafted-completion-packages)
(require 'crafted-defaults-config)
(require 'crafted-ide-packages)
(require 'crafted-org-packages)
(require 'crafted-startup-config)
(require 'crafted-ui-packages)
(require 'crafted-updates-config)

;;; ensure that all packages are installed before doing any configuration
(package-install-selected-packages :noconfirm)

;; Use the crafted-package helper
(crafted-package-install-selected-packages)

;;; Configuration phase
(require 'crafted-completion-config)
(require 'crafted-defaults-config)
(require 'crafted-ide-config)
(require 'crafted-org-config)
(require 'crafted-osx-config)
(require 'crafted-startup-config)
(require 'crafted-ui-config)

;;; Optional configuration
(require 'custom-module-config)

;;; _
(provide 'init)
;;; init.el ends here
