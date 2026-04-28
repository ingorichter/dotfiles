
;;; early-init.el --- straight.el Example -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Example early-init.el for using the straight.el package manager.
;; This does *not* load crafted-early-init-config
;; (which would normally bootstrap package.el).

;;; Code:

(setq craftedEmacsInitConfig (concat
			      (getenv "CRAFTED_EMACS_HOME")
			      "/modules/crafted-init-config"))

;;; Set up crafted-package (before straight.el to avoid package.el load-order warning)
;; Configure crafted-emacs to use straight as package manager.
;; See `(info "(crafted-emacs)Using alternate package managers")'
(setq package-enable-at-startup nil)
(load (concat (getenv "CRAFTED_EMACS_HOME") "/modules/crafted-package-config"))

(setq crafted-package-system 'straight)
(setq crafted-package-installer #'straight-use-package)
(setq crafted-package-installed-predicate #'straight--installed-p)

;; Suppress straight's package.el warning — elpa dir exists from old setup
;; but we use straight exclusively. straight checks (file-exists-p package-user-dir).
(setq straight-package--warning-displayed t)

;;; Bootstrap straight.el
;; See https://github.com/radian-software/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; _
(provide 'early-init)
;;; early-init.el ends here
