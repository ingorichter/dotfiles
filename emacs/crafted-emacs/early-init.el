
;;; early-init.el --- Crafted Emacs package setup -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Initialize the native package system for Crafted Emacs.

;;; Code:

(setq craftedEmacsInitConfig (concat
			      (getenv "CRAFTED_EMACS_HOME")
			      "/modules/crafted-init-config"))

(load (concat (getenv "CRAFTED_EMACS_HOME") "/modules/crafted-early-init-config"))
(load (concat (getenv "CRAFTED_EMACS_HOME") "/modules/crafted-package-config"))

;;; _
(provide 'early-init)
;;; early-init.el ends here
