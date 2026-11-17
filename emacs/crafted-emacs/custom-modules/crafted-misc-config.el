;;; crafted-misc-config.el --- My custom functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ingo Richter

;; Author: Ingo Richter <ingorichter@Ingos-Personal-MacBook-Pro.local>
;; Keywords: lisp

;;; Commentary:
;; This file contains misc packages.

;;; Code:

(require 'use-package)

(use-package tokyo-night
  :defer t
  :vc (:url "https://github.com/bbatsov/tokyo-night-emacs.git"
       :rev :newest))

(defun ir/apply-theme (appearance)
  "Load theme based on APPEARANCE (light or dark)."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'tokyo-night-day t))
    ('dark (load-theme 'tokyo-night t))))

(add-hook 'ns-system-appearance-change-functions #'ir/apply-theme)

(provide 'crafted-misc-config)
;;; crafted-misc-config.el ends here
