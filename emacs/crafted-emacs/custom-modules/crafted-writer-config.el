;;; crafted-writer-config.el --- Org config              -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: hypermedia, convenience

;;; Code:

;; Get rid of some visual noise
(tool-bar-mode 0)

;; Set up a nice color theme
(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-duo-dark t))

(use-package spacious-padding
  :ensure t
  :custom
  (spacious-padding-subtle-mode-line t)
  (spacious-padding-widths
      '( :internal-border-width 30
         :header-line-width 4
         :mode-line-width 10
         :tab-width 4
         :right-divider-width 30
         :scroll-bar-width 8
         :fringe-width 8))
  :config
  (spacious-padding-mode 1))

(use-package fontaine
  :ensure t
  :custom
  (fontaine-presets
   '((regular
      :default-family "JetBrains Mono"
      :default-weight normal
      :default-height 150
      :fixed-pitch-family "JetBrains Mono"
      :fixed-pitch-weight nil ; falls back to :default-weight
      :fixed-pitch-height 1.0
      :variable-pitch-family "Iosevka Aile"
      :variable-pitch-weight normal
      :variable-pitch-height 1.2
      :line-spacing 1)
     (large
      :inherit regular
      :default-height 175
      :variable-pitch-height 1.3)))
  :config
  (add-hook 'enable-theme-functions #'fontaine-apply-current-preset)
  (fontaine-set-preset 'regular))

(use-package org
  :ensure nil)

(use-package variable-pitch-mode
  :ensure nil
  :hook
  (org-mode . variable-pitch-mode))

(use-package visual-line-mode
  :ensure nil
  :hook
  (org-mode . visual-line-mode))

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-width 110))

(use-package logos
  :ensure t
  :custom
  (logos-outlines-are-pages t)
  (logos-hide-mode-line t))

(defun ir/dired-jump-sidebar ()
  (interactive)
  (display-buffer-in-side-window
   (dired-noselect default-directory)
   '((side . left))))

(use-package wc-mode
  :ensure t)

(use-package tmr
  :ensure t)

(provide 'crafted-writer-config)
;;; crafted-writer-config.el ends here
