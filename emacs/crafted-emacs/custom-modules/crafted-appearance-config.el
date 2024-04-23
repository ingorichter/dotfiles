;;; crafted-appearance-config.el --- Org config              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: hypermedia, convenience

;;; Code:

(require 'fontaine)

(setq fontaine-latest-state-file
      (locate-user-emacs-file "fontaine-latest-state.eld"))

;; Iosevka Comfy is my highly customised build of Iosevka with
;; monospaced and duospaced (quasi-proportional) variants as well as
;; support or no support for ligatures:
;; <https://github.com/protesilaos/iosevka-comfy>.
(setq fontaine-presets
      '((small
         :default-family "Iosevka Comfy Motion"
         :default-height 80
         :variable-pitch-family "Iosevka Comfy Duo")
        (regular) ; like this it uses all the fallback values and is named `regular'
        (medium
         :default-weight semilight
         :default-height 140
         :bold-weight extrabold)
        (large
         :inherit medium
         :default-height 200)
        (presentation
         :default-height 240)
        (t
         ;; I keep all properties for didactic purposes, but most can be
         ;; omitted.  See the fontaine manual for the technicalities:
         ;; <https://protesilaos.com/emacs/fontaine>.
         :default-family "Iosevka Comfy"
         :default-weight regular
         :default-height 100

         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0

         :fixed-pitch-serif-family nil ; falls back to :default-family
         :fixed-pitch-serif-weight nil ; falls back to :default-weight
         :fixed-pitch-serif-height 1.0

         :variable-pitch-family "Iosevka Comfy Motion Duo"
         :variable-pitch-weight nil
         :variable-pitch-height 1.0

         :mode-line-active-family nil ; falls back to :default-family
         :mode-line-active-weight nil ; falls back to :default-weight
         :mode-line-active-height 0.9

         :mode-line-inactive-family nil ; falls back to :default-family
         :mode-line-inactive-weight nil ; falls back to :default-weight
         :mode-line-inactive-height 0.9

         :header-line-family nil ; falls back to :default-family
         :header-line-weight nil ; falls back to :default-weight
         :header-line-height 0.9

         :line-number-family nil ; falls back to :default-family
         :line-number-weight nil ; falls back to :default-weight
         :line-number-height 0.9

         :tab-bar-family nil ; falls back to :default-family
         :tab-bar-weight nil ; falls back to :default-weight
         :tab-bar-height 1.0

         :tab-line-family nil ; falls back to :default-family
         :tab-line-weight nil ; falls back to :default-weight
         :tab-line-height 1.0

         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold

         :italic-family nil
         :italic-slant italic

         :line-spacing nil)))

;; Set the last preset or fall back to desired style from `fontaine-presets'
;; (the `regular' in this case).
(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

;; Persist the latest font preset when closing/starting Emacs and
;; while switching between themes.
(fontaine-mode 1)

;; fontaine does not define any key bindings.  This is just a sample that
;; respects the key binding conventions.  Evaluate:
;;
;;     (info "(elisp) Key Binding Conventions")
(define-key global-map (kbd "C-c f") #'fontaine-set-preset)

(provide 'crafted-appearance-config)
;;; crafted-appearance-config.el ends here
