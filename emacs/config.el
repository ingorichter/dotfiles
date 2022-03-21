(require 'rational-completion)
(require 'rational-defaults)
(require 'rational-editing)
(require 'rational-persistence)
(require 'rational-project)
(require 'rational-screencast)
(require 'rational-ui)
(require 'rational-updates)
(require 'rational-use-package)
(require 'rational-windows)
(require 'rational-code-completion)

;; still my custom version
(require 'rational-git)

(custom-set-variables
   '(rational-ui-default-font
     '(:font "JetBrains Mono" :weight light :height 185)))

;; Set further font and theme customizations
;; (set-face-attribute 'default nil
;;                   :font "JetBrains Mono"
;;                   :weight 'light
;;                   :height 185)

;; load my customizations and stuff that I want to use
(load-file (expand-file-name "ingo-config.el" user-emacs-directory))

;; (load-theme 'modus-operandi t)
(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
  :bind ("<f6>" . modus-themes-toggle))
