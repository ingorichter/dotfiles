;; Use crafted-package-install-package

(require 'crafted-defaults)
(require 'crafted-updates)
(require 'crafted-osx)
(require 'crafted-editing)
(require 'crafted-project)
(require 'crafted-screencast)
(require 'crafted-ui)
(require 'crafted-package)
(require 'crafted-windows)
(require 'crafted-speedbar)
(require 'crafted-org)
(require 'crafted-compile)
(require 'crafted-completion)

;; still my custom version
(require 'crafted-git)
(require 'crafted-persistence)
(require 'crafted-org)
(require 'crafted-org-roam)
(require 'crafted-denote)

(custom-set-variables
   '(crafted-ui-default-font
     '(:font "Iosevka Comfy Fixed" :weight regular :height 185)))

;; load my customizations and stuff that I want to use
(load-file (expand-file-name "ingo-config.el" user-emacs-directory))

;; (load-theme 'modus-operandi t)
(crafted-package-install-package 'modus-themes)
;; Add all your customizations prior to loading the themes
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-region '(bg-only no-extend))

;; Load the theme files before enabling a theme
(modus-themes-load-themes)

;; Load the theme of your choice:
(modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)

(define-key global-map (kbd "<f6>") #'modus-themes-toggle)
