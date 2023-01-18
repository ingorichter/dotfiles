;; Use crafted-package-install-package

(require 'crafted-defaults)
(require 'crafted-package)
(require 'crafted-updates)
(require 'crafted-compile)
(require 'crafted-ide)
(require 'crafted-completion)
(require 'crafted-editing)
(require 'crafted-org)
(require 'crafted-osx)
(require 'crafted-project)
(require 'crafted-screencast)
(require 'crafted-speedbar)
(require 'crafted-ui)
(require 'crafted-windows)

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

(crafted-package-install-package 'ef-themes)
(setq ef-themes-to-toggle '(ef-winter ef-frost))
(setq ef-themes-mixed-fonts t
      ef-themes-variable-pitch-ui t)
;; Load the theme of choice:
(load-theme 'ef-winter :no-confirm)
(define-key global-map (kbd "<f6>") #'ef-themes-toggle)
