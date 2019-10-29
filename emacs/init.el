(package-initialize)
;; Bootstrap 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;;(require 'package)
;;(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa2" . "http://www.mirrorservice.org/sites/melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa3" . "http://www.mirrorservice.org/sites/stable.melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/") t)
;; I've found the priority setting in this blog https://krsoninikhil.github.io/2018/12/15/easy-moving-from-vscode-to-emacs/
(setq package-archive-priorities
      '(("melpa" . 0)
        ("melpa3" . 5)
        ("gnu" . 10)))

;; load my config
(org-babel-load-file (expand-file-name "~/dotfiles/emacs/emacs-init.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "68bf77811b94a9d877f9c974c19bafe5b67b53ed82baf96db79518564177c0fb" default))
 '(fci-rule-color "#383838")
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(line-spacing 0.2)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-capture-templates
   '(("t" "To Do Item" entry
      (file+headline org-refile-location "Todo")
      "* TODO %^{Titel} %^g
  %?
  :LOGBOOK:
  - Added: %U
  :END:")
     ("a" "Appointment" entry
      (file org-gcal-location)
      "* %?

%^T

:PROPERTIES:

:END:

")
     ("n" "Note" entry
      (file+headline org-default-notes-file "Notes")
      "* Note %^{Titel} %^g
  %?
  :LOGBOOK:
  - Added: %U
  :END:")
     ("i" "Idee" entry
      (file+headline org-refile-location "Ideen")
      "* Idee %^{Titel} %^g
  %?
  :LOGBOOK:
  - Added: %U
  :END:")
     ("w" "Weekly Goals" entry
      (file+olp+datetree org-weeklygoals-location "Weekly Goals")
      "* %U

High Level Ziele fuer die x. Woche
 - [ ] $ x in die Spardose
 - [ ] Sport
 - [ ] Laufen" :empty-lines-before 1 :tree-type week)
     ("j" "Journal" entry
      (file+olp+datetree org-journal-location)
      "* %U - %?
  %i" :clock-in t :clock-resume t)) t)
 '(org-structure-template-alist
   '(("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse")))
 '(package-selected-packages
   '(keyfreq lsp-mode telephone-line rustic flycheck-rust cargo racer elfeed-org elfeed-goodies elfeed rainbow-delimiters crystal-mode buffer-expose nim-mode nim org-super-agenda benchmark-init minions moody solarized-theme smartparens-config projectile edit-indirect spaceline exec-path-from-shell yasnippet-snippets ivy rust-mode restclient treemacs magit-org-todos magit yasnippet org-pomodoro markdown-mode zenburn-theme easy-hugo mic-paren org-caldav org-dashboard org-plus-contrib org kaolin-themes spacemacs-theme nimbus-theme which-key try use-package org-bullets))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number-current-line ((t (:inherit default :background "OrangeRed1"))))
 '(telephone-line-accent-active ((t (:inherit mode-line :background "DarkOrange1" :foreground "white")))))
