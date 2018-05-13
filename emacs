					; -*-Lisp-*-
;; Configure MELPA to choose from more plugins
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(setq org-directory "~/Nextcloud/org")

(setq org-archive-location (concat org-directory "/archive/%s_archive::")
      org-default-notes-file (concat org-directory "/notes.org")
      org-refile-location (concat org-directory "/refile.org")
      org-gcal-location (concat org-directory "/gcal.org")
      org-weeklygoals-location (concat org-directory "/weekly-goals.org")
      org-journal-location (concat org-directory "/journal/journal.org")
      org-capture-templates '(("t" "To Do Item" entry
 			       (file+headline org-refile-location "Todo")
			       "* TODO %^{Titel} %^g\n  %?\n  :LOGBOOK:\n  - Added: %U\n  :END:")
			      ("a" "Appointment" entry (file org-gcal-location)
			       "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
			      ("n" "Note" entry
			       (file+headline org-default-notes-file "Notes")
			       "* Note %^{Titel} %^g\n  %?\n  :LOGBOOK:\n  - Added: %U\n  :END:")
			      ("i" "Idee" entry
			       (file+headline org-refile-location "Ideen")
			       "* Idee %^{Titel} %^g\n  %?\n  :LOGBOOK:\n  - Added: %U\n  :END:")
			      ("w" "Weekly Goals" entry
			       (file+datetree org-weeklygoals-location "Weekly Goals")
			       "* %U\n\nHigh Level Ziele fuer die x. Woche\n - [ ] $ x in die Spardose\n - [ ] Sport\n - [ ] Laufen")
			      ("j" "Journal" entry
			       (file+datetree org-journal-location)
			       "* %U - %?\n  %i" :clock-in t :clock-resume t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("7f6796a9b925f727bbe1781dc65f7f23c0aa4d4dc19613aa3cf96e41a96651e4" "8044e010182a07e3f6602a75b0d130f3e55d492cb76af843d45f6b6f81152bbd" "1ab3ace61bc100c813aa8f87a9e05628b93dbc6b6d8e635774f034dc1c382d92" "fec6c786b1d3088091715772839ac6051ed972b17991af04b50e9285a98c7463" "8ad35d6c2b35eacc328b732f0a4fe263abd96443a5075aa53b8535a9e8cb7eaf" "293b55c588c56fe062afe4b7a3a4b023712a26d26dc69ee89c347b30283a72eb" "9a58c408a001318ce9b4eab64c620c8e8ebd55d4c52327e354f24d298fb6978f" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" default)))
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files (list org-directory))
 '(package-install-selected-packages (quote (which-key try use-package org-bullets)))
 '(package-selected-packages
   (quote
    (rust-mode restclient treemacs projectile magit-org-todos magit yasnippet org-pomodoro markdown-mode zenburn-theme easy-hugo mic-paren org-caldav org-dashboard org-plus-contrib org kaolin-themes spacemacs-theme nimbus-theme which-key try use-package org-bullets)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(projectile-mode t nil (projectile))
 '(show-paren-mode t)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
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
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))

(package-install-selected-packages)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; (defun fontify-frame (frame)
;;   (set-frame-parameter frame 'font "Consolas-24"))
;; Fontify current frame
;; (fontify-frame nil)
;; Fontify any future frames
;; (push 'fontify-frame after-make-frame-functions)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; (require 'helm-config)

(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

(define-key global-map "\C-cc" 'org-capture)

;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-font-lock-mode 1)
					; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))


;; have timestamp added to finished items
(setq org-log-done 'time)

;; capture templates
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	      (sequence "WAITING(W@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
	      ("NEXT" :foreground "blue" :weight bold))))

(setq package-check-signature nil)

;; Setup Calendar sync
(use-package org-caldav
  :ensure t
  :config
  (setq org-caldav-url "https://familie-richter.synology.me/remote.php/dav/calendars/ingo"
	org-caldav-calendar-id "main"
	org-caldav-inbox "~/Nextcloud/org/gcal.org"
	org-caldav-files org-agenda-files
	org-icalendar-timezone "America/Los_Angeles"
	org-icalendar-date-time-format ";TZID=%Z:%Y%m%dT%H%M%S"))

(add-hook 'org-agenda-mode-hook (lambda () (org-caldav-sync) ))
(add-hook 'org-capture-after-finalize-hook (lambda () (org-caldav-sync) ))

;; Fancy Parens Matching
;; (setq paren-dont-touch-blink t)
(use-package mic-paren
  :ensure t
  :config
  (paren-activate)
  (setq paren-match-face 'highlight)
  (setq paren-sexp-mode t))

;; ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; markdown
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; hugo plugin
(use-package easy-hugo
  :ensure t
  :init
  (setq easy-hugo-basedir "~/bookshelf/")
  (setq easy-hugo-url "https://ingo-richter.io")
  (setq easy-hugo-sshdomain "blogdomain")
  (setq easy-hugo-root "~/develop/fun/OSS/ingorichter.io-website/")
  (setq easy-hugo-previewtime "300")
  :bind ("C-c C-e" . easy-hugo))

;; yasnippet plugin
(use-package yasnippet
  :ensure t)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        "/dotfiles/snippets/"           ;; foo-mode and bar-mode snippet collection
        "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
        ))

(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.

;; projectile
(use-package projectile
  :ensure t)

;; treemacs
(use-package treemacs
  :ensure t)

;; restclient
(use-package restclient
  :ensure t)

;; script experiements
(defun notes ()
  "Switch to my notes dir"
  (interactive)
  (find-file org-directory)
  )
