					; -*-Lisp-*-
;; Configure MELPA to choose from more plugins
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; fix keyboard on macOS
(global-set-key "\M-(" (lambda () (interactive) (insert "{")))
(global-set-key "\M-)" (lambda () (interactive) (insert "}")))

(global-set-key "\M-8" (lambda () (interactive) (insert "[")))
(global-set-key "\M-9" (lambda () (interactive) (insert "]")))

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
    (edit-indirect spaceline exec-path-from-shell yasnippet-snippets yasnippets-snippets ivy rust-mode restclient treemacs magit-org-todos magit yasnippet org-pomodoro markdown-mode zenburn-theme easy-hugo mic-paren org-caldav org-dashboard org-plus-contrib org kaolin-themes spacemacs-theme nimbus-theme which-key try use-package org-bullets)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
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
 '(vc-annotate-very-old-color "#DC8CC3")
 '(yas-global-mode t))

(package-install-selected-packages)

;; Font settings
;; (set-default-font "Consolas-24")
(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "IBM Plex Mono Medium 24"))
;; (set-frame-parameter frame 'font "Consolas for Powerline 24"))
;; Fontify current frame
(fontify-frame nil)
;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions) 

;; modify the exec-path to find system apps
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Global word wrap
(visual-line-mode t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Autocomplete with ivy
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

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
  (setq easy-hugo-basedir "~/develop/fun/OSS/ingorichter.io-website/")
  (setq easy-hugo-url "https://ingo-richter.io")
  (setq easy-hugo-sshdomain "blogdomain")
  (setq easy-hugo-root "/")
  (setq easy-hugo-previewtime "300")
  :bind ("C-c C-e" . easy-hugo))

;; yasnippet plugin
(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
      '("~/dotfiles/yasnippets"
	"~/.emacs.d/snippets"                 ;; personal snippets
        )))

(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.

(use-package yasnippet-snippets
  :ensure t)

;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

;; counsel
(use-package counsel
  :ensure t)

;; counsel-projectile
;; (use-package counsel-projectile
;;   :ensure t)

;; treemacs
(use-package treemacs
  :ensure t)

;; restclient
(use-package restclient
  :ensure t)

(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-light t)
  (setq spacemacs-theme-org-agenda-height nil)
  (setq spacemacs-theme-org-height nil))

;; set sizes here to stop spacemacs theme resizing these
(set-face-attribute 'org-level-1 nil :height 1.0)
(set-face-attribute 'org-level-2 nil :height 1.0)
(set-face-attribute 'org-level-3 nil :height 1.0)
(set-face-attribute 'org-scheduled-today nil :height 1.0)
(set-face-attribute 'org-agenda-date-today nil :height 1.1)
(set-face-attribute 'org-table nil :foreground "#008787")

(use-package spaceline
  :ensure t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

;; script experiments
(defun notes ()
  "Switch to my notes dir"
  (interactive)
  (find-file org-directory)
  )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to wrap blocks of text in org templates                       ;;
;; e.g. latex or src etc                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-begin-template ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                      ("e" . "EXAMPLE")
                      ("q" . "QUOTE")
                      ("v" . "VERSE")
                      ("c" . "CENTER")
                      ("l" . "LaTeX")
                      ("h" . "HTML")
                      ("a" . "ASCII")))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "#+END_" choice "\n")
                (goto-char start)
                (insert "#+BEGIN_" choice "\n")))
             (t
              (insert "#+BEGIN_" choice "\n")
              (save-excursion (insert "#+END_" choice))))))))))

;;bind to key
;;(define-key org-mode-map (kbd "C-<") 'org-begin-template)

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'tree))

;;
(global-visual-line-mode t)
