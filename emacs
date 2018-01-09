					; -*-Lisp-*-
;; Configure MELPA to choose from more plugins
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq org-directory "~/Nextcloud/org")
(setq org-archive-location (concat org-directory "/archive/%s_archive::"))
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-capture-templates '(("t" "To Do Item" entry
 			       (file+headline (concat org-directory "/refile.org") "Todo")
			       "* TODO %^{Titel} %^g\n  %?\n  :LOGBOOK:\n  - Added: %U\n  :END:")
			      ("n" "Note" entry
			       (file+headline org-default-notes-file "Notes")
			       "* Note %^{Titel} %^g\n  %?\n  :LOGBOOK:\n  - Added: %U\n  :END:")
			      ("i" "Idee" entry
			       (file+headline (concat org-directory "/refile.org") "Ideen")
			       "* Idee %^{Titel} %^g\n  %?\n  :LOGBOOK:\n  - Added: %U\n  :END:")
			      ("w" "Weekly Goals" entry
			       (file+datetree (concat org-directory "/weekly-goals.org") "Weekly Goals")
			       "* %U\n\nHigh Level Ziele fuer die x. Woche\n - [ ] $ x in die Spardose\n - [ ] Sport\n - [ ] Laufen")
 			      ("j" "Journal" entry
 			       (file+datetree (concat org-directory "/journal/journal.org"))
 			       "* %U - %?\n  %i" :clock-in t :clock-resume t)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (list org-directory))
 '(package-install-selected-packages (quote (which-key try use-package org-bullets)))
 '(package-selected-packages (quote (which-key try use-package org-bullets))))
(package-install-selected-packages)

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

