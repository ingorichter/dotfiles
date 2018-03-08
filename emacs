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
 '(custom-safe-themes
   (quote
    ("e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" default)))
 '(org-agenda-files (list org-directory))
 '(package-install-selected-packages (quote (which-key try use-package org-bullets)))
 '(package-selected-packages
   (quote
    (zenburn-theme easy-hugo mic-paren org-caldav org-dashboard org-plus-contrib org kaolin-themes spacemacs-theme nimbus-theme which-key try use-package org-bullets))))
(package-install-selected-packages)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun fontify-frame (frame)
    (set-frame-parameter frame 'font "Consolas-24"))
;; Fontify current frame
(fontify-frame nil)
;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

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
  (setq org-caldav-url "https://cloud.familie-richter.homelinux.com/remote.php/dav/calendars/ingo"
	org-caldav-calendar-id "main"
	org-caldav-inbox "~/Nextcloud/org/gcal.org"
	org-caldav-files org-agenda-files
	org-icalendar-timezone "America/Los_Angeles"
	org-icalendar-date-time-format ";TZID=%Z:%Y%m%dT%H%M%S"))

;; (use-package org-gcal
;;   :ensure t
;;   :config
;;   (setq org-gcal-client-id "90735309152-0rctd74sppmea7ocb2kgok4vfh1f1o99.apps.googleusercontent.com"
;; 	org-gcal-client-secret "1-x-LPwlf9wEyoV3oj3PFH7h"
;; 	org-gcal-file-alist '(("ingo.richter@gmail.com" . "~/Nextcloud/org/gcal.org"))))

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
