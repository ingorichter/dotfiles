;;; config-org.el -*- lexical-binding: t; -*-

;; Org-Mode
;; basic settings are made from ~/.emacs.d/modules/rational-org.el
(setq org-appear-autolinks t)

(rational-package-install-package 'org-contrib)

(straight-use-package 'ob-crystal)
(straight-use-package 'org)

(require 'org-protocol)

(setq org-directory "~/Nextcloud/org"
      org-agenda-files (list org-directory)
      org-archive-location (concat org-directory "/archive/%s_archive::")
      org-default-notes-file (concat org-directory "/notes.org")
      org-default-todo-file (concat org-directory "/mylife.org")
      org-refile-location (concat org-directory "/refile.org")
      org-gcal-location (concat org-directory "/gcal.org")
      org-weekly-goals (concat org-directory "/weekly-goals.org")
      org-goals (concat org-directory "/goals.org")
      org-journal-location (concat org-directory "/journal/journal.org"))

(setq org-capture-templates '(("t" "To Do Item" entry (file+headline org-refile-location "Todo")
                               "* TODO %^{Titel} %^g\n %?\n\n:LOGBOOK:\n - Added: %U\n:END:")
                              ("a" "Appointment" entry (file org-gcal-location)
                               "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
                              ("n" "Note" entry (file+headline org-default-notes-file "Notes")
                               "* Note %^{Titel} %^g\n  %?\n  :LOGBOOK:\n - Added: %U\n:END:")
                              ("i" "Idee" entry (file+headline org-refile-location "Ideen")
                               "* Idee %^{Titel} %^g\n  %?\n  :LOGBOOK:\n - Added: %U\n:END:")
                              ("w" "Weekly Goals" entry
                               (file+datetree org-weekly-goals (format-time-string "%Y"))
                               "* %U\n\nHigh Level Ziele fuer die %(format-time-string "%W"). Woche\n - [ ] %(format-time-string "%W")$ x in die Spardose\n - [ ] Workout\n - [ ] Laufen")
                              ("j" "Journal" entry (file+datetree org-journal-location)
                               "* %U - %?\n  %i" :clock-in t :clock-resume t)))

;; ;; use org-bullets for nicer formatting
;; (straight-use-package 'org-bullets)
;; (setq org-bullets-bullet-list '("●" "◎" "○" "◆" "◇" "✸" "•"))
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; ;; (use-package org-bullets
;; ;;   :ensure t
;; ;;   :config
;; ;;   (add-hook 'org-mode-hook (lambda() (org-bullets-mode 1))))

;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-font-lock-mode 1)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;; have a timestamp added to finished items
(setq org-log-done 'time)

;; ;; capture templates
;; (setq org-todo-keywords
;;       (quote (
;;               (sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
;;               (sequence "WAITING(W@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
;; ;; this was mentioned in http://howardism.org/Technical/Emacs/literate-programming-tutorial.html
;; (setq org-confirm-babel-evaluate nil
;;       org-src-fontify-natively t
;;       org-src-tab-acts-natively t)

;; (setq org-hide-emphasis-markers t
;;       org-fontify-done-headline t
;;       org-hide-leading-stars t
;;       org-pretty-entities t
;;       org-odd-levels-only t)

;; (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "†")
;;                                        ("#+END_SRC" . "†")
;;                                        ("#+begin_src" . "†")
;;                                        ("#+end_src" . "†")
;;                                        (">=" . "≥")
;;                                        ("=>" . "⇨")))
;; (setq prettify-symbols-unprettify-at-point 'right-edge)
;; (add-hook 'org-mode-hook 'prettify-symbols-mode)

;; minad/org-modern
(straight-use-package '(org-modern :type git :host github :repo "minad/org-modern"))
;; Add frame borders and window dividers
;; (modify-all-frames-parameters
;;  '((right-divider-width . 40)
;;    (internal-border-width . 40)))
;; (dolist (face '(window-divider
;;                 window-divider-first-pixel
;;                 window-divider-last-pixel))
;;   (face-spec-reset-face face)
;;   (set-face-foreground face (face-attribute 'default :background)))
;; (set-face-background 'fringe (face-attribute 'default :background))

;; Choose some fonts
(set-face-attribute 'default nil :family "Iosevka")
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
(set-face-attribute 'org-modern-symbol nil :family "Iosevka")

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 ;; org-ellipsis "…"

 ;; Agenda styling
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")

;; Enable org-modern-mode
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; ;; (custom-theme-set-faces
;; ;; 'user
;; ;; '(variable-pitch ((t (:family "Source Sans Pro" :height 120 :weight light))))
;; ;; '(fixed-pitch ((t ( :family "Consolas" :slant normal :weight normal :height 0.9 :width normal)))))
;; (custom-theme-set-faces
;;  'user
;;  '(org-code ((t (:inherit (shadow fixed-pitch))))))

;; ;; (custom-theme-set-faces
;; ;; 'user
;; ;; '(org-block                 ((t (:inherit fixed-pitch))))
;; ;;  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;; ;;  '(org-property-value        ((t (:inherit fixed-pitch))) t)
;; ;;  '(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; ;;  '(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold))))
;; ;;  '(org-verbatim              ((t (:inherit (shadow fixed-pitch))))))

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((shell      . t)
;;    (js         . t)
;;    (emacs-lisp . t)
;;    (perl       . t)
;;    (crystal    . t)
;;    (clojure    . t)
;;    (python     . t)
;;    (ruby       . t)
;;    (dot        . t)
;;    (css        . t)
;;    (plantuml   . t)))

;; (straight-use-package 'org-ql)

;; ;; org-journal
(straight-use-package 'org-journal)
(setq org-journal-dir "~/Nextcloud/org/journal/")
(setq org-journal-date-format "%A, %d %B %Y")

(defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
   (pcase org-journal-file-type
     (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
     (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
     (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
     (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))
(setq org-journal-file-header 'org-journal-file-header-func)

(provide 'config-org)
