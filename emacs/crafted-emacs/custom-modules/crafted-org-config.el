;;; crafted-org-config.el --- Org config              -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: hypermedia, convenience

;;; Code:

;; -----------------------------------------------------------------------------
;; Package Requirements
;; -----------------------------------------------------------------------------
(require 'org-web-tools)
(require 'org-protocol)
(require 'org-bullets)
(require 'ob-plantuml)
(require 'org-contrib)
(require 'org-checklist)

;; -----------------------------------------------------------------------------
;; Org Directory and File Locations
;; -----------------------------------------------------------------------------
(defcustom crafted/org-directory (expand-file-name "~/Nextcloud/org")
  "Base directory for all Org files."
  :type 'directory
  :group 'crafted-org)

(setq org-directory crafted/org-directory
      org-agenda-files (list org-directory)
      org-archive-location (expand-file-name "archive/%s_archive::" org-directory)
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      org-default-todo-file (expand-file-name "mylife.org" org-directory)
      org-refile-location (expand-file-name "refile.org" org-directory)
      org-gcal-location (expand-file-name "gcal.org" org-directory)
      org-weekly-goals (expand-file-name "weekly-goals.org" org-directory)
      org-goals (expand-file-name "goals.org" org-directory)
      org-journal-location (expand-file-name "journal/journal.org" org-directory)
      org-review-location (expand-file-name "review.org" org-directory)
      org-cicada-daily-status-location (expand-file-name "cicada-daily-standup-status.org" org-directory))

;; -----------------------------------------------------------------------------
;; Keybindings
;; -----------------------------------------------------------------------------
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c w l") 'org-web-tools-insert-link-for-url)

;; -----------------------------------------------------------------------------
;; Org Appearance and Behavior
;; -----------------------------------------------------------------------------
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-insert-heading-respect-content t)
(setq org-M-RET-may-split-line '((default . nil)))

(setq org-bullets-bullet-list '("●" "◎" "○" "◆" "◇" "✸" "•"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; -----------------------------------------------------------------------------
;; Custom Agenda Commands
;; -----------------------------------------------------------------------------
(setq org-agenda-custom-commands
      '(("p" "Planning"
         ((tags-todo "+@planning"
                     ((org-agenda-overriding-header "Planning Tasks")))
           (tags-todo "-{.*}"
                      ((org-agenda-overriding-header "Untagged Tasks")))))
      ("i" "Inbox"
       ((todo ".*" ((org-agenda-files `(,org-refile-location)))
              (org-agenda-overriding-header "Unprocessed Inbox Items"))))))

;; -----------------------------------------------------------------------------
;; Org Capture Templates
;; -----------------------------------------------------------------------------
(setq org-capture-templates
      '(("l" "A link, for reading later." entry
         (file+headline "notes-new.org" "Reading List")
         "* %:description\n%u\n\n%c\n\n%i"
         :empty-lines 1)
        ("i" "Idea" entry (file+headline org-refile-location "Ideas")
         "* Idea %^{Title} %^g\n  :LOGBOOK:\n - Added: %U\n:END:")))

;; (setq org-capture-templates '(("t" "To Do Item" entry (file+headline org-refile-location "Inbox")
;;                                "* TODO %^{Titel} %^g\n %?\n\n:LOGBOOK:\n - Added: %U\n:END:")
;;                               ("a" "Appointment" entry (file org-gcal-location)
;;                                "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
;;                               ("n" "Note" entry (file+headline org-default-notes-file "Notes")
;;                                "* Note %^{Titel} %^g\n  %?\n  :LOGBOOK:\n - Added: %U\n:END:")
;;                               ("i" "Idee" entry (file+headline org-refile-location "Ideen")
;;                                "* Idee %^{Titel} %^g\n  %?\n  :LOGBOOK:\n - Added: %U\n:END:")
;;                               ("g" "Weekly Goals" entry
;;                                (file+datetree org-weekly-goals (format-time-string "%Y"))
;;                                "* %U\n\nHigh Level Ziele fuer die %(format-time-string "%W"). Woche\n - [ ] %(format-time-string "%W")$ x in die Spardose\n - [ ] Workout\n - [ ] Laufen")
;;                               ("j" "Journal" entry (file+datetree org-journal-location)
;;                                "* %U - %?\n  %i" :clock-in t :clock-resume t)
;;                               ("s" "Team Status Update" entry (file+olp+datetree org-cicada-daily-status-location) "* Daily Status\n")
;;                               ("d" "Review: Daily Review" entry
;;                                (file+olp+datetree org-review-location)
;;                                (file "~/Nextcloud/org/BASB/review/dailyreviewtemplate.org"))
;;                               ("w" "Review: Weekly Review" entry
;;                                (file+olp+datetree org-review-location)
;;                                (file "~/Nextcloud/org/BASB/review/weeklyreviewtemplate.org"))))

;; -----------------------------------------------------------------------------
;; Refile Settings
;; -----------------------------------------------------------------------------
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
(setq org-refile-allow-creating-parent-nodes 'confirm)

(org-reload)

;; -----------------------------------------------------------------------------
;; PlantUML Integration
;; -----------------------------------------------------------------------------
(let ((plantuml-exec (or (executable-find "plantuml")
                         "/opt/homebrew/bin/plantuml")))
  (if (and plantuml-exec (file-exists-p plantuml-exec))
      (progn
        (setq org-plantuml-executable-path plantuml-exec)
        (setq org-plantuml-exec-mode 'plantuml)
        (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
        (add-to-list 'org-babel-load-languages '(plantuml . t)))
    (warn "PlantUML executable not found. PlantUML support in org-babel will be disabled.")))

(provide 'crafted-org-config)
;;; crafted-org-packages.el ends here
