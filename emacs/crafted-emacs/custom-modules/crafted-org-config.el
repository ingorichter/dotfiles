;;; crafted-org-config.el --- Org config              -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: hypermedia, convenience

;;; Code:

(require 'org-web-tools)
(require 'org-protocol)
(require 'org-bullets)
(require 'ob-plantuml)

(setq org-directory "~/Nextcloud/org"
      org-agenda-files (list org-directory)
      org-archive-location (concat org-directory "/archive/%s_archive::")
      org-default-notes-file (concat org-directory "/notes.org")
      org-default-todo-file (concat org-directory "/mylife.org")
      org-refile-location (concat org-directory "/refile.org")
      org-gcal-location (concat org-directory "/gcal.org")
      org-weekly-goals (concat org-directory "/weekly-goals.org")
      org-goals (concat org-directory "/goals.org")
      org-journal-location (concat org-directory "/journal/journal.org")
      org-review-location (concat org-directory "/review.org")
      org-cicada-daily-status-location (concat org-directory "/cicada-daily-standup-status.org"))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(global-set-key (kbd "C-c w l") 'org-web-tools-insert-link-for-url)

(setq org-agenda-custom-commands
      '(("p" "Planning"
         ((tags-todo "+@planning"
                     ((org-agenda-overriding-header "Planning Tasks")))
           (tags-todo "-{.*}"
                      ((org-agenda-overriding-header "Untagged Tasks")))))
      ("i" "Inbox"
       ((todo ".*" ((org-agenda-files `(,org-refile-location)))
              (org-agenda-overriding-header "Unprocessed Inbox Items"))))))

(setq org-bullets-bullet-list '("●" "◎" "○" "◆" "◇" "✸" "•"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

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


(org-reload)

(setq org-plantuml-executable-path "/opt/homebrew/bin/plantuml")
(setq org-plantuml-exec-mode 'plantuml)
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(add-to-list 'org-babel-load-languages '(plantuml . t))

;; (with-eval-after-load 'org
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '(other Babel languages
;;    (plantuml . t)
;;    )))

(provide 'crafted-org-config)
;;; crafted-org-packages.el ends here
