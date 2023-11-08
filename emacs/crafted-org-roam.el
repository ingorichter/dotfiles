;;; crafted-org-roam.el --- org-roam setup            -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Ingo Richter

;; Author: Ingo Richter <ingo.richter+github@gmail.com>
;; Keywords: convenience, emacs

;; org-roam
(crafted-package-install-package 'org-roam)

;; config
(setq org-roam-v2-ack t)
(setq org-roam-directory (file-truename "~/Nextcloud/org/roam/"))
(setq org-roam-dailies-directory "daily/")
(setq org-roam-completion-everywhere t)

;; capture templates
(setq org-roam-capture-templates
'(("d" "default" plain
   "%?"
   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
   :unnarrowed t)
   ("l" "programming language" plain
   "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
   :unnarrowed t)
   ("b" "book notes" plain (file "~/Nextcloud/org/roam/templates/BookNoteTemplate.org")
   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
   :unnarrowed t)
   ("p" "project" plain (file "~/Nextcloud/org/roam/templates/ProjectTemplate.org")
   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
   :unnarrowed t)
   ("c" "Contact" plain (file "~/Nextcloud/org/roam/templates/PersonTemplate.org")
   :if-new (file+head "contacts/${slug}.org" "#+title: ${title}\n")
   :unnarrowed t)
   ))
(setq org-roam-dailies-capture-templates
'(("d" "default" entry
   "* %?"
   :target (file+head "%<%Y-%m-%d>.org"
                        "#+title: %<%Y-%m-%d>\n"))
   ("m" "Five Minute Journal Morning" plain
   "* Morning Questions\n** What am I grateful for?\n** What would make today great?\n** What Am I Worried About?\n** What Am I Thinking Of?\n"
   :if-new (file+head "%<%Y-%m-%d>.org"
                        "#+title: %<%Y-%m-%d>\n"))
   ("e" "Five Minute Journal Evening" plain
   "* Evening Questions\n** How Am I feeling?\n** What's Something Good That Happened Today?\n** What Did I Do Well?\n** What Could I Have Done Better?\n** What did I learn/achieve today?\n** Who did I talk to today?\n** How is today special?\n"
   :if-new (file+head "%<%Y-%m-%d>.org"
                        "#+title: %<%Y-%m-%d>\n"))
   ))

;; keyboard shortcuts
(define-key global-map (kbd "C-c n l") #'org-roam-buffer-toggle)
(define-key global-map (kbd "C-c n f") #'org-roam-node-find)
(define-key global-map (kbd "C-c n g") #'org-roam-graph)
(define-key global-map (kbd "C-c n i") #'org-roam-node-insert)
(define-key global-map (kbd "C-c n c") #'org-roam-capture)
(define-key global-map (kbd "C-c n t") #'org-roam-tag-add)
(define-key global-map (kbd "C-c n a") #'org-roam-alias-add)
(define-key global-map (kbd "C-c n j") #'org-roam-dailies-capture-today)

(setq org-roam-capture-ref-templates
      '(("r" "ref" plain "%?" :target
         (file+head "websites/${slug}.org" "#+roam_key: ${ref}\n#+filetags: website:bookmark\n#+title: ${title}\n")
         :unnarrowed f)))

(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
(org-roam-db-autosync-mode)
(require 'org-roam-protocol)

;; org-roam-timestamp
(crafted-package-install-package '(org-roam-timestamps :type git :repo "ThomasFKJorna/org-roam-timestamps"))
(add-hook 'org-roam 'org-roam-timestamps-mode)

(crafted-package-install-package '(denote-menu :type git :repo "namilus/denote-menu"))
(require 'denote-menu)

(global-set-key (kbd "C-c z") #'list-denotes)

(define-key denote-menu-mode-map (kbd "c") #'denote-menu-clear-filters)
(define-key denote-menu-mode-map (kbd "/ r") #'denote-menu-filter)
(define-key denote-menu-mode-map (kbd "/ k") #'denote-menu-filter-by-keyword)
(define-key denote-menu-mode-map (kbd "/ o") #'denote-menu-filter-out-keyword)
(define-key denote-menu-mode-map (kbd "e") #'denote-menu-export-to-dired)

(provide 'crafted-org-roam)
;;; end crafted-org-roam
