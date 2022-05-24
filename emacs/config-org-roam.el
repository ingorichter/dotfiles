;;; config-org-roam.el --- org-roam setup            -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Ingo Richter

;; Author: Ingo Richter <ingo.richter+github@gmail.com>
;; Keywords: convenience, emacs

;; org-roam
(use-package org-roam
  :straight t
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/Nextcloud/org/roam/"))
  (setq org-roam-dailies-directory "daily/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
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
  (org-roam-dailies-capture-templates
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
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n a" . org-roam-alias-add)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%?" :target
           (file+head "websites/${slug}.org" "#+roam_key: ${ref}\n#+filetags: website:bookmark\n#+title: ${title}\n")
           :unnarrowed f)))
  (require 'org-roam-protocol))

;; org-roam-timestamp
(rational-package-install-package '(org-roam-timestamps :type git :repo "ThomasFKJorna/org-roam-timestamps"))
(add-hook 'org-roam 'org-roam-timestamps-mode)

(provide 'config-org-roam)
;;; end config-org-roam
