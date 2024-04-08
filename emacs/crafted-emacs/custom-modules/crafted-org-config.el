;;; crafted-org-config.el --- Org config              -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: hypermedia, convenience

;;; Code:

(require 'org-web-tools)
(require 'org-protocol)
(global-set-key (kbd "C-c w l") 'org-web-tools-insert-link-for-url)

(setq org-capture-templates
      '(("l" "A link, for reading later." entry
         (file+headline "notes-new.org" "Reading List")
         "* %:description\n%u\n\n%c\n\n%i"
         :empty-lines 1)))

(org-reload)

(provide 'crafted-org-config)
;;; crafted-org-packages.el ends here
