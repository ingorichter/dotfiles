;;; crafted-org-config.el --- Org config              -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: hypermedia, convenience

;;; Code:

(require 'org-web-tools)
(require 'org-protocol)
(require 'org-bullets)
(require 'ob-plantuml)

(global-set-key (kbd "C-c w l") 'org-web-tools-insert-link-for-url)

(setq org-bullets-bullet-list '("●" "◎" "○" "◆" "◇" "✸" "•"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-capture-templates
      '(("l" "A link, for reading later." entry
         (file+headline "notes-new.org" "Reading List")
         "* %:description\n%u\n\n%c\n\n%i"
         :empty-lines 1)))

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
