;;; crafted-org-packages.el --- Org config              -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: hypermedia, convenience

;;; Code:

(add-to-list 'package-selected-packages 'org-web-tools)
(add-to-list 'package-selected-packages 'org-bullets)
(add-to-list 'package-selected-packages 'org-appear)
(add-to-list 'package-selected-packages '(org-contrib :type git :host sourcehut :repo "bzg/org-contrib" :files ("lisp/*.el")))
;; (add-to-list 'package-selected-packages 'org-protocol)

(provide 'crafted-org-packages)
;;; crafted-org-packages.el ends here

