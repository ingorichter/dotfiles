;;; crafted-denote-packages.el --- Org config              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: hypermedia, convenience

;;; Code:

(add-to-list 'package-selected-packages 'denote)
(add-to-list 'package-selected-packages 'denote-journal)
(add-to-list 'package-selected-packages 'denote-org)
(add-to-list 'package-selected-packages 'denote-silo)
(add-to-list 'package-selected-packages 'denote-markdown)
(add-to-list 'package-selected-packages 'denote-sequence)
(add-to-list 'package-selected-packages '(denote-menu :type git :repo "namilus/denote-menu"))

(provide 'crafted-denote-packages)
;;; crafted-denote-packages.el ends here
