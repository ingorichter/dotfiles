;;; crafted-empv-packages.el --- Org config              -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: hypermedia, convenience

;;; Code:

(add-to-list 'package-selected-packages '(empv :type git :repo "isamert/empv.el"))
(add-to-list 'package-selected-packages '(ytr :type git :host github :repo "xenodium/ytr"))

(provide 'crafted-empv-packages)
;;; crafted-empv-packages.el ends here
