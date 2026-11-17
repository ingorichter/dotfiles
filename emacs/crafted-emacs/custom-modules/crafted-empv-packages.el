;;; crafted-empv-packages.el --- Org config              -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: hypermedia, convenience

;;; Code:

(add-to-list 'package-selected-packages 'empv)
(unless (package-installed-p 'ytr)
  (package-vc-install
   '(ytr :url "https://github.com/xenodium/ytr.git")))

(provide 'crafted-empv-packages)
;;; crafted-empv-packages.el ends here
