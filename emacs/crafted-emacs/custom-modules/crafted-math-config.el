;;; crafted-math-config.el --- Org config              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: hypermedia, convenience

;;; Code:

(require 'casual-calc)
(define-key calc-mode-map (kbd "C-o") 'casual-main-menu)

(provide 'crafted-math-config)
;;; crafted-math-packages.el ends here
