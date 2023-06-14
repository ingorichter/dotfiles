;;; crafted-writing.el --- Config for focussed writing  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: tools, writing

(crafted-package-install-package 'writeroom-mode)
(require 'writeroom-mode)

(define-key writeroom-mode-map (kbd "C-M-<") #'writeroom-decrease-width)
(define-key writeroom-mode-map (kbd "C-M->") #'writeroom-increase-width)
(define-key writeroom-mode-map (kbd "C-M-=") #'writeroom-adjust-width)

(provide 'crafted-writing)
