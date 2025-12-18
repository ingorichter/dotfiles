;;; crafted-casual-config.el --- Telegram config              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: socialmedia

;;; Code:

(require 'casual-agenda)
(require 'casual-bookmarks)
(require 'casual-calc)
(require 'casual-calendar)
(require 'casual-dired)
(require 'casual-editkit)
(require 'casual-ibuffer)
(require 'casual-info)
(require 'casual-isearch)
(require 'casual-re-builder)
;; (require 'casual)
;; (require 'casual-avy)
;; (require 'casual-suite)
(require 'casual-symbol-overlay)

;; The following code is a TL;DR initialization for Casual Suite.
(require 'casual-suite)
(keymap-set calc-mode-map "C-o" #'casual-calc-tmenu)
(keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
(keymap-set isearch-mode-map "C-o" #'casual-isearch-tmenu)
(keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu)
(keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
(keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu)
(keymap-set Info-mode-map "C-o" #'casual-info-tmenu)
;; (keymap-global-set "M-g" #'casual-avy-tmenu)
(keymap-set reb-mode-map "C-o" #'casual-re-builder-tmenu)
(keymap-set reb-lisp-mode-map "C-o" #'casual-re-builder-tmenu)
(keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu)
(keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)
(keymap-set symbol-overlay-map "C-o" #'casual-symbol-overlay-tmenu)
(keymap-global-set "C-o" #'casual-editkit-main-tmenu)

(provide 'crafted-casual-config)
;;; crafted-casual-config.el ends here
