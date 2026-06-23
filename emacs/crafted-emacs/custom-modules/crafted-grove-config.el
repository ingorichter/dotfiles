;;; crafted-grove-config.el --- Obsidian style notes organizer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: data

;;; Code:

(require 'grove)

(setq grove-directory "~/Nextcloud/org")

(define-key global-map (kbd "C-c v") grove-command-map)

(global-grove-mode 1)

;; (use-package grove
;;   :ensure t
;;   :bind-keymap ("C-c v" . grove-command-map)
;;   :custom
;;   (grove-directory "~/Nextcloud/org")
;;   :config
;;   (global-grove-mode 1))

(provide 'crafted-grove-config)
;;; crafted-grove-config.el ends here
