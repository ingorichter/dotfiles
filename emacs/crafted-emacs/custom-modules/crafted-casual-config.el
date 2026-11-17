;;; crafted-casual-config.el --- Telegram config              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: socialmedia

;;; Code:

(require 'casual-suite)

;; Preserve the existing mode shortcuts alongside the new Casual menus.
(setq casual-agenda-add-extra-keybindings nil
      casual-bibtex-add-extra-keybindings nil
      casual-bookmarks-add-extra-keybindings nil
      casual-compile-add-extra-keybindings nil
      casual-dired-add-extra-keybindings nil
      casual-eww-add-extra-keybindings nil
      casual-grep-add-extra-keybindings nil
      casual-help-add-extra-keybindings nil
      casual-ibuffer-add-extra-keybindings t
      casual-info-add-extra-keybindings nil
      casual-ispell-add-extra-keybindings nil
      casual-man-add-extra-keybindings nil)

(setq casual-avy-keybinding "M-g a")
(casual-suite-init)

(provide 'crafted-casual-config)
;;; crafted-casual-config.el ends here
