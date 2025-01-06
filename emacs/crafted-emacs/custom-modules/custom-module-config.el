;;; custom-module-config.el --- Configuration of my custom modules  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ingo Richter

;; Author: Ingo Richter <ingo.richter+github@gmail.com>
;; Keywords: lisp, local, configuration

;; remove janet from list of supported ts languages
;; (setq mylist (seq-copy treesit-auto-recipe-list))
(defun ir/remove-janet-from-treesit()
    "Workaround to remove janet from automatically installed languages"
  (setq newlist (seq-remove (lambda(e) (equal (treesit-auto-recipe-lang e) 'janet)) treesit-auto-recipe-list))
  (setq treesit-auto-recipe-list (seq-copy newlist)))

(defun ir/treesit-message()
  (message "Install all grammars"))
(advice-add 'treesit-auto-install-all :before #'ir/remove-janet-from-treesit)
;; (advice-remove 'treesit-auto-install-all #'ir/treesit-message)

;; packages
;; denote is part of crafted emacs
(require 'crafted-appearance-packages)
(require 'crafted-denote-packages)
(require 'crafted-org-packages)
;; (require 'crafted-rust-packages)
(require 'crafted-customfunctions-packages)
(require 'crafted-mu4e-packages)
(require 'crafted-org-roam-packages)
(require 'crafted-markdown-packages)
(require 'crafted-gptel-packages)
;; (require 'crafted-math-packages)
(require 'crafted-magit-packages)
(require 'crafted-mastodon-packages)
(require 'crafted-telegram-packages)
(require 'crafted-casual-packages)
;; (require 'crafted-writer-packages)

(crafted-package-install-selected-packages)

;; configs
(require 'crafted-appearance-config)
(require 'crafted-denote-config)
(require 'crafted-org-config)
;; (require 'crafted-rust-config)
(require 'crafted-customfunctions-config)
(require 'crafted-mu4e-config)
(require 'crafted-org-roam-config)
(require 'crafted-markdown-config)
(require 'crafted-gptel-config)
;; (require 'crafted-math-config)
(require 'crafted-magit-config)
(require 'crafted-mastodon-config)
(require 'crafted-telegram-config)
(require 'crafted-casual-config)
;; (require 'crafted-writer-config)

(provide 'custom-module-config)
;;; crafted-module-config.el ends here
