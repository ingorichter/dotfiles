;; crafted-casual-packages.el --- Telegram config              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: socialmedia

;;; Code:

;; (add-to-list 'package-selected-packages '(casual :type git :host github :repo "kickingvegas/casual-suite" :files ("lisp/casual-suite*.el")))
(add-to-list 'package-selected-packages '(casual :type git :host github :repo "kickingvegas/casual" :files ("lisp/casual*.el")))
(add-to-list 'package-selected-packages '(casual-suite :type git :host github :repo "kickingvegas/casual-suite" :files ("lisp/casual*.el")))
(add-to-list 'package-selected-packages '(casual-avy :type git :host github :repo "kickingvegas/casual-avy" :files ("lisp/casual*.el")))
(add-to-list 'package-selected-packages 'symbol-overlay)
(add-to-list 'package-selected-packages '(casual-symbol-overlay :type git :host github :repo "kickingvegas/casual-symbol-overlay" :files ("lisp/casual-symbol-overlay*.el")))

(setq package-install-upgrade-built-in t)

(provide 'crafted-casual-packages)
;;; crafted-casual-packages.el ends here
