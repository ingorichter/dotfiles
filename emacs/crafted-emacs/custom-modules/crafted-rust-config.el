;;; crafted-rust-config.el --- Org config              -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: hypermedia, convenience

;;; Code:

(require 'crafted-defaults-config)
(require 'crafted-completion-config)
(require 'crafted-ide-config)

;; Automatically register `eglot-ensure' hooks for relevant major
;; modes (notably `rust-ts-mode').
(crafted-ide-eglot-auto-ensure-all)

;; The first time you run Emacs with this enabled, the Rust tree
;; sitter parser will be installed for you automatically.
(crafted-ide-configure-tree-sitter)

;; You will probably want to tweak this variable, it determines how
;; quickly the completion prompt provides LSP suggestions when
;; typing. Be careful if you set it to 0 in a large project!
(customize-set-variable 'corfu-auto-delay 0.25)

;; Reassign the rust-mode keybindings to the rust-ts-mode map.
(with-eval-after-load 'rust-ts-mode
  (require 'rust-mode)
  (define-key rust-ts-mode-map (kbd "C-c C-c C-u") #'rust-compile)
  (define-key rust-ts-mode-map (kbd "C-c C-c C-k") #'rust-check)
  (define-key rust-ts-mode-map (kbd "C-c C-c C-t") #'rust-test)
  (define-key rust-ts-mode-map (kbd "C-c C-c C-r") #'rust-run)
  (define-key rust-ts-mode-map (kbd "C-c C-c C-l") #'rust-run-clippy)
  (define-key rust-ts-mode-map (kbd "C-c C-f") #'rust-format-buffer)
  (define-key rust-ts-mode-map (kbd "C-c C-n") #'rust-goto-format-problem))

(provide 'crafted-rust-config)
;;; crafted-rust-config.el ends here

