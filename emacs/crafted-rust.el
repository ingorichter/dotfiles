;;; crafted-rust.el --- Config to support rust development  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: languages, tools
;; inspired by https://robert.kra.hn/posts/rust-emacs-setup/

(crafted-package-install-package 'rustic)
(crafted-package-install-package 'lsp-mode)

(setq lsp-eldoc-hook nil)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-signature-auto-activate nil)

;; uncomment for less flashiness
;; (setq lsp-eldoc-hook nil)
;; (setq lsp-enable-symbol-highlighting nil)
;; (setq lsp-signature-auto-activate nil)

;; comment to disable rustfmt on save
(setq rustic-format-on-save t)

;; (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)

(with-eval-after-load 'rustic
  (define-key rustic-mode-map (kbd "M-j") 'lsp-ui-imenu)
  (define-key rustic-mode-map (kbd "M-?") 'lsp-find-references)
  (define-key rustic-mode-map (kbd "C-c C-c l") 'flycheck-list-errors)
  (define-key rustic-mode-map (kbd "C-c C-c a") 'lsp-execute-code-action)
  (define-key rustic-mode-map (kbd "C-c C-c r") 'lsp-rename)
  (define-key rustic-mode-map (kbd "C-c C-c q") 'lsp-workspace-restart)
  (define-key rustic-mode-map (kbd "C-c C-c Q") 'lsp-workspace-shutdown)
  (define-key rustic-mode-map (kbd "C-c C-c s") 'lsp-rust-analyzer-status)
  (define-key rustic-mode-map (kbd "C-c C-c e") 'lsp-rust-analyzer-expand-macro)
  (define-key rustic-mode-map (kbd "C-c C-c h") 'lsp-ui-doc-glance))
  ;; (bind-keys :map rustic-mode-map
  ;;            ("M-j" . lsp-ui-imenu)
  ;;             ("M-?" . lsp-find-references)
  ;;             ("C-c C-c l" . flycheck-list-errors)
  ;;             ("C-c C-c a" . lsp-execute-code-action)
  ;;             ("C-c C-c r" . lsp-rename)
  ;;             ("C-c C-c q" . lsp-workspace-restart)
  ;;             ("C-c C-c Q" . lsp-workspace-shutdown)
  ;;             ("C-c C-c s" . lsp-rust-analyzer-status)
  ;;             ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
  ;;             ("C-c C-c d" . dap-hydra)
  ;;             ("C-c C-c h" . lsp-ui-doc-glance)))

(crafted-package-install-package 'lsp-mode)
(crafted-package-install-package 'lsp-ui)

(setq lsp-rust-analyzer-cargo-watch-command "clippy")
(setq lsp-eldoc-render-all t)
(setq lsp-idle-delay 0.6)
(setq lsp-rust-analyzer-server-display-inlay-hints t)
(setq lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
(setq lsp-rust-analyzer-display-chaining-hints t)
(setq lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
(setq lsp-rust-analyzer-display-closure-return-type-hints t)
(setq lsp-rust-analyzer-display-parameter-hints nil)
(setq lsp-rust-analyzer-display-reborrow-hints nil)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(with-eval-after-load 'lsp-mode
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-sideline-show-hover t))

(setq lsp-ui-doc-enable nil)

(provide 'crafted-rust)
