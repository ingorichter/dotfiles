;;;; crafted-webdev.el --- Git config                  -*- lexical-binding: t; -*-

(crafted-package-install-package 'tree-sitter)
(require 'tree-sitter)
(global-tree-sitter-mode)

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(crafted-package-install-package 'tree-sitter-langs)
(add-hook 'tree-sitter-langs #'tree-sitter)

(crafted-package-install-package 'typescript-mode)
(add-hook 'typescript-mode #'tree-sitter)

(define-derived-mode typescriptreact-mode typescript-mode
  "TypeScript TSX")
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))

(add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx))

(crafted-package-install-package '(tsi.el :type git :repo "orzechowskid/tsi.el"))
(require 'tsi-css)
(require 'tsi-json)
(require 'tsi-typescript)

(add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
(add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
(add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
(add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1)))

;; https://github.com/radian-software/apheleia
(crafted-package-install-package '(apheleia :type git :repo "radian-software/apheleia"))
(apheleia-global-mode +1)

(provide 'crafted-webdev)
;;; crafted-webdev.el ends here
