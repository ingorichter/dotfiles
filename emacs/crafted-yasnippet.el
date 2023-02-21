;;;; crafted-yasnippet.el --- Git config                  -*- lexical-binding: t; -*-

(crafted-package-install-package 'yasnippet)
(crafted-package-install-package 'yasnippet-snippets)

(require 'yasnippet)
(yas-global-mode 1)
;; found this in https://www.youtube.com/watch?v=xmBovJvQ3KU
(add-hook 'yar-minor-mode-hook (lambda ()
                                 (yas-activate-extra-mode 'fundamental-mode)))

(provide 'crafted-yasnippet)
;;; crafted-yasnippet.el ends here
