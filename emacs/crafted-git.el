;;;; crafted-git.el --- Git config                  -*- lexical-binding: t; -*-

(crafted-package-install-package 'magit)
(let ((map global-map))
  (define-key map (kbd "C-x g") #'magit-status))

(crafted-package-install-package 'git-gutter)
(add-hook 'prog-mode 'git-gutter-mode)
(setq git-gutter:update-interval 0.02)

(crafted-package-install-package 'git-gutter-fringe)
(define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)

(provide 'crafted-git)
;;; crafted-git.el ends here
