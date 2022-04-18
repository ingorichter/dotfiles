;;;; rational-git.el --- Git config                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;; Commentary

;;

;;; Code:

(rational-package-install-package 'magit)
(let ((map global-map))
  (define-key map (kbd "C-x g") #'magit-status))

(rational-package-install-package 'git-gutter)
(add-hook 'prog-mode 'git-gutter-mode)
(setq git-gutter:update-interval 0.02)

(rational-package-install-package 'git-gutter-fringe)
(define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)

(provide 'rational-git)
;;; rational-git.el ends here
