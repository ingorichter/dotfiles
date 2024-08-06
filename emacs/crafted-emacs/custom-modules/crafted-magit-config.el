;;; crafted-magit-config.el --- Org config              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: hypermedia, convenience

;;; Code:

(let ((map global-map))
  (define-key map (kbd "C-x g") #'magit-status))

(add-hook 'prog-mode 'git-gutter-mode)
(setq git-gutter:update-interval 0.02)

(define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)

(provide 'crafted-magit-config)
;;; crafted-magit-config.el ends here

