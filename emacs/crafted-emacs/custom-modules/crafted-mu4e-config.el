;;; crafted-mu4e-config.el --- mu4e config           -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ingo Richter

;; Author: Ingo Richter <ingorichter@Ingos-Personal-MacBook-Pro.local>
;; Keywords: lisp

(defun load-if-exists (f)
  "load the elisp file is it exists and is readable"
  (if (file-readable-p f)
      (load-file f)))

;; (if (featurep 'mu4e)
(if (executable-find "mu")
    (load-if-exists "~/.dotfiles/emacs/mu4econfig.el"))

(provide 'crafted-mu4e-config)
;;; crafted-mu4e-config.el ends here
