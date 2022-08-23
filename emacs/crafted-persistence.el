;;; crafted-persistence.el -*- lexical-binding: t; -*-

;; When you visit a file, point goes to the last place it was when file was previously visited.
(save-place-mode 1)

;; Saving of history (minibuffer, kill-ring, search-ring, keyboard macros, shell commands).
(setq savehist-save-minibuffer-history t)
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring
          last-kbd-macro
          kmacro-ring
          shell-command-history))

(savehist-mode)

;; Restore last opened buffers after closing emacs.
(setq desktop-restore-eager 5)
(desktop-save-mode)
(setq desktop-path '(crafted-config-var-directory "~/.emacs.d/" "~"))
(provide 'crafted-persistence)

;;; crafted-persistence.el ends here
