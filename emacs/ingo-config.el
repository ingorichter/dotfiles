;;; ingo-config.el -*- lexical-binding: t; -*-

;; this is required to run gpg to decode the password to sync with nextcloud
(crafted-package-install-package 'exec-path-from-shell)

(with-eval-after-load "exec-path-from-shell"
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Timer
(crafted-package-install-package '(tmr :type git :repo "protesilaos/tmr"))
(require 'tmr)
(setq tmr-notification-urgency 'normal)
(setq tmr-descriptions-list 'tmr-description-history)
;; OPTIONALLY set global key bindings:
(let ((map global-map))
  (define-key map (kbd "C-c t t") #'tmr)
  (define-key map (kbd "C-c t T") #'tmr-with-description)
  (define-key map (kbd "C-c t l") #'tmr-tabulated-view) ; "list timers" mnemonic
  (define-key map (kbd "C-c t c") #'tmr-clone)
  (define-key map (kbd "C-c t k") #'tmr-cancel)
  (define-key map (kbd "C-c t s") #'tmr-reschedule)
  (define-key map (kbd "C-c t e") #'tmr-edit-description)
  (define-key map (kbd "C-c t r") #'tmr-remove)
  (define-key map (kbd "C-c t R") #'tmr-remove-finished))

;; Encoding
(prefer-coding-system       'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(global-set-key (kbd "<f5>") 'revert-buffer)
(setenv "BROWSER" "firefox")
(setq org-ellipsis "⤵")
(global-display-line-numbers-mode t)
(global-visual-line-mode t)
;; monitor file changes and update buffers that haven't been modified
;; this will be set by modules/rational-defaults.el
;; (global-auto-revert-mode 1)

;; https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs

(setq user-full-name "Ingo Richter"
      user-mail-address "ingo.richter@gmail.com")

;; some custom funtions
(defun ir/new-buffer ()
  "Create a new frame with an empty buffer"
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

(defun ir/empty-frame ()
  "Open a new frame with a buffer named Untitled<N>.

        The buffer is not associated with a file."
  (interactive)
  (switch-to-buffer-other-frame (generate-new-buffer "untitled")))

(defun ir/kill-all-buffer ()
  "Kill all open buffer"
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

;;  backup settings
(setq backup-directory (concat crafted-config-var-directory "saved"))
(setq backup-by-copying t
      backup-directory-alist
      `((".*" . ,backup-directory))
      delete-old-versions t
      kept-old-versions 2
      kept-new-versions 6
      version-control t)

(setq auto-save-default t)
(setq make-backup-files t)
(setq auto-save-list-file-prefix nil)

;; custom functions and key map
(defun ir/org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading))) "/DONE" 'tree))

;; z-map is convenient since it's close to the ctrl key on the left side ...
(crafted-package-install-package 'general)

(general-define-key
 :prefix-command 'z-map
 :prefix "C-z"
 "n" 'ir/empty-frame
 "f" 'ir/new-buffer
 "K" 'ir/kill-all-buffer
 "a" 'ir/org-archive-done-tasks)

;; ;; Flycheck
;; (straight-use-package 'flycheck)

;; ;; Caldav
;; ;; password and username are stored in netrc
;; ;; https://github.com/dengste/org-caldav
;; (use-package org-caldav
;;   :ensure t
;;   :config
;;   (setq org-caldav-url "https://cloud.familie-richter.de/remote.php/dav/calendars/ingo"
;;         ;;          org-caldav-calendar-id "F4F90979-075A-4128-934F-C709FF6C0112"
;;         org-caldav-calendar-id "personal"
;;         org-caldav-inbox "~/Nextcloud/org/gcal.org"
;;         org-caldav-save-directory "~/Nextcloud/org"
;;         org-caldav-files org-agenda-files
;;         org-icalendar-timezone "America/Los_Angeles"
;;         org-icalendar-date-time-format ";TZID=%Z:%Y%m%dT%H%M%S"))

;; (add-hook 'org-agenda-mode-hook (lambda () (org-caldav-sync) ))
;; (add-hook 'org-capture-after-finalize-hook (lambda () (org-caldav-sync) ))

;; ;; Custom Keymap

;; (defun i/load-keymap ()
;;   (interactive)
;;   (define-prefix-command 'z-map)
;;   (global-set-key (kbd "C-=") 'z-map)
;;   (define-key z-map (kbd "a") 'org-archive-done-tasks)
;;   (define-key z-map (kbd "j") 'org-journal-new-entry))

;; (i/load-keymap)

;; (global-set-key (kbd "<f9>") 'org-pomodoro)

;; ;; undo-tree
;; (straight-use-package '(undo-tree :type git :host gitlab :repo "tsc25/undo-tree"))

;; hacker-news
(crafted-package-install-package '(hackernews :type git :repo "clarete/hackernews.el"))

;; ;; markdown-mode
;; (use-package markdown-mode
;;   :ensure t
;;   :mode ("README\\.md\\'" . gfm-mode)
;;   :init (setq markdown-command "multimarkdown"))

;; (autoload 'markdown-mode "markdown-mode"
;;   "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist
;;              '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

;; (autoload 'gfm-mode "markdown-mode"
;;   "Major mode for editing GitHub Flavored Markdown files" t)
;; (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; ;; Pulsar - highlight lines
;; ;; https://protesilaos.com/emacs/pulsar
;; (straight-use-package '(pulsar :type git :host gitlab :repo "protesilaos/pulsar"))

;; (setq pulsar-pulse-functions
;;       '(isearch-repeat-forward
;;         isearch-repeat-backward
;;         recenter-top-bottom
;;         move-to-window-line-top-bottom
;;         reposition-window
;;         bookmark-jump
;;         other-window
;;         delete-window
;;         delete-other-windows
;;         forward-page
;;         backward-page
;;         scroll-up-command
;;         scroll-down-command
;;         windmove-right
;;         windmove-left
;;         windmove-up
;;         windmove-down
;;         windmove-swap-states-right
;;         windmove-swap-states-left
;;         windmove-swap-states-up
;;         windmove-swap-states-down
;;         tab-new
;;         tab-close
;;         tab-next
;;         org-next-visible-heading
;;         org-previous-visible-heading
;;         org-forward-heading-same-level
;;         org-backward-heading-same-level
;;         outline-backward-same-level
;;         outline-forward-same-level
;;         outline-next-visible-heading
;;         outline-previous-visible-heading
;;         outline-up-heading))

;; (setq pulsar-pulse t)
;; (setq pulsar-delay 0.055)
;; (setq pulsar-iterations 10)
;; (setq pulsar-face 'pulsar-magenta)
;; (setq pulsar-highlight-face 'pulsar-yellow)

;; (pulsar-global-mode 1)

;; ;; pulsar does not define any key bindings.  This is just a sample that
;; ;; respects the key binding conventions.  Evaluate:
;; ;;
;; ;;     (info "(elisp) Key Binding Conventions")
;; ;;
;; ;; The author uses C-x l for `pulsar-pulse-line' and C-x L for
;; ;; `pulsar-highlight-line'.
;; (let ((map global-map))
;;   (define-key map (kbd "C-c h p") #'pulsar-pulse-line)
;;   (define-key map (kbd "C-c h h") #'pulsar-highlight-line))

;; ;; load custom.el file
;; ;;(load "custom")

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection{%s}")
                 ("\\paragraph{%s}" . "\\paragraph{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph{%s}"))))

(if (file-readable-p "~/.quicklisp/slime-helper.el")
    (load (expand-file-name "~/.quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl"))

;; mu4e
(defun load-if-exists (f)
  "load the elisp file is it exists and is readable"
  (if (file-readable-p f)
      (load-file f)))

(if (featurep 'mu4e)
    (load-if-exists "~/.dotfiles/emacs/mu4econfig.el"))
