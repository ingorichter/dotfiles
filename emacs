					; -*-Lisp-*-
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(require 'helm-config)

(setq org-directory "~/Nextcloud/org")
(setq org-archive-location (concat org-directory "/archive/%s_archive::"))
(setq org-default-notes-file (concat org-directory "/notes.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (list org-directory))
 '(package-selected-packages
   (quote
    (spacemacs-theme grandshell-theme kaolin-themes helm org-bullets which-key try use-package))))

(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

(define-key global-map "\C-cc" 'org-capture)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-font-lock-mode 1)

;; have timestamp added to finished items
(setq org-log-done 'time)

;; capture templates
(setq org-capture-templates '(("t" "To Do Item" entry
			       (file+headline (concat org-directory "/mylife.org") "Tasks")
			       "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
			      ("j" "Journal" entry
			       (file+datetree (concat org-directory "/journal/journal.org"))
			       "* %?\n%U\n")))
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	      (sequence "WAITING(W@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
	      ("NEXT" :foreground "blue" :weight bold))))
