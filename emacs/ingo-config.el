;;; ingo-config.el -*- lexical-binding: t; -*-

;; this is required to run gpg to decode the password to sync with nextcloud
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(global-set-key (kbd "<f5>") 'revert-buffer)
(setenv "BROWSER" "firefox")
(setq org-ellipsis "⤵")
(global-display-line-numbers-mode t)
;; monitor file changes and update buffers that haven't been modified
(global-auto-revert-mode 1)

;; https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs
;; (setq custom-file "~/.emacs.d/custom.el")
;; (setq custom-file (locate-user-emacs-file "custom-vars.el"))
;; (when (file-exists-p custom-file)
;; (load custom-file))
;; this is now loaded at the end of this file

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
(setq backup-by-copying t
      backup-directory-alist
      '(("." . "~/.saves.d/"))
      delete-old-versions t
      kept-old-versions 2
      kept-new-versions 6
      version-control t)

;; z-map is convenient since it's close to the ctrl key on the left side ...
;; (straight-use-package 'general)

;; (general-define-key
;;  :prefix-command 'z-map
;;  :prefix "C-z"
;;  "n" 'ir/empty-frame
;;  "f" 'ir/new-buffer
;;  "K" 'ir/kill-all-buffer)

;; ;; Flycheck
;; (straight-use-package 'flycheck)

;; ;; Org-Mode
;; basic settings are made from ~/.emacs.d/modules/rational-org.el
(setq org-appear-autolinks t)

;; (straight-use-package 'ob-crystal)
;; (straight-use-package 'org)

;; (require 'org-protocol)

;; (setq org-directory "~/Nextcloud/org"
;;       org-agenda-files (list org-directory)
;;       org-archive-location (concat org-directory "/archive/%s_archive::")
;;       org-default-notes-file (concat org-directory "/notes.org")
;;       org-default-todo-file (concat org-directory "/mylife.org")
;;       org-refile-location (concat org-directory "/refile.org")
;;       org-gcal-location (concat org-directory "/gcal.org")
;;       org-weekly-goals (concat org-directory "/weekly-goals.org")
;;       org-goals (concat org-directory "/goals.org")
;;       org-journal-location (concat org-directory "/journal/journal.org"))

;; (setq org-capture-templates '(("t" "To Do Item" entry (file+headline org-refile-location "Todo")
;;                                "* TODO %^{Titel} %^g\n %?\n\n:LOGBOOK:\n - Added: %U\n:END:")
;;                               ("a" "Appointment" entry (file org-gcal-location)
;;                                "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
;;                               ("n" "Note" entry (file+headline org-default-notes-file "Notes")
;;                                "* Note %^{Titel} %^g\n  %?\n  :LOGBOOK:\n - Added: %U\n:END:")
;;                               ("i" "Idee" entry (file+headline org-refile-location "Ideen")
;;                                "* Idee %^{Titel} %^g\n  %?\n  :LOGBOOK:\n - Added: %U\n:END:")
;;                               ("w" "Weekly Goals" entry
;;                                (file+datetree org-weekly-goals (format-time-string "%Y"))
;;                                "* %U\n\nHigh Level Ziele fuer die %(format-time-string "%W"). Woche\n - [ ] %(format-time-string "%W")$ x in die Spardose\n - [ ] Workout\n - [ ] Laufen")
;;                               ("j" "Journal" entry (file+datetree org-journal-location)
;;                                "* %U - %?\n  %i" :clock-in t :clock-resume t)))

;; ;; use org-bullets for nicer formatting
;; (straight-use-package 'org-bullets)
;; (setq org-bullets-bullet-list '("●" "◎" "○" "◆" "◇" "✸" "•"))
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; ;; (use-package org-bullets
;; ;;   :ensure t
;; ;;   :config
;; ;;   (add-hook 'org-mode-hook (lambda() (org-bullets-mode 1))))

;; ;; Org-mode settings
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (global-font-lock-mode 1)
;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c c") 'org-capture)

;; ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
;; (setq org-refile-targets (quote ((nil :maxlevel . 9)
;;                                  (org-agenda-files :maxlevel . 9))))

;; ;; have a timestamp added to finished items
;; (setq org-log-done 'time)

;; ;; capture templates
;; (setq org-todo-keywords
;;       (quote (
;;               (sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
;;               (sequence "WAITING(W@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
;; ;; this was mentioned in http://howardism.org/Technical/Emacs/literate-programming-tutorial.html
;; (setq org-confirm-babel-evaluate nil
;;       org-src-fontify-natively t
;;       org-src-tab-acts-natively t)

;; (setq org-hide-emphasis-markers t
;;       org-fontify-done-headline t
;;       org-hide-leading-stars t
;;       org-pretty-entities t
;;       org-odd-levels-only t)

;; (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "†")
;;                                        ("#+END_SRC" . "†")
;;                                        ("#+begin_src" . "†")
;;                                        ("#+end_src" . "†")
;;                                        (">=" . "≥")
;;                                        ("=>" . "⇨")))
;; (setq prettify-symbols-unprettify-at-point 'right-edge)
;; (add-hook 'org-mode-hook 'prettify-symbols-mode)

;; minad/org-modern
(straight-use-package '(org-modern :type git :host github :repo "minad/org-modern"))
;; Add frame borders and window dividers
;; (modify-all-frames-parameters
;;  '((right-divider-width . 40)
;;    (internal-border-width . 40)))
;; (dolist (face '(window-divider
;;                 window-divider-first-pixel
;;                 window-divider-last-pixel))
;;   (face-spec-reset-face face)
;;   (set-face-foreground face (face-attribute 'default :background)))
;; (set-face-background 'fringe (face-attribute 'default :background))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 ;; org-ellipsis "…"

 ;; Agenda styling
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")

;; Enable org-modern-mode
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; ;; (custom-theme-set-faces
;; ;; 'user
;; ;; '(variable-pitch ((t (:family "Source Sans Pro" :height 120 :weight light))))
;; ;; '(fixed-pitch ((t ( :family "Consolas" :slant normal :weight normal :height 0.9 :width normal)))))
;; (custom-theme-set-faces
;;  'user
;;  '(org-code ((t (:inherit (shadow fixed-pitch))))))

;; ;; (custom-theme-set-faces
;; ;; 'user
;; ;; '(org-block                 ((t (:inherit fixed-pitch))))
;; ;;  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;; ;;  '(org-property-value        ((t (:inherit fixed-pitch))) t)
;; ;;  '(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; ;;  '(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold))))
;; ;;  '(org-verbatim              ((t (:inherit (shadow fixed-pitch))))))

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((shell      . t)
;;    (js         . t)
;;    (emacs-lisp . t)
;;    (perl       . t)
;;    (crystal    . t)
;;    (clojure    . t)
;;    (python     . t)
;;    (ruby       . t)
;;    (dot        . t)
;;    (css        . t)
;;    (plantuml   . t)))

;; (straight-use-package 'org-ql)

;; ;; org-journal
;; (straight-use-package 'org-journal)
;; (setq org-journal-dir "~/Nextcloud/org/journal/")
;; (setq org-journal-date-format "%A, %d %B %Y")

;; (defun org-journal-file-header-func (time)
;;   "Custom function to create journal header."
;;   (concat
;;    (pcase org-journal-file-type
;;      (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
;;      (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
;;      (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
;;      (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))
;; (setq org-journal-file-header 'org-journal-file-header-func)

;; org-roam
;; (use-package org-roam
(use-package org-roam
  :straight t
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/Nextcloud/org/roam/"))
  (setq org-roam-dailies-directory "daily/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain (file "~/Nextcloud/org/roam/templates/BookNoteTemplate.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain (file "~/Nextcloud/org/roam/templates/ProjectTemplate.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
      :unnarrowed t)
     ("c" "Contact" plain (file "~/Nextcloud/org/roam/templates/PersonTemplate.org")
      :if-new (file+head "contacts/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n"))
     ("m" "Five Minute Journal Morning" plain
      "* Morning Questions\n** What am I grateful for?\n** What would make today great?\n** What Am I Worried About?\n** What Am I Thinking Of?\n"
      :if-new (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n"))
     ("e" "Five Minute Journal Evening" plain
      "* Evening Questions\n** How Am I feeling?\n** What's Something Good That Happened Today?\n** What Did I Do Well?\n** What Could I Have Done Better?\n** What did I learn/achieve today?\n** Who did I talk to today?\n** How is today special?\n"
      :if-new (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n"))
     ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n a" . org-roam-alias-add)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%?" :target
           (file+head "websites/${slug}.org" "#+roam_key: ${ref}\n#+filetags: website:bookmark\n#+title: ${title}\n")
           :unnarrowed f)))
  (require 'org-roam-protocol))

;; org-roam-timestamp
(rational-package-install-package '(org-roam-timestamps :type git :repo "ThomasFKJorna/org-roam-timestamps"))
(add-hook 'org-roam 'org-roam-timestamps-mode)

;; ;; Caldav
;; ;; password and username are stored in netrc
;; ;; https://github.com/dengste/org-caldav
;; (use-package org-caldav
;;   :ensure t
;;   :config
;;   (setq org-caldav-url "https://cloud.familie-richter.synology.me/remote.php/dav/calendars/ingo"
;;         ;;          org-caldav-calendar-id "F4F90979-075A-4128-934F-C709FF6C0112"
;;         org-caldav-calendar-id "personal"
;;         org-caldav-inbox "~/Nextcloud/org/gcal.org"
;;         org-caldav-save-directory "~/Nextcloud/org"
;;         org-caldav-files org-agenda-files
;;         org-icalendar-timezone "America/Los_Angeles"
;;         org-icalendar-date-time-format ";TZID=%Z:%Y%m%dT%H%M%S"))

;; (add-hook 'org-agenda-mode-hook (lambda () (org-caldav-sync) ))
;; (add-hook 'org-capture-after-finalize-hook (lambda () (org-caldav-sync) ))

;; ;; custom functions and key map
;; (defun org-archive-done-tasks ()
;;   (interactive)
;;   (org-map-entries
;;    (lambda ()
;;      (org-archive-subtree)
;;      (setq org-map-continue-from (outline-previous-heading))) "/DONE" 'tree))

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
(rational-package-install-package '(hackernews :type git :repo "clarete/hackernews.el"))

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
