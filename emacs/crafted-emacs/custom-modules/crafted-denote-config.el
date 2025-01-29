;;; crafted-denote.el --- Denote config              -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: hypermedia, convenience

;; (crafted-package-install-package '(denote :type git :host github :repo "protesilaos/denote"))

;; Remember to check the doc strings of those variables.
(setq denote-directory (expand-file-name "~/Nextcloud/org/notes/"))
(setq denote-known-keywords '("emacs" "csdk" "read" "programming", "s4r", "adobe"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-file-type nil) ; Org is the default, set others here
(setq denote-prompts '(subdirectory title keywords))

;; Pick dates, where relevant, with Org's advanced interface:
(setq denote-date-prompt-use-org-read-date t)

(setq denote-allow-multi-word-keywords t)

(setq denote-date-format nil) ; read doc string

;; By default, we fontify backlinks in their bespoke buffer.
(setq denote-link-fontify-backlinks t)

;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
;; advanced.

;; If you use Markdown or plain text files (Org renders links as buttons
;; right away)
(add-hook 'find-file-hook #'denote-fontify-links-mode)

;; We use different ways to specify a path for demo purposes.
(setq denote-dired-directories
      (list denote-directory
            (thread-last denote-directory (expand-file-name "attachments"))
            (expand-file-name "~/Nextcloud/org/books")))

;; Generic (great if you rename files Denote-style in lots of places):
;; (add-hook 'dired-mode-hook #'denote-dired-mode)
;;
;; OR if only want it in `denote-dired-directories':
(add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

;; Denote DOES NOT define any key bindings.  This is for the user to
;; decide.  For example:
(let ((map global-map))
  (define-key map (kbd "C-c n j") #'ir/my-denote-journal) ; our custom command
  (define-key map (kbd "C-c n n") #'denote)
  (define-key map (kbd "C-c n N") #'denote-type)
  (define-key map (kbd "C-c n d") #'denote-date)
  (define-key map (kbd "C-c n s") #'denote-subdirectory)
  (define-key map (kbd "C-c n t") #'denote-template)
  ;; If you intend to use Denote with a variety of file types, it is
  ;; easier to bind the link-related commands to the `global-map', as
  ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
  ;; `markdown-mode-map', and/or `text-mode-map'.
  (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
  (define-key map (kbd "C-c n I") #'denote-link-add-links)
  (define-key map (kbd "C-c n l") #'denote-link-find-file) ; "list" links
  (define-key map (kbd "C-c n b") #'denote-link-backlinks)
  ;; Note that `denote-rename-file' can work from any context, not just
  ;; Dired bufffers.  That is why we bind it here to the `global-map'.
  (define-key map (kbd "C-c n r") #'denote-rename-file)
  (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter))

;; Key bindings specifically for Dired.
;;(let ((map dired-mode-map))
;; (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
;; (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-marked-files)
;; (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter))

(with-eval-after-load 'org-capture
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
               '("n" "New note (with denote.el)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

;; custom functions
;; used this function from https://whhone.com/posts/denote-with-subdirectories/
(defun ir/consult-denote-ripgrep ()
  "Search with 'rg' for files in denote-directory where the content matches a regexp"
  (interactive)
  (consult-ripgrep denote-directory ""))

(defun ir/create-denote-journal-entry ()
  (interactive)
  (let* ((date (org-read-date))
         (time (org-time-string-to-time date))
         (title (format-time-string "%A %d %B %Y" time))
         (initial (denote-sluggify title))
         (target (read-file-name "Select note: " (concat (denote-directory) "journal/") nil nil initial
                                 (lambda (f)
                                   (or (denote-file-has-identifier-p f)
                                       (file-directory-p f))))))
    (if (file-exists-p target)
        (find-file target)
      (denote title '("journal") denote-file-type nil date))))

(defun ir/insert-time-stamp ()
  (interactive)
  (insert (current-time-string)))

(defun ir/my-denote-journal ()
  "An alias for denote-journal-extras-new-or-existing-entry"
  (interactive)
  (denote-journal-extras-new-or-existing-entry))

(defun ir/my-denote-journal-old ()
  "Create an entry tagged 'journal' with the date as its title.
If a journal for the current day exists, visit it.  If multiple
entries exist, prompt with completion for a choice between them.
Else create a new file."
  (interactive)
  (let* ((today (format-time-string "%A %e %B %Y"))
         (string (denote-sluggify today))
         (files (denote-directory-files-matching-regexp string)))
    (cond
     ((> (length files) 1)
      (find-file (completing-read "Select file: " files nil :require-match)))
     (files
      (find-file (car files)))
     (t
      (denote
       today
       '("journal"))))))


(require 'denote-menu)

(global-set-key (kbd "C-c z") #'list-denotes)

(define-key denote-menu-mode-map (kbd "c") #'denote-menu-clear-filters)
(define-key denote-menu-mode-map (kbd "/ r") #'denote-menu-filter)
(define-key denote-menu-mode-map (kbd "/ k") #'denote-menu-filter-by-keyword)
(define-key denote-menu-mode-map (kbd "/ o") #'denote-menu-filter-out-keyword)
(define-key denote-menu-mode-map (kbd "e") #'denote-menu-export-to-dired)

(provide 'crafted-denote-config)
