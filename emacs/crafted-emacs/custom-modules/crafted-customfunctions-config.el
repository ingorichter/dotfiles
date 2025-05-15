;;; crafted-customfunctions-config.el --- My custom functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ingo Richter

;; Author: Ingo Richter <ingorichter@Ingos-Personal-MacBook-Pro.local>
;; Keywords: lisp

;;; Commentary:
;; This file contains custom functions for my Emacs configuration.

;;; Code:

(defun ir/new-buffer ()
  "Create a new frame with an empty buffer."
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
  "Kill all open buffers."
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(defun ir/list-all-buffers (name)
  "Find all buffers whose title contains NAME.
Returns a list of matching buffers and kills them."
  (interactive "sEnter the buffer name: ")
  (let ((matching-buffers (list)))
    (dolist (buffer (buffer-list))
      (when (string-match-p name (buffer-name buffer))
        (push buffer matching-buffers)))
    (if matching-buffers
        (progn
          (message "Matching buffers: %s" matching-buffers)
          (mapc #'kill-buffer matching-buffers)
          matching-buffers)
      (message "No matching buffers found."))))

;; custom functions and key map
(defun ir/org-archive-done-tasks ()
  "Archive all DONE tasks in the current buffer."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading))) "/DONE" 'tree))

;; open random denote for refinement
(defun ir/denote-file-is-journal-p (file)
  "Check if FILE is a journal entry.
Take a denote file and extract the keywords.  Return non-nil
if the keywords contain the string 'journal'."
  (string-match "journal" (denote-retrieve-filename-keywords file)))

(defun ir/denote-open-random-note ()
  "Open a random note from denote-directory-files.
This will exclude journal notes."
  (interactive)
  (denote-open-or-create
   (seq-random-elt
    (seq-filter (lambda (file)
                  (not (ir/denote-file-is-journal-p file)))
                (denote--directory-get-files)))))

;; z-map is convenient since it's close to the ctrl key on the left side ...

(general-define-key
 :prefix-command 'z-map
 :prefix "C-z"
 "r" 'ir/denote-open-random-note
 "n" 'ir/empty-frame
 "f" 'ir/new-buffer
 "K" 'ir/kill-all-buffer
 "a" 'ir/org-archive-done-tasks)

(provide 'crafted-customfunctions-config)
;;; crafted-customfunctions-config.el ends here
