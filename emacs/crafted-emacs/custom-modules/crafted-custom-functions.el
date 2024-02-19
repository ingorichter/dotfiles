;;; crafted-custom-functions.el --- My custom functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ingo Richter

;; Author: Ingo Richter <ingorichter@Ingos-Personal-MacBook-Pro.local>
;; Keywords: lisp

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

(defun ir/list-all-buffers (name)
       "Find all buffers whose title contains name"
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
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading))) "/DONE" 'tree))

;; z-map is convenient since it's close to the ctrl key on the left side ...
(add-to-list 'package-selected-packages 'general)

(general-define-key
 :prefix-command 'z-map
 :prefix "C-z"
 "n" 'ir/empty-frame
 "f" 'ir/new-buffer
 "K" 'ir/kill-all-buffer
 "a" 'ir/org-archive-done-tasks)

(provide 'crafted-custom-functions)
