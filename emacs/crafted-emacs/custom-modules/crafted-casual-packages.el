;; crafted-casual-packages.el --- Telegram config              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: socialmedia

;;; Code:

;; Define stub function to fix native compiler warning
;; casual-timezone-planner-current-remote is referenced but not defined
(defun casual-timezone-planner-current-remote ()
  "Stub function - the original was removed from casual-timezone-utils."
  (interactive)
  (message "casual-timezone-planner-current-remote is not implemented"))

(add-to-list 'package-selected-packages 'symbol-overlay)

(setq package-install-upgrade-built-in t)

(add-to-list 'package-selected-packages 'casual-suite)

(provide 'crafted-casual-packages)
;;; crafted-casual-packages.el ends here
