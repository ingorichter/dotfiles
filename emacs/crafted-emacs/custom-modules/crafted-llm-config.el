;;; crafted-llm-config.el --- Org config              -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: llm, hypermedia

;;; Code:

(global-set-key (kbd "C-c o") #'ollama-buddy-menu)
(global-set-key (kbd "C-c O") #'ollama-buddy-transient-menu-wrapper)

(provide 'crafted-llm-config)
;;; crafted-llm-config.el ends here
