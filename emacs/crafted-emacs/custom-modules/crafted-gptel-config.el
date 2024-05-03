;;; crafted-gptel-config.el --- Org config              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: hypermedia, convenience

;;; Code:

(require 'gptel)

(gptel-make-ollama "Ollama"             ;Any name of your choosing
  :host "localhost:11434"               ;Where it's running
  :stream t                             ;Stream responses
  :models '("llama3:latest"))          ;List of models

(provide 'crafted-gptel-config)
;;; crafted-gptel-packages.el ends here
