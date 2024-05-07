;;; crafted-gptel-config.el --- Org config              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: hypermedia, convenience

;;; Code:

(require 'gptel)
(require 'auth-source)

(gptel-make-ollama "Ollama"             ;Any name of your choosing
  :host "localhost:11434"               ;Where it's running
  :stream t                             ;Stream responses
  :models '("llama3:latest"))          ;List of models

(defun ir/openai-token ()
  (let ((found (nth 0 (auth-source-search :host "api.openai.com" :max 1))))
    (when found
      (let ((secret (plist-get found :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

;; (setq gptel-api-key (ir/openai-token))

(provide 'crafted-gptel-config)
;;; crafted-gptel-packages.el ends here
