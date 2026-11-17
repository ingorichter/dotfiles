;;; custom-module-config.el --- Configuration of my custom modules  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ingo Richter

;; Author: Ingo Richter <ingo.richter+github@gmail.com>
;; Keywords: lisp, local, configuration

;; setup harper for grammar checks
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(org-mode . ("harper-ls" "--stdio"))))
;; (add-hook 'org-mode-hook 'eglot-ensure)
(setq-default eglot-workspace-configuration
              '(:harper-ls (:userDictPath ""
                            :workspaceDictPath ""
                            :fileDictPath ""
                            :linters (:SpellCheck t
                                      :SpelledNumbers :json-false
                                      :AnA t
                                      :SentenceCapitalization t
                                      :UnclosedQuotes t
                                      :WrongQuotes :json-false
                                      :LongSentences t
                                      :RepeatedWords t
                                      :Spaces t
                                      :Matcher t
                                      :CorrectNumberSuffix t)
                            :codeActions (:ForceStable :json-false)
                            :markdown (:IgnoreLinkTitle :json-false)
                            :diagnosticSeverity "hint"
                            :isolateEnglish :json-false
                            :dialect "American"
                            :maxFileLength 120000
                            :ignoredLintsPath ""
                            :excludePatterns [])))


;; packages
;; denote is part of crafted emacs
(require 'crafted-appearance-packages)
(require 'crafted-denote-packages)
;; (require 'crafted-rust-packages)
(require 'crafted-customfunctions-packages)
(require 'crafted-mu4e-packages)
(require 'crafted-org-roam-packages)
(require 'crafted-markdown-packages)
(require 'crafted-gptel-packages)
;; (require 'crafted-math-packages)
(require 'crafted-magit-packages)
(require 'crafted-mastodon-packages)
(require 'crafted-telegram-packages)
(require 'crafted-casual-packages)
;; (require 'crafted-writer-packages)
(require 'crafted-llm-packages)
(require 'crafted-empv-packages)
(require 'crafted-grove-packages)
(require 'crafted-misc-packages)

(crafted-package-install-selected-packages)

;; configs
(require 'crafted-appearance-config)
(require 'crafted-denote-config)
;; (require 'crafted-rust-config)
(require 'crafted-customfunctions-config)
(require 'crafted-mu4e-config)
(require 'crafted-org-roam-config)
(require 'crafted-markdown-config)
(require 'crafted-gptel-config)
;; (require 'crafted-math-config)
(require 'crafted-magit-config)
(require 'crafted-mastodon-config)
(require 'crafted-telegram-config)
(require 'crafted-casual-config)
;; (require 'crafted-writer-config)
(require 'crafted-llm-config)
(require 'crafted-empv-config)
(require 'crafted-grove-config)
(require 'crafted-misc-config)

;;; crafted-module-config.el ends here
(provide 'custom-module-config)
