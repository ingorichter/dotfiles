;;; custom-module-config.el --- Configuration of my custom modules  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ingo Richter

;; Author: Ingo Richter <ingo.richter+github@gmail.com>
;; Keywords: lisp, local, configuration

;; packages
;; denote is part of crafted emacs
(require 'crafted-denote-packages)
(require 'crafted-org-packages)
(require 'crafted-rust-packages)
(require 'crafted-customfunctions-packages)
(require 'crafted-mu4e-packages)
(require 'crafted-org-roam-packages)

(crafted-package-install-selected-packages)

;; configs
(require 'crafted-denote-config)
(require 'crafted-org-config)
(require 'crafted-rust-config)
(require 'crafted-customfunctions-config)
(require 'crafted-mu4e-config)
(require 'crafted-org-roam-config)

(provide 'custom-module-config)
;;; crafted-module-config.el ends here
