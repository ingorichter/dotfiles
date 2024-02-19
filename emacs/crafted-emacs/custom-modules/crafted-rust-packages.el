;;; crafted-rust-packages.el --- Org config              -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: hypermedia, convenience

;;; Code:

(require 'crafted-completion-packages)
(require 'crafted-ide-packages)

(add-to-list 'package-selected-packages 'rust-mode)
(package-install-selected-packages :noconfirm)

(provide 'crafted-rust-packages)
;;; crafted-rust-packages.el ends here

