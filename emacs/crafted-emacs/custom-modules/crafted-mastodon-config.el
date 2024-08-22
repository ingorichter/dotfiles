;;; crafted-mastodon-config.el --- Org config              -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ingo Richter

;; Author: Ingo Richter <ingo.richter@gmail.com>
;; Keywords: hypermedia, convenience

;;; Code:

(setq mastodon-instance-url "https://mastodon.social")
(setq mastodon-active-user "ingorichter")

(require 'mastodon-alt)
(mastodon-alt-tl-activate)

(provide 'crafted-mastodon-config)
;;; crafted-mastodon-config.el ends here

