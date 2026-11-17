;;; crafted-mastodon-packages.el --- everything needed to use mastodon  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ingo Richter

;; Author: Ingo Richter <ingorichter@Ingos-Adobe-MB-Pro.local>
;; Keywords: socialmedia

(add-to-list 'package-selected-packages 'mastodon)
(unless (package-installed-p 'mastodon-alt)
  (package-vc-install
   '(mastodon-alt :url "https://github.com/rougier/mastodon-alt.git")))
 
(provide 'crafted-mastodon-packages)

;;; crafted-mastodon-packages.el ends here
