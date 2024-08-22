;;; crafted-mastodon-packages.el --- everything needed to use mastodon  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ingo Richter

;; Author: Ingo Richter <ingorichter@Ingos-Adobe-MB-Pro.local>
;; Keywords: socialmedia

(add-to-list 'package-selected-packages 'mastodon)
(add-to-list 'package-selected-packages '(mastodon-alt :type git :host github :repo "rougier/mastodon-alt"))
 
(provide 'crafted-mastodon-packages)

;;; crafted-mastodon-packages.el ends here
