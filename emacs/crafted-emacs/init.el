;;; init.el -*- lexical-binding: t; -*-

;;; Initial phase.

;; Load the custom file if it exists.  Among other settings, this will
;; have the list `package-selected-packages', so we need to load that
;; before adding more packages.  The value of the `custom-file'
;; variable must be set appropriately, by default the value is nil.
;; This can be done here, or in the early-init.el file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil 'nomessage))

;; Adds crafted-emacs modules to the `load-path', sets up a module
;; writing template, sets the `crafted-emacs-home' variable.
(load "~/Documents/develop/crafted-emacs/modules/crafted-init-config")

(server-start)

;;; Packages phase

;; Collect list of packages to install.  Do not just blindly copy this
;; list, instead think about what you need and see if there is a
;; module which provides the list of packages needed.  This phase is
;; not needed if manage the installed packages with Guix or Nix.  It
;; is also not needed if you do not need Crafted Emacs to install
;; packages for a module, for example,
;; `crafted-speedbar-config' does not require any packages to
;; be installed.
(require 'crafted-completion-packages)  ; add completion packages to
                                        ; the
                                        ; `package-selected-packages'
                                        ; list

;; Install the packages listed in the `package-selected-packages' list.
(package-install-selected-packages :noconfirm)

;;; Configuration phase

;; Some example modules to configure Emacs. Don't blindly copy these,
;; they are here for example purposes.  Find the modules which work
;; for you and add them here.
(require 'crafted-updates-config)
(require 'crafted-defaults-config)
(require 'crafted-startup-config)
(require 'crafted-ui-packages)
(require 'crafted-ide-packages)
(require 'crafted-completion-packages)
(require 'crafted-org-packages)

;;; Optional configuration
(require 'custom-module-config)

(package-install-selected-packages :noconfirm)

(require 'crafted-ui-config)
(require 'crafted-osx-config)
(require 'crafted-ide-config)
(require 'crafted-completion-config)
(require 'crafted-org-config)

;; install all language grammars
(crafted-ide-configure-tree-sitter)

;; install all language grammars, except protobuf
(crafted-ide-configure-tree-sitter '(protobuf))

;; custom Configuration
;; (require 'custom-module-config)
;; (package-install-selected-packages :noconfirm)

;; Profile emacs startup
(defun crafted-startup-example/display-startup-time ()
  "Display the startup time after Emacs is fully initialized."
  (message "Crafted Emacs loaded in %s."
           (emacs-init-time)))
(add-hook 'emacs-startup-hook #'crafted-startup-example/display-startup-time)

;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)
