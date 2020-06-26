;; -*- lexical-binding: t; -*-

(require 'package)

(defun setup-melpa ()
  "Add melpa to repositories and call package-initialize afterwards."
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  ; TODO: is the following call necessary? 
  (package-initialize))

(defun package-install-if-not-already (package)
  "Install package only if not already installed."
  (unless (package-installed-p package)
    (package-install package)))

(defun setup-custom-file ()
  "Set file path used for Easy Customization."
  (setq custom-file "~/.emacs.d/custom.el"))

(defun setup-vscode-theme ()
  "Install and load vscode-dark-plus-theme."
  (package-install-if-not-already 'vscode-dark-plus-theme)
  (load-theme 'vscode-dark-plus t))

(defun setup-zenburn-theme ()
  "Install and load zenburn-theme"
  (package-install-if-not-already 'zenburn-theme)
  (load-theme 'zenburn t))

(defun disable-all-themes ()
  "Disable all enabled themes."
  (mapc #'(disable-theme) 'custom-enabled-themes))

(defun change-theme (theme-name)
  "Disable themes and setup new theme."
  (disable-all-themes)
  (setup-theme theme-name))

(defun setup-theme (theme-name)
  "Setup a specific supported theme."
  (cond ((equal theme-name "vscode") (setup-vscode-theme))
	((equal theme-name "zenburn") (setup-zenburn-theme))
	(t (warn "Theme is not supported: %s" theme-name))))

(defun main ()
  "Entry point for init file."
  (setup-melpa) 
  ;(package-refresh-contents) ; TODO: too slow, but how to deal with outdated cache or when bootstrapping?
  (mapc #'package-install-if-not-already '(use-package magit))
  (setup-custom-file) 
  (setup-theme "vscode")
  (global-set-key (kbd "C-x g") 'magit-status))

(main)
