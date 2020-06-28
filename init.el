;; -*- lexical-binding: t; -*-
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(require 'package)
(require 'sz-utils)

(defun setup-melpa ()
  "Add melpa to repositories and call package-initialize afterwards."
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(defun setup-custom-file ()
  "Set file path used for Easy Customization."
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file))

(defun main ()
  "Entry point for init file."
  (with-gc-cons-threshold (* 50 1000 1000)
   (setup-custom-file) 
   (setup-melpa)
   (package-initialize) ; TODO: is this necessary?
   (package-refresh-contents) ; TODO: too slow
   (mapc #'package-install-if-not-already '(use-package magit))
   (global-set-key (kbd "C-x g") 'magit-status) ; TODO: find another place
   (if (require-no-error 'sz-themes)
       (setup-theme "vscode"))
   (if (require-no-error 'sz-programming)
       (setup-common-lisp))))

(main)
