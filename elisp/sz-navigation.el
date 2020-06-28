(require 'sz-utils)

(defun jump-to-init-file ()
  "Open current user's init file."
  (interactive)
  (find-file user-init-file))

(eval-after-load 'ivy
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-count-format "(%d/%d) "))

(use-package counsel
  :after ivy
  :config (counsel-mode 1))

(use-package swiper
  :bind (("C-s" . swiper-isearch))
  :after ivy)

(use-package flx)

(use-package ivy-rich
  :after ivy
  :demand
  :init
  (setq ivy-rich--original-display-transformers-list nil) 
  (setq ivy-rich-path-style 'abbrev
	  ivy-virtual-abbreviate 'full)
  :config
  (ivy-rich-mode 1))

(defun setup-navigation ())


(provide 'sz-navigation)
