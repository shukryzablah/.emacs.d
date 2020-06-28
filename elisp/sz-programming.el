(require 'sz-utils)

(defvar inferior-lisp-program "sbcl"
  "Preferred inferior lisp program")

(defun setup-common-lisp ()
  (package-install-if-not-already 'slime)
  (with-eval-after-load 'slime
    (load (expand-file-name "~/quicklisp/slime-helper.el"))
    (setq inferior-lisp-program "/usr/bin/sbcl"
	  slime-contribs '(slime-fancy slime-quicklisp))))

(provide 'sz-programming)
