(defun package-install-if-not-already (package)
  "Install package only if not already installed."
  (unless (package-installed-p package)
    (package-install package)))

(defmacro require-no-error (feature)
  `(require ,feature nil t))

(defmacro with-gc-cons-threshold (bytes &rest body)
  "Run a body of code with new gc-cons-threshold defined in bytes."
  `(let ((gc-cons-threshold ,bytes))
     ,@body))

(provide 'sz-utils)
