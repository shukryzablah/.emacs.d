(defun setup-vscode-theme ()
  "Install and load vscode-dark-plus-theme."
  (package-install-if-not-already 'vscode-dark-plus-theme)
  (load-theme 'vscode-dark-plus t))

(defun setup-zenburn-theme ()
  "Install and load zenburn-theme"
  (package-install-if-not-already 'zenburn-theme)
  (load-theme 'zenburn t))

(defun disable-all-themes ()
  (interactive)
  "Disable all enabled themes."
  (mapc #'disable-theme custom-enabled-themes))

(defun setup-theme (theme-name)
  "Setup a specific supported theme."
  (cond ((equal theme-name "vscode") (setup-vscode-theme))
	((equal theme-name "zenburn") (setup-zenburn-theme))
	(t (warn "Theme is not supported: %s" theme-name))))

(defun change-theme (theme-name)
  "Disable themes and setup new theme."
  (interactive "sTheme: ")
  (disable-all-themes)
  (setup-theme theme-name))

(provide 'sz-themes)
