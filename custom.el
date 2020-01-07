(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
    ("c82d24bfba431e8104219bfd8e90d47f1ad6b80a504a7900cbee002a8f04392f" default)))
 '(doom-modeline-bar-width 6)
 '(doom-modeline-buffer-encoding t)
 '(doom-modeline-continuous-word-count-modes (quote (markdown-mode org-mode)))
 '(doom-modeline-enable-word-count t)
 '(doom-modeline-github t)
 '(doom-modeline-github-interval 600)
 '(doom-modeline-gnus t)
 '(doom-modeline-height 25)
 '(doom-modeline-icon t)
 '(doom-modeline-indent-info nil)
 '(doom-modeline-mu4e nil)
 '(doom-modeline-unicode-fallback t)
 '(doom-modeline-vcs-max-length 10)
 '(erc-modules
   (quote
    (autoaway autojoin button completion fill irccontrols keep-place list match menu move-to-prompt netsplit networks noncommands notifications readonly ring services stamp track)))
 '(ess-roxy-template-alist
   (quote
    (("description" . ".. content for \\description{} (no empty lines) ..")
     ("details" . ".. content for \\details{} ..")
     ("title" . "")
     ("param" . "")
     ("return" . ""))))
 '(fci-rule-color "#383838")
 '(gnus-asynchronous t)
 '(inhibit-startup-screen t)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-custom-commands
   (quote
    (("d" "TODAY / NEXT tasks / Agenda"
      ((todo "GOAL" nil)
       (todo "NEXT" nil)
       (agenda ""
	       ((org-agenda-span
		 (quote day))))
       (stuck "" nil))
      nil
      ("~git/meta/main.html")))))
 '(org-agenda-files (quote ("~/git/meta/main.org")))
 '(org-capture-templates
   (quote
    (("i" "inbox")
     ("l" "log")
     ("le" "Emacs Log" entry
      (file+olp "~/git/meta/main.org" "Emacs Personalization" "Log")
      "*** %U ")
     ("ie" "Emacs Inbox" checkitem
      (file+olp "~/git/meta/main.org" "Emacs Personalization" "Inbox")
      "")
     ("im" "Meta Inbox" checkitem
      (file+olp "~/git/meta/main.org" "Meta Management" "Inbox")
      ""))))
 '(org-catch-invisible-edits (quote error))
 '(org-default-priority 67)
 '(org-global-properties
   (quote
    (("Effort_ALL" . "0 0:10 0:30 1:00 2:00 3:00 4:00"))))
 '(org-refile-targets (quote ((nil :maxlevel . 5))))
 '(org-refile-use-outline-path t)
 '(org-stuck-projects (quote ("+PROJECT/-DONE" ("NEXT") nil "")))
 '(org-tags-exclude-from-inheritance (quote ("PROJECT")))
 '(package-selected-packages
   (quote
    (yasnippet-snippets circe ghub doom-modeline doom-themes org-mime htmlize epa-file markdown-mode keyfreq which-key dired-rainbow dired-git-info diredfl dired ess-smart-underscore elpy org-gcal use-package restclient ess poly-R paredit queue spinner clojure-mode cider pdf-tools magit)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(send-mail-function (quote smtpmail-send-it))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 151 :width normal)))))
