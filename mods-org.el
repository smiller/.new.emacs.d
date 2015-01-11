(setq org-export-with-sub-superscripts nil)

(global-set-key (kbd "C-c C-g C-p")
		(lambda ()
		  (interactive)
		  (org-publish-project "org")))

(require 'ox-publish)
(setq org-publish-project-alist
      '(
	("org"
	 :base-directory "~/Dropbox/gesta/"
	 :base-extension "org"
	 :publishing-directory "~/Dropbox/gesta_html/"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 4             ; Just the default for this project.
	 :auto-preamble t
	 )
	))

(global-set-key (kbd "M-s M-q")
		(lambda()
		  (interactive)
		  (insert "#+begin_quote")
		  (newline)
		  (newline)
		  (insert "#+end_quote")
		  (newline)
		  (previous-line)
		  (previous-line)))

(global-set-key (kbd "M-s M-v")
		(lambda()
		  (interactive)
		  (insert "#+begin_verse")
		  (newline)
		  (newline)
		  (insert "#+end_verse")
		  (newline)
		  (previous-line)
		  (previous-line)))
