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

(defun begin-end-quote ()
  (interactive)
  (if (use-region-p)
      (begin-end-quote-for-region)
    (begin-end-quote-new)
    )
  )

(defun begin-end-quote-for-region ()
  (interactive)
  (insert "#+end_quote")
  (newline)
  (goto-char (region-beginning))
  (insert "#+begin_quote")
  (newline)
)

(defun begin-end-quote-new ()
  (interactive)
  (insert "#+begin_quote")
  (newline)
  (newline)
  (insert "#+end_quote")
  (newline)
  (previous-line)
  (previous-line)
)

(defun begin-end-verse ()
  (interactive)
  (if (use-region-p)
      (begin-end-verse-for-region)
    (begin-end-verse-new)
    )
  )

(defun begin-end-verse-for-region ()
  (interactive)
  (insert "#+end_verse")
  (newline)
  (goto-char (region-beginning))
  (insert "#+begin_verse")
  (newline)
)

(defun begin-end-verse-new ()
  (interactive)
  (insert "#+begin_verse")
  (newline)
  (newline)
  (insert "#+end_verse")
  (newline)
  (previous-line)
  (previous-line)
)
