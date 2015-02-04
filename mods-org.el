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

(defun cid ()
  (interactive)
  (setq custom_id (read-from-minibuffer "CUSTOM_ID: "))
  (org-set-property "CUSTOM_ID" custom_id))

(setq org-default-notes-file "~/Dropbox/gesta/notes.org")
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)

(defun begin-end-quote ()
  (interactive)
  (if (use-region-p)
      (begin-end-quote-for-region)
    (begin-end-quote-new)
    )
  )

(defun begin-end-quote-for-region ()
  (interactive)
  (fix-old-formatting)
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
  (fix-old-formatting)
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

(defun fix-old-formatting ()
  (remove-old-formatting-code)
  (indent-if-not-indented)
)

(defun remove-old-formatting-code ()
  (setq in (filter-buffer-substring (region-beginning) (region-end) t))
  (setq out (replace-regexp-in-string "^> " "" in))
  (setq out2 (replace-regexp-in-string "\s*<br/>$" "" out))
  (insert out2)
)

(defun indent-if-not-indented ()
  (setq firstFour (filter-buffer-substring (region-beginning) (+ (region-beginning) 4)))
  (if (not (string= firstFour "    "))
      (indent-region (region-beginning) (region-end) 4)
    )
)
