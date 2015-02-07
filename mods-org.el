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

(defun cid (custom-id)
  (interactive "MCUSTOM_ID: ")
  (org-set-property "CUSTOM_ID" custom-id))

(setq org-default-notes-file "~/Dropbox/gesta/notes.org")
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)

(defun my/begin-end-quote ()
  (my/begin-end "quote")
  )

(defun my/begin-end-verse ()
  (my/begin-end "verse")
  )

(defun my/begin-end (variant)
  (interactive)
  (if (use-region-p)
      (my/begin-end-selected-region variant)
    (my/begin-end-no-selected-region variant)
    )
  )

(defun my/begin-end-selected-region (variant)
  (interactive)
  (my/fix-old-formatting)
  (insert "#+end_" variant)
  (newline)
  (goto-char (region-beginning))
  (insert "#+begin_" variant)
  (newline)
)

(defun my/begin-end-no-selected-region (variant)
  (interactive)
  (insert "#+begin_" variant)
  (newline)
  (newline)
  (insert "#+end_" variant)
  (newline)
  (previous-line)
  (previous-line)
)

(defun my/fix-old-formatting ()
  (my/remove-old-formatting-code)
  (my/indent-if-not-indented)
)

(defun my/remove-old-formatting-code ()
  (setq in (filter-buffer-substring (region-beginning) (region-end) t))
  (setq out (replace-regexp-in-string "^> " "" in))
  (setq out2 (replace-regexp-in-string "\s*<br/>$" "" out))
  (insert out2)
)

(defun my/indent-if-not-indented ()
  (setq firstFour (filter-buffer-substring (region-beginning) (+ (region-beginning) 4)))
  (if (not (string= firstFour "    "))
      (indent-region (region-beginning) (region-end) 4)
    )
)
