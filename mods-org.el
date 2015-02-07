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
  (interactive)
  (my/begin-end "quote" "quote"))

(defun my/begin-end-verse ()
  (interactive)
  (my/begin-end "verse" "verse"))

(defun my/begin-end-example ()
  (interactive)
  (my/begin-end "example" "example"))

(defun my/begin-end-src-emacs-lisp ()
  (interactive)
  (my/begin-end "src emacs-lisp" "src"))

(defun my/begin-end-src-ruby ()
  (interactive)
  (my/begin-end "src ruby" "src"))

(defun my/begin-end (begin-tag end-tag)
  (interactive)
  (let ((cited-string "\n"))
    (when (use-region-p)
      (setq cited-string
	    (my/remove-old-citation-formatting (buffer-substring-no-properties (region-beginning) (region-end))))
      (delete-region (region-beginning) (region-end)))
    (insert "#+begin_" begin-tag "\n"
	    cited-string
	    "#+end_" end-tag "\n"))
    (unless (use-region-p)
      (forward-line -2)))

(defun my/remove-old-citation-formatting (str)
  (interactive)
  (if (string= (substring str 0 2) "> ")
      (replace-regexp-in-string "^> " "    "
				(replace-regexp-in-string "\s*<br/>" ""
							    (replace-regexp-in-string "\n!$" "\n    " str)))
    str))
