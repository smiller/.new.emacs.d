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

(ert-deftest test-begin-end-quote-new-content ()
  "Tests begin-end-quote without selected region string"
  (should (string= (with-temp-buffer
		   (begin-end-quote)
		   (buffer-string))
		 "#+begin_quote\n\n#+end_quote\n")))

(ert-deftest test-begin-end-quote-new-point ()
  "Tests begin-end-quote without selected region cursor position"
  (should (equal (with-temp-buffer
		   (begin-end-quote)
		   (point))
		 (length "#+begin_quote\n\n"))))

(ert-deftest test-begin-end-verse-new-content ()
  "Tests begin-end-verse without selected region string"
  (should (string= (with-temp-buffer
		   (begin-end-verse)
		   (buffer-string))
		 "#+begin_verse\n\n#+end_verse\n")))

(ert-deftest test-begin-end-verse-new-point ()
  "Tests begin-end-verse without selected region cursor position"
  (should (equal (with-temp-buffer
		   (begin-end-quote)
		   (point))
		 (length "#+begin_verse\n\n"))))

(ert-deftest test-begin-end-quote-region ()
  "Tests begin-end-quote with selected region"
  (should (string= (with-temp-buffer
		   (insert "> Dear Sir, your astonishment’s odd;\n")
		   (set-mark 0)
		   (begin-end-quote)
		   (buffer-string))
		 "#+begin_quote\n    Dear Sir, your astonishment’s odd;\n#+end_quote\n")))

(ert-deftest test-begin-end-verse-region ()
  "Tests begin-end-verse with selected region, two spaces before <br/>"
  (should (string= (with-temp-buffer
		   (insert "> Dear Sir, your astonishment’s odd;  <br/>\n")
		   (set-mark 0)
		   (end-of-buffer)
		   (begin-end-verse)
		   (buffer-string))
		 "#+begin_verse\n    Dear Sir, your astonishment’s odd;\n#+end_verse\n")))

(ert-deftest test-begin-end-verse-region-one-space ()
  "Tests begin-end-verse with selected region, one space before <br/>"
  (should (string= (with-temp-buffer
		   (insert "> Dear Sir, your astonishment’s odd; <br/>\n")
		   (set-mark 0)
		   (end-of-buffer)
		   (begin-end-verse)
		   (buffer-string))
		 "#+begin_verse\n    Dear Sir, your astonishment’s odd;\n#+end_verse\n")))

(ert-deftest test-begin-end-verse-region-no-space ()
  "Tests begin-end-verse with selected region, no space before <br/>"
  (should (string= (with-temp-buffer
		   (insert "> Dear Sir, your astonishment’s odd;<br/>\n")
		   (set-mark 0)
		   (end-of-buffer)
		   (begin-end-verse)
		   (buffer-string))
		 "#+begin_verse\n    Dear Sir, your astonishment’s odd;\n#+end_verse\n")))

(ert-deftest test-begin-end-quote-region-newer-styling ()
  "Tests begin-end-quote with selected region, new styling"
  (should (string= (with-temp-buffer
		   (insert "    talsernilli barenya-tar falon enyi\n")
		   (set-mark 0)
		   (end-of-buffer)
		   (begin-end-quote)
		   (buffer-string))
		 "#+begin_quote\n    talsernilli barenya-tar falon enyi\n#+end_quote\n")))

(ert-deftest test-begin-end-verse-region-newer-styling ()
  "Tests begin-end-verse with selected region, new styling"
  (should (string= (with-temp-buffer
		   (insert "    i talessen\n    talalor ar-entai kirenyi\n    basleth solth sares\n")
		   (set-mark 0)
		   (end-of-buffer)
		   (begin-end-verse)
		   (buffer-string))
		 "#+begin_verse\n    i talessen\n    talalor ar-entai kirenyi\n    basleth solth sares\n#+end_verse\n")))

(ert-deftest test-begin-end-quote-region-newer-styling-with-medial-gt ()
  "Tests begin-end-quote with selected region, new styling, with > in line"
  (should (string= (with-temp-buffer
		   (insert "    count > 42\n")
		   (set-mark 0)
		   (end-of-buffer)
		   (begin-end-quote)
		   (buffer-string))
		 "#+begin_quote\n    count > 42\n#+end_quote\n")))

(ert-deftest test-begin-end-verse-region-newer-styling-with-medial-gt ()
  "Tests begin-end-verse with selected region, new styling, with > in line"
  (should (string= (with-temp-buffer
		   (insert "    count > 42<br/>\n")
		   (set-mark 0)
		   (end-of-buffer)
		   (begin-end-verse)
		   (buffer-string))
		 "#+begin_verse\n    count > 42\n#+end_verse\n")))
