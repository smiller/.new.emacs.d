(load-file "mods-org.el")

(ert-deftest test-begin-end-quote-new-content ()
  "Tests begin-end-quote without selected region string"
  (should (string= (with-temp-buffer
		     (my/begin-end-quote)
		     (buffer-string))
		   "#+begin_quote\n\n#+end_quote\n")))

(ert-deftest test-begin-end-quote-new-point ()
  "Tests begin-end-quote without selected region cursor position"
  (should (equal (with-temp-buffer
		   (my/begin-end-quote)
		   (point))
		 (length "#+begin_quote\n\n"))))

(ert-deftest test-begin-end-verse-new-content ()
  "Tests begin-end-verse without selected region string"
  (should (string= (with-temp-buffer
		     (my/begin-end-verse)
		     (buffer-string))
		   "#+begin_verse\n\n#+end_verse\n")))

(ert-deftest test-begin-end-verse-new-point ()
  "Tests begin-end-verse without selected region cursor position"
  (should (equal (with-temp-buffer
		   (my/begin-end-quote)
		   (point))
		 (length "#+begin_verse\n\n"))))

(ert-deftest test-begin-end-quote-region ()
  "Tests begin-end-quote with selected region"
  (should (string= (with-temp-buffer
		     (insert "> Dear Sir, your astonishment’s odd;\n")
		     (select-region)
		     (my/begin-end-quote)
		     (buffer-string))
		   "#+begin_quote\n    Dear Sir, your astonishment’s odd;\n#+end_quote\n")))

(ert-deftest test-begin-end-verse-region ()
  "Tests begin-end-verse with selected region, two spaces before <br/>"
  (should (string= (with-temp-buffer
		     (insert "> Dear Sir, your astonishment’s odd;  <br/>\n")
		     (select-region)
		     (my/begin-end-verse)
		     (buffer-string))
		   "#+begin_verse\n    Dear Sir, your astonishment’s odd;\n#+end_verse\n")))

(ert-deftest test-begin-end-verse-region-one-space ()
  "Tests begin-end-verse with selected region, one space before <br/>"
  (should (string= (with-temp-buffer
		     (insert "> Dear Sir, your astonishment’s odd; <br/>\n")
		     (select-region)
		     (my/begin-end-verse)
		     (buffer-string))
		   "#+begin_verse\n    Dear Sir, your astonishment’s odd;\n#+end_verse\n")))

(ert-deftest test-begin-end-verse-region-no-space ()
  "Tests begin-end-verse with selected region, no space before <br/>"
  (should (string= (with-temp-buffer
		     (insert "> Dear Sir, your astonishment’s odd;<br/>\n")
		     (select-region)
		     (my/begin-end-verse)
		     (buffer-string))
		   "#+begin_verse\n    Dear Sir, your astonishment’s odd;\n#+end_verse\n")))

(ert-deftest test-begin-end-quote-region-newer-styling ()
  "Tests begin-end-quote with selected region, new styling"
  (should (string= (with-temp-buffer
		     (insert "    talsernilli barenya-tar falon enyi\n")
		     (select-region)
		     (my/begin-end-quote)
		     (buffer-string))
		   "#+begin_quote\n    talsernilli barenya-tar falon enyi\n#+end_quote\n")))

(ert-deftest test-begin-end-verse-region-newer-styling ()
  "Tests begin-end-verse with selected region, new styling"
  (should (string= (with-temp-buffer
		     (insert "    i talessen\n    talalor ar-entai kirenyi\n    basleth solth sares\n")
		     (select-region)
		     (my/begin-end-verse)
		     (buffer-string))
		   "#+begin_verse\n    i talessen\n    talalor ar-entai kirenyi\n    basleth solth sares\n#+end_verse\n")))

(ert-deftest test-begin-end-quote-region-newer-styling-with-medial-gt ()
  "Tests begin-end-quote with selected region, new styling, with > in line"
  (should (string= (with-temp-buffer
		     (insert "    count > 42\n")
		     (select-region)
		     (my/begin-end-quote)
		     (buffer-string))
		   "#+begin_quote\n    count > 42\n#+end_quote\n")))

(ert-deftest test-begin-end-verse-region-newer-styling-with-medial-gt ()
  "Tests begin-end-verse with selected region, new styling, with > in line"
  (should (string= (with-temp-buffer
		     (insert "    count > 42\n")
		     (select-region)
		     (my/begin-end-verse)
		     (buffer-string))
		   "#+begin_verse\n    count > 42\n#+end_verse\n")))

(ert-deftest test-begin-end-example ()
  "Tests begin-end-example"
  (should (string= (with-temp-buffer
		     (insert "exemplaria\ninter alia\n")
		     (select-region)
		     (my/begin-end-example)
		     (buffer-string))
		   "#+begin_example\nexemplaria\ninter alia\n#+end_example\n")))

(ert-deftest test-begin-end-src-emacs-lisp ()
  "Tests begin-end-src-emacs-lisp"
  (should (string= (with-temp-buffer
		     (insert "(defun my/begin-end-example\n(interactive)\n(my/begin-end \"example\"))\n")
		     (select-region)
		     (my/begin-end-src-emacs-lisp)
		     (buffer-string))
		   "#+begin_src emacs-lisp\n(defun my/begin-end-example\n(interactive)\n(my/begin-end \"example\"))\n#+end_src\n")))

(ert-deftest test-begin-end-src-ruby ()
  "Tests begin-end-src-ruby"
  (should (string= (with-temp-buffer
		     (insert "class User < ActiveRecord::Base\n  include Permissions\n")
		     (select-region)
		     (my/begin-end-src-ruby)
		     (buffer-string))
		   "#+begin_src ruby\nclass User < ActiveRecord::Base\n  include Permissions\n#+end_src\n")))

(defun select-region ()
  (goto-char (point-min))
  (set-mark-command nil)
  (goto-char (point-max))
  (transient-mark-mode 1)
  )
