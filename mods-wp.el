;; http://vxlabs.com/2014/05/25/emacs-24-with-prelude-org2blog-and-wordpress/
;; but following the org2blog README and using auth-soure instead of netrc
;; https://github.com/punchagan/org2blog/commit/52be89507c337e5f74be831ca563a8023e0ec736

(setq org-directory "~/Dropbox/gesta/twc/")
;; and you need this, else you'll get symbol void errors when doing
;; fill paragraph
(setq org-list-allow-alphabetical t)

(require 'org2blog-autoloads)
(require 'auth-source)
(let (credentials)
        (add-to-list 'auth-sources "~/.authinfo")
        (setq credentials (auth-source-user-and-password "thewanderingcoder"))
	(setq org2blog/wp-blog-alist
	      `(("twc"
		 :url "http://thewanderingcoder.com/xmlrpc.php"
		 :username ,(car credentials)
		 :password ,(cadr credentials)
		 :default-title "Hello World"
		 :default-categories ("org2blog" "emacs")
		 :tags-as-categories nil))))

;; http://blog.binchen.org/posts/how-to-use-org2blog-effectively-as-a-programmer.html
;; has half the instructions, but was missing
;; `wp-use-sourcecode-shortcode` at the time of this writing, without
;; which this does not work at all.

;; * `M-x package-install RET htmlize` is required, else you get empty
;;   code blocks https://github.com/punchagan/org2blog/blob/master/org2blog.el
;; * with wp-use-sourcecode-shortcode set to 't, org2blog will use 1
;;   shortcodes, and hence the SyntaxHighlighter Evolved plugin on your blog.
;;   however, if you set this to nil, native Emacs highlighting will be used,
;;   implemented as HTML styling. Your pick!
(setq org2blog/wp-use-sourcecode-shortcode 't)
;; removed light="true"
(setq org2blog/wp-sourcecode-default-params nil)
;; target language needs to be in here
(setq org2blog/wp-sourcecode-langs
      '("actionscript3" "bash" "coldfusion" "cpp" "csharp" "css" "delphi"
        "erlang" "fsharp" "diff" "groovy" "javascript" "java" "javafx" "matlab"
        "objc" "perl" "php" "text" "powershell" "python" "ruby" "scala" "sql"
        "vb" "xml"
        "sh" "emacs-lisp" "lisp" "lua"))

;; this will use emacs syntax higlighting in your #+BEGIN_SRC
;; <language> <your-code> #+END_SRC code blocks.
(setq org-src-fontify-natively t)
