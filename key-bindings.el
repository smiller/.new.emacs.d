(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "C-c h") 'helm-projectile)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; http://whattheemacsd.com/key-bindings.el-03.html

(global-set-key (kbd "M-j")
  (lambda ()
    (interactive)
    (join-line -1)))

;;

(global-set-key (kbd "C-c r g q")
  (lambda ()
    (interactive)
    (find-file "db/structure.sql")))

(global-set-key (kbd "C-x RET <right>") 'windmove-right)
(global-set-key (kbd "C-x RET <left>") 'windmove-left)
(global-set-key (kbd "C-x RET <up>") 'windmove-up)
(global-set-key (kbd "C-x RET <down>") 'windmove-down)

(global-set-key (kbd "C-c C-g C-d")
		(lambda ()
		  (interactive)
		  (setq default-directory "~/Dropbox/gesta/")
		  (dired ".")))

(global-set-key (kbd "C-c C-g C-h")
		(lambda ()
		  (interactive)
		  (setq default-directory "~/Dropbox/gesta/")
		  (find-file "2015.org")))

(global-set-key (kbd "C-c C-g C-t")
		(lambda ()
		  (interactive)
		  (setq default-directory "~/Dropbox/gesta/")
		  (find-file "todo.org")))

(global-set-key (kbd "C-c C-g C-a")
		(lambda ()
		  (interactive)
		  (setq default-directory "~/Dropbox/gesta/")
		  (find-file "2015.org")
                  (split-window-horizontally)
		  (find-file "todo.org")))

(global-set-key (kbd "C-c C-g C-u")
		(lambda ()
		  (interactive)
		  (setq default-directory "~/code/autrui/")
		  (dired ".")))
