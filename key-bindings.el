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
