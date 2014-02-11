(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "C-c h") 'helm-projectile)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(define-abbrev-table 'global-abbrev-table '(
  ("a5" "à")
  ("e5" "è")
  ("i5" "ì")
  ("o5" "ò")
  ("u5" "ù")
  ("a6" "â")
  ("e6" "ê")
  ("i6" "î")
  ("o6" "ô")
  ("u6" "û")
  ("c6" "ç")
  ("a7" "á")
  ("e7" "é")
  ("i7" "ī")
  ("o7" "ō")
  ("u7" "ū")
  ("a8" "ā")
  ("e8" "ē")
  ("i8" "ī")
  ("o8" "ō")
  ("u8" "ū")
  ("t8" "þ")
  ("T8" "Þ")
  ("d8" "ð")
  ("D8" "Ð")
  ("ae8" "æ")
  ("AE8" "Æ")
))

(setq-default abbrev-mode t)

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
