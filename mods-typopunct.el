(require 'typopunct)

(typopunct-change-language 'english t)
;; easy switch between French and English
(defun to-french()
  (interactive)
  (typopunct-change-language 'francais t))
(global-set-key (kbd "C-c C-g C-f") 'to-french)
(defun to-english()
  (interactive)
  (typopunct-change-language 'english t))
(global-set-key (kbd "C-c C-g C-e") 'to-english)

;; http://www.emacswiki.org/emacs/TypographicalPunctuationMarks
(defconst typopunct-ellipsis (decode-char 'ucs #x2026))
    (defun typopunct-insert-ellipsis-or-middot (arg)
      "Change three consecutive dots to a typographical ellipsis mark."
      (interactive "p")
      (cond
       ((and (= 1 arg)
             (eq (char-before) ?^))
        (delete-char -1)
        (insert typopunct-middot))
       ((and (= 1 arg)
             (eq this-command last-command)
             (looking-back "\\.\\."))
        (replace-match "")
        (insert typopunct-ellipsis))
       (t
        (self-insert-command arg))))
    (define-key typopunct-map "." 'typopunct-insert-ellipsis-or-middot)

(global-set-key (kbd "C-c d") 'typopunct-mode)
