(require 'ruby-tools)

(global-set-key (kbd "C-c q :") 'ruby-tools-to-symbol)
(global-set-key (kbd "C-c q '") 'ruby-tools-to-single-quote-string)
(global-set-key (kbd "C-c q \"") 'ruby-tools-to-double-quote-string)

; http://blog.senny.ch/blog/2012/10/06/emacs-tidbits-for-ruby-developers/
(defun senny-ruby-interpolate ()
  "In a double quoted string, interpolate."
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

(eval-after-load 'enh-ruby-mode
  '(progn
     (define-key enh-ruby-mode-map (kbd "#") 'senny-ruby-interpolate)))
