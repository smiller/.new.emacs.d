(defun my-text-mode-hook ()
  (typopunct-mode)
  (linum-mode -1)
  (auto-fill-mode 1)
  (set-input-method "TeX")
  (define-key org-mode-map (kbd "M-s M-q") 'begin-end-quote)
  (define-key org-mode-map (kbd "M-s M-v") 'begin-end-verse)
)

(add-hook 'text-mode-hook 'my-text-mode-hook)

(defun my-code-mode-hook ()
  (whitespace-mode)
  (linum-mode)
  (typopunct-mode -1))

(add-hook 'ruby-mode-hook 'my-code-mode-hook)
(add-hook 'ruby-mode-hook
  (lambda () (rvm-activate-corresponding-ruby)))
(add-hook 'js-mode-hook 'my-code-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-code-mode-hook)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
