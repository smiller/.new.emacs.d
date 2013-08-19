(defun my-text-mode-hook ()
  (typopunct-mode)
  (longlines-mode)
  (linum-mode -1))

(add-hook 'text-mode-hook 'my-text-mode-hook)

(defun my-code-mode-hook ()
  (whitespace-mode)
  (linum-mode)
  (typopunct-mode -1))

(add-hook 'ruby-mode-hook 'my-code-mode-hook)
(add-hook 'js-mode-hook 'my-code-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-code-mode-hook)
