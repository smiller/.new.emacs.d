(require 'cask "~/.cask/cask.el")
(cask-initialize)
(load-file "~/.emacs.d/tangled-settings.el")
(load-file "~/.emacs.d/tangled-code.el")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-gnus ((t (:foreground "green"))))
 '(bmkp-heading ((t (:foreground "green"))))
 '(bmkp-local-directory ((t (:background "black" :foreground "green"))))
 '(bmkp-local-file-with-region ((t (:foreground "green"))))
 '(bmkp-local-file-without-region ((t (:foreground "green"))))
 '(helm-selection ((t (:background "green" :underline nil)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(org-agenda-files (quote ("~/Dropbox/gesta/2015.org"))))
