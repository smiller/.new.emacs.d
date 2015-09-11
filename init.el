
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(load-file "~/.emacs.d/tangled-settings.el")
(load-file "~/.emacs.d/tangled-code.el")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#FDF5E6" :foreground "grey22" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(bmkp-gnus ((t (:foreground "green"))))
 '(bmkp-heading ((t (:foreground "green"))))
 '(bmkp-local-directory ((t (:background "black" :foreground "green"))))
 '(bmkp-local-file-with-region ((t (:foreground "green"))))
 '(bmkp-local-file-without-region ((t (:foreground "green"))))
 '(bold ((t (:weight ultra-bold))))
 '(helm-selection ((t (:background "green" :underline nil))))
 '(italic ((t (:foreground "mediumseagreen" :slant oblique))))
 '(org-link ((t (:foreground "purple3" :underline t)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(org-agenda-files (quote ("~/Dropbox/gesta/2015.org"))))
