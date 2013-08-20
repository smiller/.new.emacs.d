;; -- disable stuff ----------------------------------------------------------
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode -1)

;; -- rows and columns -------------------------------------------------------
(setq line-number-mode t)
(setq column-number-mode t)
(setq linum-format "%4d ")
(setq-default fill-column 80)

;; -- windmove ---------------------------------------------------------------
(define-key input-decode-map "\e[1;2A" [S-up])
(windmove-default-keybindings)
(setq windmove-wrap-around t)

(global-hl-line-mode 1)

(projectile-global-mode)
