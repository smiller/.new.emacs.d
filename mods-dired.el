(require 'dired-x)

;; http://whattheemacsd.com/setup-dired.el-01.html

;; Make dired less verbose
(require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

;; http://whattheemacsd.com/setup-dired.el-02.html

(setq-default dired-omit-files-p t)

(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line (if dired-omit-mode 2 4)))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
