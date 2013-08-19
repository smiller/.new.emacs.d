(require 'rspec-mode)

(setq rspec-use-rvm t)
(setq rspec-use-rake-when-possible nil)
(setq rspec-use-zeus-when-possible t)

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))
(ad-activate 'rspec-compile)
