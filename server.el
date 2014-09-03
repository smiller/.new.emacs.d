;; http://devblog.avdi.org/2010/04/23/daemonic-emacs/
;; http://devblog.avdi.org/2011/10/27/running-emacs-as-a-server-emacs-reboot-15/
(server-start)

;; Create and chmod +x /usr/local/bin/ec
;; #!/bin/sh
;; exec /usr/bin/env emacsclient -c -a '' $*
;; and add export EDITOR=ec to ~/.zshrc
