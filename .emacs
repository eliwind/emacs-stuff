;; Set up package system
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))


;; Postpone loading configuration until after pacakges have been loaded
(add-hook 'after-init-hook
		  (lambda() (load (concat user-emacs-directory ".emacs-real.el"))))
