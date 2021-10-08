;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
   '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents)

;; Download Evil
(unless (package-installed-p 'evil)
    (package-install 'evil))

;; Download Slime
(unless (package-installed-p 'slime)
    (package-install 'slime))

;; Enable Evil
(require 'evil)
(evil-mode 1)

;; Enable slime
(require 'slime)
(setq inferior-lisp-program "sbcl")

;; Make 'delete' on my iPad keyboard behave like 'backspace'
;; or something like that, I was certain that this would break
;; it for my workstation keyboard -- but it still works. Go
;; figure.
(keyboard-translate ?\C-h ?\C-?)
