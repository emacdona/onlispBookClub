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

;; Dockerfile
(unless (package-installed-p 'dockerfile-mode)
  (package-install 'dockerfile-mode))

;; Enable Evil
(require 'evil)
(evil-mode 1)

;; Enable slime
(require 'slime)
(setq inferior-lisp-program "sbcl")

;; Enable Dockerfile mode
(require 'dockerfile-mode)

;what a fucking pain
;https://github.com/sellout/emacs-color-theme-solarized
;https://www.philipdaniels.com/blog/2017/spacemacs-solarized/
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(set-terminal-parameter nil 'background-mode 'dark)
(set-frame-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)

;; Make 'delete' on my iPad keyboard behave like 'backspace'
;; or something like that, I was certain that this would break
;; it for my workstation keyboard -- but it still works. Go
;; figure.
(keyboard-translate ?\C-h ?\C-?)
