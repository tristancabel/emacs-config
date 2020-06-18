;;; package --- Summary
;;; Commentary:
;;; that's my Emacs

;;; Code:
;; set user name and email address
(setq user-full-name "Tristan Cabel")
(setq user-mail-address "tristan.cabel@inria.fr")

;;; uncomment this line to disable loading of "default.el" at startup
(setq inhibit-default-init t)

(load-theme 'tango-dark)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;(add-to-list 'package-archives
;             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Dependencies -> see the readme

;; to test yas-snippet

;;;;; eglot -> Language Server Protocol back end
;; rainbow-mode -> colorize color names
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq url-http-attempt-keepalives nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variable configurations
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/variables-configuration.el")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.scala\\'"       . scala-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'"           . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'"           . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc$"            . c++-mode))
(add-to-list 'auto-mode-alist '("\\.C$"             . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp$"           . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx$"           . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tpp\\'"         . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tcc$"           . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp$"           . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hxx$"           . c++-mode))
(add-to-list 'auto-mode-alist '("\\.qdoc$"          . c++-mode))
(add-to-list 'auto-mode-alist '(".gitignore\\'"     . makefile-mode))
(add-to-list 'auto-mode-alist '(".gitattributes\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("qmldir\\'"         . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.pr[io]\\'"      . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.info\\'"        . info-mode))
(add-to-list 'auto-mode-alist '("\\.qmltypes\\'"    . json-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'"         . html-mode))
(use-package        js2-mode :mode "\\.js\\'")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages configuration
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/packages-configuration.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(conda-anaconda-home "/home/trcabel/miniconda3/")
 '(ivy-mode t)
 '(lsp-auto-guess-root nil)
 '(lsp-prefer-flymake nil t)
 '(package-selected-packages
   (quote
    (markdown-mode json-mode eglot company conda projectile counsel ivy which-key beacon git-gutter-fringe magit which-key-mode beacon-mode undo-tree smart-mode-line use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#458d4e9f51a5"))))
 '(company-scrollbar-fg ((t (:background "#39c6414f43d2"))))
 '(company-tooltip ((t (:inherit default :background "#32b539533b87"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))
