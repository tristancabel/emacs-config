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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get and install required packages
;;
;; beacon -> highlight cursor after window moves
;; company -> Company is a modular in-buffer completion mechanism
;; company-web -> company for web development
;; conda  -> to work with conda environment
;; dash -> A modern list api for Emacs. No 'cl required.
;; flycheck -> syntax checker
;; git-gutter -> to have + = on line changes by git
;; ccls -> replace cquery. c++ front end for language server protocol (eglot)
;;;;; eglot -> Language Server Protocol back end
;; lsp-mode  -> Language Server Protocol back end
;; lsp-ui -> UI modules of lsp-mode
;; lsp-python-ms -> python for lsp mode
;; projectile -> project interaction library for Emacs.
;; helm -> Emacs incremental completion and selection narrowing framework
;; helm-ag -> helm with ag for search
;; helm-dash -> helm integration with dash
;; helm-projectile -> Helm UI for Projectile
;; helm-flycheck -> Helm UI for flycheck
;; magit -> git mode
;;;;;; realgud -> better debugger
;; smart-mode-line -> line model
;; undo-tree -> undo and redo functions
;; use-package -> The use-package macro allows you to isolate package configuration in your .emacs file in a way that is both performance-oriented and, well, tidy.
;; which-key -> which-key is a minor mode for Emacs that displays the key bindings following your currently entered incomplete command
;; neotree -> tree navigation mode (activated on F8)
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
 '(ispell-dictionary nil)
 '(ivy-mode t)
 '(lsp-auto-guess-root nil)
 '(lsp-prefer-flymake nil t)
 '(package-selected-packages
        '(browse-kill-ring markdown-mode json-mode eglot company conda projectile counsel ivy which-key beacon git-gutter-fringe magit which-key-mode beacon-mode undo-tree smart-mode-line use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#45bb4ed351db"))) t)
 '(company-scrollbar-fg ((t (:background "#39f441834408"))) t)
 '(company-tooltip ((t (:inherit default :background "#32e339873bbd"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-scrollbar-thumb ((t (:background "#39f441834408"))))
 '(company-tooltip-scrollbar-track ((t (:background "#45bb4ed351db"))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))
