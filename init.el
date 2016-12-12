;; .emacs

;; set user name and email address
(setq user-full-name "Tristan Cabel")
(setq user-mail-address "tristan.cabel@amadeus.com")

;;; uncomment this line to disable loading of "default.el" at startup
(setq inhibit-default-init t)

(load-theme 'tango-dark)

(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(require 'cl)
(require 'cl-lib)
(package-initialize) 

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; get and install required packages
;;
;; anaconda-mode
;; beacon -> highlight cursor after window moves
;; company -> Company is a modular in-buffer completion mechanism
;;;; company-jedi -> Company for python
;; company-anaconda
;; company-emacs-eclim -> company for java
;; company-web -> company for web development
;; dash -> A modern list api for Emacs. No 'cl required.
;; ensime -> scala dev environment
;; flycheck -> syntax checker
;; projectile -> project interaction library for Emacs.
;; helm -> Emacs incremental completion and selection narrowing framework
;; helm-projectile -> Helm UI for Projectile
;; helm-flycheck -> Helm UI for flycheck
;; magit -> git mode
;; smart-mode-line -> line model
;; undo-tree -> undo and redo functions
;; use-package -> The use-package macro allows you to isolate package configuration in your .emacs file in a way that is both performance-oriented and, well, tidy.
;; which-key -> which-key is a minor mode for Emacs that displays the key bindings following your currently entered incomplete command
;;
;;
;;   dash -> A modern list api for Emacs. 
;;   helm-dash -> helm integration with dash
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(setq url-http-attempt-keepalives nil)

(defvar init-packages '(helm projectile esqlite helm-projectile magit
                             beacon rich-minority  undo-tree use-package
                             which-key helm-flycheck flycheck company
                             anaconda-mode company-anaconda company-web dash
                             company-quickhelp smart-mode-line ensime
                             cmake-mode markdown-mode js2-mode json-mode
                             python-mode scala-mode yaml-mode ac-html)
  "A list of packages to ensure are installed at launch.")

(defun init-packages-installed-p ()
  (cl-every #'package-installed-p init-packages))

(defun init-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package init-packages)
    (add-to-list 'init-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun init-install-packages ()
"Install all packages listed in `init-packages'."
(unless (init-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (mapc #'init-require-package init-packages)))

;; run package installation
(init-install-packages)

(provide 'init-packages)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; variable configurations
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/variables-configuration.el")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; kill lines backward
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))


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
(use-package      cmake-mode :mode "\\.cmake\\'" "\\CMakeLists.txt\\'")
(use-package javascript-mode :mode "\\.qs\\'")
(use-package       yaml-mode :mode "\\.yml\\'")
(use-package        js2-mode :mode "\\.js\\'")
(use-package       json-mode :mode "\\.json\\'")


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages configuration
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/packages-configuration.el")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rename a file
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; ensime set up
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/ensim-configuration.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-anaconda anaconda-mode helm-flycheck flycheck yaml-mode which-key use-package undo-tree smart-mode-line scala-mode python-mode markdown-mode magit json-mode js2-mode helm-projectile esqlite company cmake-mode beacon ac-html))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#454e51"))))
 '(company-scrollbar-fg ((t (:background "#394143"))))
 '(company-tooltip ((t (:inherit default :background "#32393b"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))
