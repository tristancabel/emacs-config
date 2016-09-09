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

(require 'cl-lib)
(package-initialize) 

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; get and install required packages
;; dash -> A modern list api for Emacs. 
;; projectile -> project interaction library for Emacs.
;; helm -> Emacs incremental completion and selection narrowing framework
;; helm-projectile -> Helm UI for Projectile
;; helm-dash -> helm integration with dash
;; magit -> git mode
;; smart-mode-line -> line model
;; undo-tree -> undo and redo functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(setq url-http-attempt-keepalives nil)

(defvar init-packages '(helm projectile esqlite dash helm-projectile magit
			     helm-dash cmake-mode markdown-mode
			     python-mode scala-mode smart-mode-line undo-tree )
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
;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
(setq transient-mark-mode t)

(setq c-default-style "linux" c-basic-offset 2)

;; always replace tabs with spaces
(setq-default indent-tabs-mode nil)

;; set tab width to 4 for all buffers
(setq-default tab-width 4)

;; default to better frame titles
(setq frame-title-format  (concat  "%b - emacs@" system-name))

;; default to unified diffs
(setq diff-switches "-u")

;; set file to auto refresh when change detected (➢ for example: changed by other)
(global-auto-revert-mode 1)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Newline at end of file
(setq require-final-newline t)

;; Highlight current line
;;(global-hl-line-mode +1)

;; Highlight git lines change
(global-git-gutter-mode +1)

;; set custom backup directory  ~ files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; yes or no as y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; save disk space ?
;;(setq delete-old-versions -1)
;;(setq version-control t)
;;(setq vc-make-backup-files t)
;;(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

; kill lines backward
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages configuration
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; smart-mode-line
;; ;;;;;;;;;;;;;;;;;;;;
(setq sml/no-confirm-load-theme t)
(sml/setup)

;; undo-tree
;; ;;;;;;;;;;;;;;;;;;;;
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)

;; magit
;; ;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(global-set-key (kbd "C-=") 'er/expand-region)

;;projectile
;; ;;;;;;;;;;;;;;;;;;;;
;; extra prefix for projectile
;;(define-key map (kbd "s-p") 'projectile-command-map)


;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;helm
;; ;;;;;;;;;;;;;;;;;;;;
;;(setq helm-dash-min-length 3)

(setq helm-candidate-number-limit 100)

;; From https://gist.github.com/antifuchs/9238468
(setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
      helm-input-idle-delay 0.01  ; this actually updates things reeeelatively quickly.
      helm-yas-display-key-on-candidate t
      helm-quick-update t
      helm-M-x-requires-pattern nil
      helm-ff-skip-boring-files t)

(global-set-key(kbd "C-c h") 'helm-mini)
(global-set-key(kbd "C-h a") 'helm-apropos)
(global-set-key(kbd "C-x C-b") 'helm-buffers-list)
(global-set-key(kbd "C-x b") 'helm-buffers-list)
(global-set-key(kbd "M-y") 'helm-show-kill-ring)
(global-set-key(kbd "M-x") 'helm-M-x)
(global-set-key(kbd "C-x c o") 'helm-occur) ;; WHAT
(global-set-key(kbd "C-x c s") 'helm-swoop) ;; WHAT
(global-set-key(kbd "C-x c y") 'helm-yas-complete) ;; WHAT
(global-set-key(kbd "C-x c Y") 'helm-yas-create-snippet-on-region) ;; WHAT
(global-set-key(kbd "C-x c b") 'my/helm-do-grep-book-notes) ;; WHAT
(global-set-key(kbd "C-x c SPC") 'helm-all-mark-rings) ;; WHAT
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally


;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;;(global-set-key (kbd "C-c h") 'helm-command-prefix)
;;(global-unset-key (kbd "C-x c"))

(helm-mode 1)

;;helm-dash

;;helm-projectile

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; C++ Mode for tpp
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(setq auto-mode-alist (append '(("/*.\.tpp$" . c++-mode)) auto-mode-alist))


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
