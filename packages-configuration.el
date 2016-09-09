
;; smart-mode-line
;; ;;;;;;;;;;;;;;;;;;;;
(setq sml/no-confirm-load-theme t)
(sml/setup)

;; undo-tree
;; ;;;;;;;;;;;;;;;;;;;;
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)

;; highlight cursor after window moves
;; ;;;;;;;;;;;;;;;;;;;;
(beacon-mode 1)

;; which-key
;; ;;;;;;;;;;;;;;;;;;;;
(which-key-mode)

;; Font size
;; ;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-+") 'zoom-frm-in)
(global-set-key (kbd "C--") 'zoom-frm-out)

;; magit
;; ;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c v m") 'magit-status)
(global-set-key (kbd "C-c v l") 'magit-log)
(global-set-key (kbd "C-c v b") 'magit-blame)

(global-set-key (kbd "C-=") 'er/expand-region)

;; Highlight git lines change
;;(global-git-gutter-mode +1)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;helm
;; ;;;;;;;;;;;;;;;;;;;;
;;(setq helm-dash-min-length 3)
;;helm-dash
;;(defun dash-install-dset (docset)
;;  (unless (file-exists-p (
;;(setq helm-dash-docsets-path (format "%s/.emacs.d/docsets" (getenv "HOME")))
;;(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq helm-candidate-number-limit 100)

;; From https://gist.github.com/antifuchs/9238468
(setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
      helm-input-idle-delay 0.01  ; this actually updates things reeeelatively quickly.
      helm-yas-display-key-on-candidate t
      helm-quick-update t
      helm-M-x-requires-pattern nil
      helm-ff-skip-boring-files t
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-display-header-line nil
      )

(global-set-key(kbd "C-c h") 'helm-mini)
(global-set-key(kbd "C-h a") 'helm-apropos)
(global-set-key(kbd "C-x C-b") 'helm-buffers-list)
(global-set-key(kbd "C-x b") 'helm-mini)
(global-set-key(kbd "M-y") 'helm-show-kill-ring)
(global-set-key(kbd "M-x") 'helm-M-x)
(global-set-key(kbd "C-x C-f") 'helm-find-files)

;(global-set-key(kbd "C-x c o") 'helm-occur) ;; WHAT
;(global-set-key(kbd "C-x c s") 'helm-swoop) ;; WHAT
;(global-set-key(kbd "C-x c y") 'helm-yas-complete) ;; WHAT
;(global-set-key(kbd "C-x c Y") 'helm-yas-create-snippet-on-region) ;; WHAT
;(global-set-key(kbd "C-x c b") 'my/helm-do-grep-book-notes) ;; WHAT
;(global-set-key(kbd "C-x c SPC") 'helm-all-mark-rings) ;; WHAT
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally
(helm-mode 1)
(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 30)
(setq helm-autoresize-min-height 30)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal

;;projectile
;; ;;;;;;;;;;;;;;;;;;;;
;; extra prefix for projectile
;;(define-key map (kbd "s-p") 'projectile-command-map)

;;helm-projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)


;; Completion hooks
;; ;;;;;;;;;;;;;;;;;;;;
(add-hook          'c-mode-hook 'company-mode)
(add-hook        'c++-mode-hook 'company-mode)
(add-hook       'objc-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook      'cmake-mode-hook 'company-mode)
(add-hook      'scala-mode-hook 'company-mode)
(add-hook       'html-mode-hook 'company-mode)
(add-hook        'qml-mode-hook 'company-mode)
(add-hook        'js2-mode-hook 'company-mode)
