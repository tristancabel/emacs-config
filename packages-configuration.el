
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
(global-set-key (kbd "C-c v s") 'magit-status)
(global-set-key (kbd "C-c v l") 'magit-log)
(global-set-key (kbd "C-c v v") 'magit-blame)
(global-set-key (kbd "C-c v p") 'magit-pull)
(global-set-key (kbd "C-c v b") 'magit-branch-popup)


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

(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))


;; flycheck
;; ;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook #'global-flycheck-mode)


;;projectile
;; ;;;;;;;;;;;;;;;;;;;;
;; extra prefix for projectile
;;(define-key map (kbd "s-p") 'projectile-command-map)

;;helm-projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(global-set-key(kbd "C-c p h") 'helm-projectile)

;; Completion hooks
;; ;;;;;;;;;;;;;;;;;;;;

(require 'company)
(global-company-mode t)

(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)

;; set default `company-backends'
(setq company-backends
      '((company-files          ; files & directory
         company-keywords       ; keywords
         company-capf
         company-yasnippet
         company-semantic
         )
        (company-abbrev company-dabbrev)
        ))

;; python: company-anaconda
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-anaconda)))

;; javascript
(dolist (hook '(js-mode-hook
                js2-mode-hook
                js3-mode-hook
                inferior-js-mode-hook
                ))
  (add-hook hook
            (lambda ()
              (tern-mode t)
              (add-to-list (make-local-variable 'company-backends)
                           'company-tern)
              )))


;; web
;(add-hook 'python-mode-hook
;          (lambda ()
;            (add-to-list (make-local-variable 'company-backends)
;                         'company-web)))

;; c 
(dolist (hook '(c-mode-hook
                c++-mode-hook
                cmake-mode-hook
                ))
  (add-hook hook
            (lambda ()
              (tern-mode t)
              (add-to-list (make-local-variable 'company-backends)
                           'company-c)
;              (add-to-list (make-local-variable 'company-backends)
;                           'company-c++)
              )))

;; company for java 
;;(require ‘company-emacs-eclim)
;;(company-emacs-eclim-setup)
;;(setq company-emacs-eclim-ignore-case t)

;(add-hook      'scala-mode-hook 'company-mode)

;; company colors
 (require 'color)
  
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

(company-quickhelp-mode 1)
