;; smart-mode-line
;; ;;;;;;;;;;;;;;;;;;;;
(setq sml/no-confirm-load-theme t)
(sml/setup)

;; neotree
;; ;;;;;;;;;;;;;;;;;;;;
(global-set-key [f8] 'neotree-toggle)

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

;; magit
;; ;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c v s") 'magit-status)
(global-set-key (kbd "C-c v l") 'magit-log)
(global-set-key (kbd "C-c v v") 'magit-blame)
(global-set-key (kbd "C-c v p") 'magit-pull)
(global-set-key (kbd "C-c v b") 'magit-branch-popup)


(global-set-key (kbd "C-=") 'er/expand-region)

;; Highlight git lines change
(global-git-gutter-mode +1)

;; ediff - don't start another frame
;; ;;;;;;;;;;;;;;;;;;;;
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


;; conda
;; ;;;;;;;;;;;;;;;;;;;;
(require 'conda)
(conda-env-initialize-interactive-shells)
(custom-set-variables
 '(conda-anaconda-home "/home/trcabel/miniconda3/"))


;;helm
;; ;;;;;;;;;;;;;;;;;;;;
(setq helm-candidate-number-limit 100)

(setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf))

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


;; find-other-file
;; ;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c a") 'ff-find-other-file)

;; eglot
;; ;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(global-set-key(kbd "C-c l r") 'eglot-rename)

(setq company-transformers nil)

;; ccls
;; ;;;;;;;;;;;;;;;;;;;

;;(require 'ccls)
;;(add-to-list 'eglot-server-programs
 ;;            '((c++ mode c-mode) . ("/home/trcabel/Tools/ccls/build/Release/ccls")))
;(setq ccls-executable "/home/trcabel/Tools/ccls/build/Release/ccls")
;;setq(ccls-initialization-options "--init='{\"compilationDatabaseDirectory\":\"build\"}'")

(defun projectile-project-find-function (dir)
  (let* ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'projectile-project-find-function))

;;--init='{"compilationDatabaseCommand":"/tmp/c/x"}'

;;projectile
;; ;;;;;;;;;;;;;;;;;;;;
;; extra prefix for projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(projectile-register-project-type 'cmake '("CMakeLists.txt")
                                  :compilation-dir "build"
                                  :configure "cmake %s"
                                  :compile "make -j"
                                  :test "make test")

;;helm-projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(global-set-key(kbd "C-c h") 'helm-projectile)

;; Completion hooks
;; ;;;;;;;;;;;;;;;;;;;;

(require 'company)
(global-company-mode t)

(add-hook 'after-init-hook 'global-company-mode)

(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 1)

(setq company-dabbrev-downcase nil)

(add-hook 'c++-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '((
                                                            company-capf
                                                            company-files
                                                            )))))

(add-hook 'emacs-lisp-mode-hook  'company-mode)
(add-hook      'cmake-mode-hook  'company-mode)
(add-hook       'html-mode-hook  'company-mode)
(add-hook        'qml-mode-hook  'company-mode)
(add-hook        'js2-mode-hook  'company-mode)

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
