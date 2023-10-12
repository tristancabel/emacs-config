;;; package --- config
;;; Commentary:
;;; packages configurations
;;;

;;; Code:
;; smart-mode-line
;; smart-mode-line -> line model
;; ;;;;;;;;;;;;;;;;;;;;
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

;; undo-tree
;; undo and redo functions
;; ;;;;;;;;;;;;;;;;;;;;
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history nil))

;; whitespace-cleanup-mode
;; whitespace-cleanup-mode is a minor mode which calls whitespace-cleanup before saving the current buffer,
;; by default only if the whitespace in the buffer was initially clean. It determines this by quickly checking
;; to see if whitespace-cleanup would have any effect on the buffer.
;; ;;;;;;;;;;;;;;;;;;;;
(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (global-whitespace-cleanup-mode))


;; browse-kill-ring
;; to be able to browse kill-ring
;; ;;;;;;;;;;;;;;;;;;;;
(use-package browse-kill-ring
  :ensure t
  :bind (("M-y" . brosw-kill-ring)))

;; beacon
;; highlight cursor after window moves
;; ;;;;;;;;;;;;;;;;;;;;
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

;; which-key
;; displays the key bindings following your currently entered incomplete command
;; ;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; json-mode
;; ;;;;;;;;;;;;;;;;;;;;
(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

;; markdown-mode
;; ;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; find-other-file
;; ;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c a") 'ff-find-other-file)

;; ediff - don't start another frame
;; ;;;;;;;;;;;;;;;;;;;;
;(require 'ediff)
;(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; magit
;; ;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :bind (("C-c v s" . magit-status)
         ("C-c v l" . magit-log)
         ("C-c v v" . magit-blame)
         ("C-c v p" . magit-pull)
         ("C-c v b" . magit-branch-popup))
         )

;; Highlight git lines change
(use-package git-gutter-fringe
  :ensure t
  :config
  (global-git-gutter-mode))


;; icons for sidebar
;; icons
;; ;;;;;;;;;;;;;;;;;;;;
(use-package vscode-icon
  :ensure t
  :commands (vscode-icon-for-file))

;; dired-sidebar
;; sidebar with files arborescence
;; ;;;;;;;;;;;;;;;;;;;;
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "_")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

;; counsel/ivy/swipper
;; completion framework
;; ;;;;;;;;;;;;;;;;;;;;
(use-package counsel
  :ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package ivy :demand
             :diminish (ivy-mode)
             :bind (("C-x b" . ivy-switch-buffer))
             :config
             (setq ivy-use-virtual-buffers t
                   ivy-count-format "%d/%d "
                   ivy-display-style 'fancy))


(use-package swiper
  :ensure t
  :bind (("C-s" . swiper-isearch)
     ("C-r" . swiper-isearch)
     ("C-c C-r" . ivy-resume)
     ("M-x" . counsel-M-x)
     ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))


;; ;;projectile
;; project interaction library for Emacs.
;; ;; ;;;;;;;;;;;;;;;;;;;;
(use-package projectile
    :ensure t
    :bind (:map projectile-mode-map
                  ("s-p" . 'projectile-command-map)
                  ("C-c p" . 'projectile-command-map)
                )
    :config
    (setq projectile-completion-system 'ivy)
    (add-to-list 'projectile-globally-ignored-directories "build*")
    (add-to-list 'projectile-globally-ignored-directories ".cache")
    (projectile-mode 1)
    (defun projectile-project-find-function (dir)
      (let* ((root (projectile-project-root dir)))
        (and root (cons 'transient root))))
    (with-eval-after-load 'project
      (add-to-list 'project-find-functions 'projectile-project-find-function))
    (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                  :compilation-dir "build"
                                  :configure "cmake %s"
                                  :compile "make -j 6"
                                  :install "make -j 6 install"
                                  :test "make test")
)

;; conda
;; to work with conda environment
;; ;;;;;;;;;;;;;;;;;;;;
(defun my/conda_hook ()
  (let* ((env-name (conda--infer-env-from-buffer))
         (env-path (concat conda-env-home-directory "/envs/" env-name)))
    (setq-local lsp-pyright-venv-path env-path)
    (setq-local mode-line-process (concat "(" env-name ")"))
    (eglot-reconnect())
    (message "setting lsp-pyright-venv-path to %s" env-path)))

(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3"))
   :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode t)

  :hook
  ((conda-postactivate-hook . my/conda_hook)
   (conda-postdeactivate-hook . eglot-reconnect)))

(add-hook 'python-mode-hook '(lambda () (when (bound-and-true-p conda-project-env-path)
                                          (conda-env-activate-for-buffer)
                                          (eglot-reconnect())
                                          )))
;(add-to-hook 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path)
;                                          (conda-env-activate-for-buffer))))


;; flycheck
;; syntax checker
;; ;;;;;;;;;;;;;;;;;;;
;; (use-package flycheck
;;   :ensure t
;;   :init
;;   (global-flycheck-mode t)
;;   :config
;;   ;; Check only when saving or opening files. Newline & idle checks are a mote
;;   ;; excessive and can catch code in an incomplete state, producing false
;;   ;; positives, so we removed them.
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled idle-buffer-switch))
;;
;;   ;; For the above functionality, check syntax in a buffer that you switched to
;;   ;; only briefly. This allows "refreshing" the syntax check state for several
;;   ;; buffers quickly after e.g. changing a config file.
;;   (setq flycheck-buffer-switch-check-intermediate-buffers t)
;;
;;   ;; Display errors a little quicker (default is 0.9s)
;;   (setq flycheck-display-errors-delay 0.25)
;;   :custom
;;    (global-flycheck-modes
;;     '(not text-mode outline-mode fundamental-mode org-mode
;;           diff-mode shell-mode eshell-mode term-mode)))

;; company
;; Completion hooks
;; ;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode)
  ((c++-mode
    c-mode
    python-mode) . (lambda () (set (make-local-variable 'company-backends)
                            '((company-capf
                               company-files
                               company-dabbrev-code
                               company-qml
                               )))))
  :init
  (setq company-global-modes '(not gud-mode))
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-limit 10)
  (setq company-dabbrev-downcase nil) ; completion in case-sensitie mode
  (setq company-dabbrev-ignore-case nil)
  (setq company-show-numbers t)
  )

;; add icons to cmpany backends
(use-package company-box
  :ensure t
  :after company
  :delight
  :hook (company-mode . company-box-mode))

;; company colors
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))


;; cmake stuff
(use-package cmake-mode
  :ensure t
  :mode ("\\CMakeLists.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :ensure t
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

;; (use-package cmake-ide
;;   :ensure t
;;   :after projectile
;;   :hook (c++-mode . my/cmake-ide-find-project)
;;   :preface
;;   (defun my/cmake-ide-find-project ()
;;     "Finds the directory of the project for cmake-ide."
;;     (with-eval-after-load 'projectile
;;       (setq cmake-ide-project-dir (projectile-project-root))
;;       (setq cmake-ide-build-dir (concat cmake-ide-project-dir "build")))
;;     (setq cmake-ide-compile-command
;;           (concat "cd " cmake-ide-build-dir " && cmake .. && make -j"))
;;     (cmake-ide-load-db))

;;   (defun my/switch-to-compilation-window ()
;;     "Switches to the *compilation* buffer after compilation."
;;     (other-window 1))
;;   :bind ([remap comment-region] . cmake-ide-compile)
;;   :init (cmake-ide-setup)
;;   :config (advice-add 'cmake-ide-compile :after #'my/switch-to-compilation-window))

;; ;; flymake
;; ;; ;;;;;;;;;;;;;;;;;;;
;;(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
;;(setq flymake-start-on-flymake-mode nil)
(use-package flymake
  :ensure t
  :defer t)


;; Customize flymake to show errors on minibuffer instead of popups
(setq flymake-gui-warnings-enabled nil)
(setq flymake-start-syntax-check-on-newline nil)
(setq flymake-no-changes-timeout nil)
(setq flymake-proc-compilation-prevents-echo t)

;; Show flymake error on minibuffer when cursor is on a line with an error
;(defun my/flymake-show-error ()
;  (when (flymake-mode)
;    (let ((line-no (line-number-at-pos)))
;      (dolist (elem flymake-err-info)
;        (when (eq line-no (car elem))
;          (message "%s" (flymake-ler-text (cdr elem))))))))
;(add-hook 'post-command-hook #'my/flymake-show-error)


;; pyright  -- lsp for python
;; ;; ;;;;;;;;;;;;;;;;;;;

(use-package lsp-pyright
  :ensure t)

;; ;; eglot
;; ;; ;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :ensure t
  :after projectile
  :commands (eglot eglot-ensure)
  :hook ((rust-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (web-mode . eglot-ensure)       ; no linting
         (js-mode . eglot-ensure)
         (json-mode . eglot-ensure))      ; no linting)
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l h" . eglot-help-at-point)
              ("C-c l a" . eglot-code-actions)
              ("M-n"     . flymake-goto-next-error)
              ("M-p"     . flymake-goto-prev-error))
  :init
  :custom
  ;; Shutdown server after buffer kill
  (eglot-autoshutdown t)
  ;; Enable eglot in code external to project
  (eglot-extend-to-xref t)
  :config
  ;;(add-to-list 'eglot-server-programs '((c-mode c++-mode) "ccls"
  ;;                                      "-init={\"compilationDatabaseDirectory\":\"build\"}"))                                        ;  (company-transformers nil)
  (add-to-list 'eglot-server-programs '((c-mode c++-mode) "clangd-13"
                    "-j=4"
                    "--malloc-trim"
                    "--log=error"
                    "--background-index"
                    "--clang-tidy"
                    "--cross-file-rename"
                    "--completion-style=detailed"
                    "--pch-storage=memory"))
  ; Add server for web-mode
  ;(add-to-list 'eglot-server-programs
  ;             '(web-mode . ("vscode-html-language-server" "--stdio")))

  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  ;;(add-to-list 'eglot-server-programs
  ;;             '(python-mode . ("jedi-language-server"))))
)


;;(defun eglot-ccls-inheritance-hierarchy (&optional derived)
;;  "Show inheritance hierarchy for the thing at point.
;;If DERIVED is non-nil (interactively, with prefix argument), show
;;the children of class at point."
;;  (interactive "P")
;;  (if-let* ((res (jsonrpc-request
;;                  (eglot--current-server-or-lose)
;;                  :$ccls/inheritance
;;                  (append (eglot--TextDocumentPositionParams)
;;                          `(:derived ,(if derived t :json-false))
;;                          '(:levels 100) '(:hierarchy t))))
;;            (tree (list (cons 0 res))))
;;      (with-help-window "*ccls inheritance*"
;;        (with-current-buffer standard-output
;;          (while tree
;;            (pcase-let ((`(,depth . ,node) (pop tree)))
;;              (cl-destructuring-bind (&key uri range) (plist-get node :location)
;;                (insert (make-string depth ?\ ) (plist-get node :name) "\n")
;;                (make-text-button (+ (point-at-bol 0) depth) (point-at-eol 0)
;;                                  'action (lambda (_arg)
;;                                            (interactive)
;;                                            (find-file (eglot--uri-to-path uri))
;;                                            (goto-char (car (eglot--range-region range)))))
;;                (cl-loop for child across (plist-get node :children)
;;                         do (push (cons (1+ depth) child) tree)))))))
;;    (eglot--error "Hierarchy unavailable")))




(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))
