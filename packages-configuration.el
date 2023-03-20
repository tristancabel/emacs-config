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
(use-package conda
  :ensure t
  :init
  (conda-env-initialize-interactive-shells)
  (setq conda-anaconda-home (expand-file-name "~/miniconda3")))
;  (setq conda-env-home-directory (expand-file-name "~/miniconda3")))

;; flycheck
;; syntax checker
;; ;;;;;;;;;;;;;;;;;;;
 (use-package flycheck
   :ensure t
   :init
   (global-flycheck-mode t)
   :config
   ;; Check only when saving or opening files. Newline & idle checks are a mote
   ;; excessive and can catch code in an incomplete state, producing false
   ;; positives, so we removed them.
   (setq flycheck-check-syntax-automatically '(save mode-enabled idle-buffer-switch))

   ;; For the above functionality, check syntax in a buffer that you switched to
   ;; only briefly. This allows "refreshing" the syntax check state for several
   ;; buffers quickly after e.g. changing a config file.
   (setq flycheck-buffer-switch-check-intermediate-buffers t)

   ;; Display errors a little quicker (default is 0.9s)
   (setq flycheck-display-errors-delay 0.25)
   :custom
    (global-flycheck-modes
     '(not text-mode outline-mode fundamental-mode org-mode
           diff-mode shell-mode eshell-mode term-mode)))

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
;                               ;; company-dabbrev-code
                               )))))
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
;(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
(setq flymake-start-on-flymake-mode nil)

;; ;; eglot
;; ;; ;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :ensure t
  :after projectile
  :commands (eglot eglot-ensure)
  :hook ((rust-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (python-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l h" . eglot-help-at-point)
              ("C-c l a" . eglot-code-actions)
              ("M-n"     . flymake-goto-next-error)
              ("M-p"     . flymake-goto-prev-error))
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
  ;;python-mode ("pyright-langserver" "--stdio"))
  :init
  (setq eglot-autoshutdown t))

(defun eglot-ccls-inheritance-hierarchy (&optional derived)
  "Show inheritance hierarchy for the thing at point.
If DERIVED is non-nil (interactively, with prefix argument), show
the children of class at point."
  (interactive "P")
  (if-let* ((res (jsonrpc-request
                  (eglot--current-server-or-lose)
                  :$ccls/inheritance
                  (append (eglot--TextDocumentPositionParams)
                          `(:derived ,(if derived t :json-false))
                          '(:levels 100) '(:hierarchy t))))
            (tree (list (cons 0 res))))
      (with-help-window "*ccls inheritance*"
        (with-current-buffer standard-output
          (while tree
            (pcase-let ((`(,depth . ,node) (pop tree)))
              (cl-destructuring-bind (&key uri range) (plist-get node :location)
                (insert (make-string depth ?\ ) (plist-get node :name) "\n")
                (make-text-button (+ (point-at-bol 0) depth) (point-at-eol 0)
                                  'action (lambda (_arg)
                                            (interactive)
                                            (find-file (eglot--uri-to-path uri))
                                            (goto-char (car (eglot--range-region range)))))
                (cl-loop for child across (plist-get node :children)
                         do (push (cons (1+ depth) child) tree)))))))
    (eglot--error "Hierarchy unavailable")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; lsp-mode
;; ;; language server protocol
;; ;; ;;;;;;;;;;;;;;;;;;;
;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp
;;   :custom
;;   ;; Auto-kill LSP server after last workspace buffer is killed.
;;   (lsp-keep-workspace-alive nil)
;;   (lsp-auto-guess-root nil)
;; ;  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
;;   (lsp-enable-snippet nil)
;;   (lsp-file-watch-ignored '("build" ".git" "build*" "doc" ".ccls-cache"))
;;   (lsp-flycheck-live-reporting t)
;;   :bind (:map lsp-mode-map ("C-c l" . lsp-format-buffer))
;;   :hook
;;   ((python-mode c-mode c++-mode) . lsp)
;;   )

;; ; TODO get correct include
;; ;; if you are ivy user
;; ;(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; ;; (setq lsp-keymap-prefix "C-c l")

;; ;; ;; lsp-ui
;; ;; ;; ;;;;;;;;;;;;;;;;;;;
;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode
;;   :requires lsp-mode flycheck
;;   :bind
;;     (:map lsp-mode-map
;;     ("C-c C-r" . lsp-ui-peek-find-references)
;;     ("C-c C-j" . lsp-ui-peek-find-definitions)
;;     ("C-c i"   . lsp-ui-peek-find-implementation)
;;     ("C-c m"   . lsp-ui-imenu)
;;     ("C-c s"   . lsp-ui-sideline-mode)
;;     ("C-c d"   . ladicle/toggle-lsp-ui-doc))
;; ;   :bind (:map lsp-ui-mode-map
;; ;              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;; ;              ([remap xref-find-references] . lsp-ui-peek-find-references)
;;                                         ;              ("C-c u" . lsp-ui-imenu))
;;     :custom
;;     (lsp-ui-doc-enable nil)
;;     (lsp-ui-doc-header t)
;;     (lsp-ui-sideline-enable nil)
;;     (lsp-ui-sideline-ignore-duplicate t)
;;     (lsp-ui-sideline-show-symbol t)
;;     (lsp-ui-sideline-show-hover t)
;;     (lsp-ui-sideline-show-diagnostics nil)
;;     (lsp-ui-sideline-show-code-actions t)
;;     (lsp-ui-peek-enable t)
;; ;  :config
;; ;  (setq lsp-ui-doc-enable t
;; ;        lsp-ui-doc-position 'top
;; ;        lsp-ui-sideline-show-diagnostics t
;; ;        lsp-ui-sideline-ignore-duplicate t
;; ;  ;      lsp-ui-sideline-enable nil
;; ;        ;lsp-ui-flycheck-enable t
;; ;        lsp-ui-flycheck-list-position 'right
;; ;        ;lsp-ui-flycheck-live-reporting t
;; ;        lsp-ui-peek-enable t
;; ;        lsp-ui-peek-list-width 60
;; ;        lsp-ui-peek-peek-height 25
;; ;        lsp-ui-peek-show-directory t)
;;                                         ;
;;   :hook ((lsp-mode-hook) . lsp-ui-mode))

;; ccls
;; lsp for c++
;; ;;;;;;;;;;;;;;;;;;;
;; (use-package ccls
;;   :ensure t
;;   :after projectile
;; ;  :hook ((c-mode c++-mode objc-mode cuda-mode) .
;; ;         (lambda () (require 'ccls) (lsp)))
;;   :custom
;;   (ccls-executable "/home/trcabel/Tools/ccls/Release/bin/ccls")
;;                                         ;  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
;; ;  (ccls-initialization-options "--init='{\"compilationDatabaseDirectory\":\"build\"}'")
;;   (projectile-project-root-files-top-down-recurring
;;    (append '("compile_commands.json" ".ccls")
;;            projectile-project-root-files-top-down-recurring)))
;;setq


; TODO look at https://github.com/MaskRay/Config/blob/master/home/.config/doom/modules/private/my-cc/autoload.el

;; (defun ccls/callee ()
;;   (interactive)
;;   (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
;; (defun ccls/caller ()
;;   (interactive)
;;   (lsp-ui-peek-find-custom "$ccls/call"))
;; (defun ccls/vars (kind)
;;   (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
;; (defun ccls/base (levels)
;;   (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
;; (defun ccls/derived (levels)
;;   (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
;; (defun ccls/member (kind)
;;   (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))

;; ;; The meaning of :role corresponds to https://github.com/maskray/ccls/blob/master/src/symbol.h

;; ;; References w/ Role::Address bit (e.g. variables explicitly being taken addresses)
;; (defun ccls/references-address ()
;;   (interactive)
;;   (lsp-ui-peek-find-custom "textDocument/references"
;;    (plist-put (lsp--text-document-position-params) :role 128)))

;; ;; References w/ Role::Dynamic bit (macro expansions)
;; (defun ccls/references-macro ()
;;   (interactive)
;;   (lsp-ui-peek-find-custom "textDocument/references"
;;    (plist-put (lsp--text-document-position-params) :role 64)))

;; ;; References w/o Role::Call bit (e.g. where functions are taken addresses)
;; (defun ccls/references-not-call ()
;;   (interactive)
;;   (lsp-ui-peek-find-custom "textDocument/references"
;;    (plist-put (lsp--text-document-position-params) :excludeRole 32)))

;; ;; References w/ Role::Read
;; (defun ccls/references-read ()
;;   (interactive)
;;   (lsp-ui-peek-find-custom "textDocument/references"
;;    (plist-put (lsp--text-document-position-params) :role 8)))

;; ;; References w/ Role::Write
;; (defun ccls/references-write ()
;;   (interactive)
;;   (lsp-ui-peek-find-custom "textDocument/references"
;;    (plist-put (lsp--text-document-position-params) :role 16)))

;; (require 'ccls)
;; ;;(add-to-list 'eglot-server-programs
;;  ;;            '((c++ mode c-mode) . ("/home/trcabel/Tools/ccls/build/Release/ccls")))
;;
