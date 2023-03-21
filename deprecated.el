;; to try again later

;; lsp-bridge
;; https://github.com/lambdadog/.emacs.d/blob/6a9e23eb9e209b0d82cd5b60a9e9153fad8385fb/init.el#L330-L391

;; (lsp-bridge-restart-process)) -> after conda init
;; (use-package lsp-bridge
;;   :commands lsp-bridge-mode
;;   :load-path "/home/trcabel/Tools/lsp-bridge"
;;   :ensure nil
;;   :init
;;   (use-package posframe)
;;   :bind
;;   (:map lsp-bridge-mode-map
;;         ("M-." . lsp-bridge-find-def)
;;         ("M-," . lsp-bridge-return-from-def)
;;         ("M-?" . lsp-bridge-find-references)
;;         ("M-i" . lsp-bridge-lookup-documentation)
;;         ("M-n" . lsp-bridge-popup-documentation-scroll-up)
;;         ("M-p" . lsp-bridge-popup-documentation-scroll-down)
;;         ("s-C-n" . lsp-bridge-jump-to-next-diagnostic)
;;         ("s-C-p" . lsp-bridge-jump-to-prev-diagnostic))
;;   :hook (after-init . global-lsp-bridge-mode)
;;   :config
;;   (setq lsp-bridge-python-lsp-server "jedi")
;;   (setq lsp-bridge-c-lsp-server "clangd-13")
;;   ;(setq lsp-bridge-auto-format-code-idle 5)
;;   ;(setq lsp-bridge-enable-auto-format-code t)
;;   ;(setq lsp-bridge-enable-log nil)
;;   ;(setq lsp-bridge-enable-signature-help t)
;;   )

;;(require 'lsp-bridge)
;;(global-lsp-bridge-mode)

;;(setq lsp-bridge-python-lsp-server 'jedi)






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
