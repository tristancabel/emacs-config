;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
(setq transient-mark-mode t)

(setq-default c-basic-offset 4)
(setq-default c++-basic-offset 4)

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

;; set custom backup directory  ~ files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; no more tool bar
(tool-bar-mode 0)

;; yes or no as y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; save desktop mode
(desktop-save-mode 1)

;; Font size
;; ;;;;;;;;;;;;;;;;;;;;
(setq text-scale-mode-step 1.1)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; go to previous window
;; ;;;;;;;;;;;;;;;;;;;;
(defun myprevious-window ()
    "go to previous window"
    (interactive)
    (other-window -1))

(global-set-key (kbd "C-x p") 'myprevious-window)

;; save disk space ?
;;(setq delete-old-versions -1)
;;(setq version-control t)
;;(setq vc-make-backup-files t)
;;(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
