(setq startup/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))
(add-hook 'emacs-startup-hook 'startup/reset-gc)
(setq inhibit-startup-message t)

(column-number-mode)
(global-display-line-numbers-mode t)
(setq x-select-enable-clipboard t)
(setq make-backup-files nil)
(auto-save-visited-mode)
(setq create-lock-files nil)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(setq recentf-exclude '(".*-autoloads\\.el\\'"))