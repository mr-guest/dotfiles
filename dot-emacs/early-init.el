(setq startup/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))
(add-hook 'emacs-startup-hook 'startup/reset-gc)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq inhibit-startup-message t)
(scroll-bar-mode -1)       
(tool-bar-mode -1)          
(tooltip-mode -1)           
(menu-bar-mode -1)            
(setq visible-bell t)       

(column-number-mode)
(global-display-line-numbers-mode t)
(setq x-select-enable-clipboard t)
(setq make-backup-files nil)
(auto-save-visited-mode)
(setq create-lock-files nil)
(setq org-roam-db-update-method 'immediate)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(setq recentf-exclude '(".*-autoloads\\.el\\'"))
(add-to-list 'exec-path "~/.emacs.d/sqlite")
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
