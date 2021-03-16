(setq startup/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))
(add-hook 'emacs-startup-hook 'startup/reset-gc)

(setq inhibit-startup-message t)
(scroll-bar-mode -1)       
(tool-bar-mode -1)          
(tooltip-mode -1)           
(menu-bar-mode 1)            
(setq visible-bell t)       
(set-face-attribute 'default nil :height 110) 

(add-to-list 'exec-path "~/.emacs.d/sqlite")
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)