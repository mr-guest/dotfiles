;; Increases Garbage Collection During Startup
(setq startup/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))
(add-hook 'emacs-startup-hook 'startup/reset-gc)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
(setq visible-bell t)       ; Set up the visible bell
(set-face-attribute 'default nil :height 110) ; Changing font and height

  
;; Package Managing.
;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)          

;; Emacs Dasbhoard
(use-package dashboard
  :defer nil
  :preface
  :config
  (dashboard-setup-startup-hook))
  (setq dashboard-items '((recents . 7)))
  (setq dashboard-banner-logo-title "Personal Emacs / mr.guest")
  (setq dashboard-startup-banner "~/.emacs.d/kokoron.png")
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-set-init-info t)
  (setq dashboard-init-info (format "%d packages loaded in %s"
                                    (length package-activated-list) (emacs-init-time)))
  (setq dashboard-set-footer nil)
  (setq dashboard-set-navigator t)

;; doom theme
(use-package doom-themes
  :demand
  :config
  (load-theme 'doom-one t))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

 ;; Auto update package globally
(use-package auto-package-update
  :defer nil
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Using command-log
(use-package command-log-mode)

;; doom-modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

;; ivy - autocompletion
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; detailed command
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))
;; counsel the package u in
(use-package counsel)

;; Great complement with ivy might add projectile
(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode 1))

;; Minimap enabled
(use-package minimap
  :init
  (minimap-mode 1))

;; Enable Line Numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line number for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode(lambda ()(display-line-numbers-mode 0))))

;; Snippets for Snippets
(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets)

;; Evil Mode
(use-package evil
  :init
  (evil-mode 1))

;; Enable copypasting outside of Emacs
(setq x-select-enable-clipboard t)

;; Transform yes-or-no to y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Collection of org-mode config

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

;;end of init.el

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-mode t)
 '(minimap-window-location 'right)
 '(org-agenda-files '("~/Document/Seven.org"))
 '(package-selected-packages
   '(minimap all-the-icons-ivy-rich ivy-rich which-key ivy doom-modeline command-log-mode auto-package-update doom-themes dashboard use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
