(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                        ("org" . "https://orgmode.org/elpa/")
						("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)          

;; Dasbhoard
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

(use-package doom-themes
  :demand
  :config
  (load-theme 'doom-one t))


(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

;; ivy 
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

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel)

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode 1))

(use-package minimap
  :init
  (minimap-mode 1))

;; Snippets
(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets)

;; Evil 
(use-package evil
  :init
  (evil-mode 1))

;; Enable copypasting outside of Emacs
(setq x-select-enable-clipboard t)

;; org-mode 
(use-package org)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

;; org-roam
(use-package org-roam
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/Document/org/")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

(setq org-roam-db-update-method 'immediate)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode(lambda ()(display-line-numbers-mode 0))))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq custom-file (concat user-emacs-directory "custom.el"))
;end of init.el

