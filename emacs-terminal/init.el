;;package
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

;; Dasbhoard + Theming
(use-package dashboard
  :defer nil
  :preface
  (defun my/dashboard-banner ()
  :config
  (dashboard-setup-startup-hook))
  (setq dashboard-items '((agenda . 10)
			  (recents . 10)))
  (setq dashboard-banner-logo-title "No Title")
  (setq dashboard-center-content nil)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-set-init-info t)
  (setq dashboard-init-info (format "%d packages loaded in %s"
                                    (length package-activated-list) (emacs-init-time)))
  (setq dashboard-set-footer nil)
  (setq dashboard-set-navigator t)
  (setq dashboard-week-agenda t)
  :hook ((after-init     . dashboard-refresh-buffer)
         (dashboard-mode . my/dashboard-banner)))
  (setq global-page-break-lines-mode t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (global-set-key "\C-ck" 'dashboard-refresh-buffer)

;; ivy + counsel + which key /Search and Completion
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
(setq counsel-describe-function-function #'helpful-callable) ;; for helpfpul
(setq counsel-describe-variable-function #'helpful-variable)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel)

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode 1))

(use-package helpful)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

;;Company = Autocomplete
(use-package company
  :hook (prog-mode . company-mode)
  :hook (shell-mode . company-mode)
  :hook (eshell-mode . company-mode)
  :custom
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; evil 
(use-package evil
  :init
  (evil-mode 1))
;; Might add evil-everywhere

;; org-mode 
(use-package org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-ct" 'org-todo)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cw" 'org-refile)
(global-set-key "\C-cr" 'org-tag-view)

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  
           "PROJ(p)" 
           "LOOP(r)"
           "STRT(s)"
           "WAIT(w)"
           "HOLD(h)"
           "IDEA(i)"
           "|"
           "DONE(d)" 
           "KILL(k)")))

(setq org-todo-keyword-faces
      '(("TODO" . "green") ("STRT" . "yellow")
	("PROJ" . "red") ("LOOP" . "orange") ("WAIT" . "white")
        ("HOLD" . "blue" ) ("IDEA" . "purple")))

(setq org-hide-leading-stars t)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;;Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package clean-buffers 
  :custom
  ((clean-buffers-useless-buffer-time-out (* 24 3600))
   (clean-buffers-useless-buffer-names
    (list "*Buffer List*" "*Backtrace*" "*Apropos*" "*Completions*"
          "*Help*" "\\.~master~" "\\*vc-dir\\*" "\\*tramp/.+\\*"
          "\\*vc-git.+\\*" "\\*helm.+\\*")))
  :config
  (clean-buffers-turn-on-auto-clean-buffers))

(setq custom-file (concat user-emacs-directory "custom.el"))

;;exception number
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		eww-mode-hook))
  (add-hook mode(lambda ()
		  (display-line-numbers-mode 0))))

