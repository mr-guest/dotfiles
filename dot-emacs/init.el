;; Package --- Summary.

(set-face-attribute 'default nil :font "Consolas" :height 110) 

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
			  (recents . 10)
			  (projects . 5)))
  (setq dashboard-banner-logo-title "No Title")
  (setq dashboard-startup-banner "~/.emacs.d/banner1.jpeg")
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

(use-package doom-themes
  :demand
  :defer nil
  :config
  (load-theme 'doom-city-lights t)
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))
      (setq doom-modeline-modal-icon t)
      (setq doom-modeline-buffer-file-name-style 'auto)
      (setq doom-modeline-workspace-name t)
      (setq doom-modeline-env-version t)

;;Magit
(use-package magit
  :defer t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Projectile
(use-package projectile)
:init
(projectile-mode +1)

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

(use-package dash)

;minimap
(use-package minimap
  :init
  (minimap-mode 1))  
(setq
 minimap-window-location 'right 
 minimap-width-fraction 0.0 
 minimap-minimum-width 15 
 minimap-dedicated-window t 
 minimap-enlarge-certain-faces nil)

;; Snippets
(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets)

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

(setq org-agenda-files '("~/Document/org/"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
				(file+headline "~/Document/org/gtd.org" "Tasks")
				"* TODO %i%? :TODO:")
			      ("T" "Tickler" entry
                               (file+headline "~/Document/org/tickler.org" "tickler")
                               "* %i%? \n %U")))

(setq org-hide-leading-stars t)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

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

;; org-brain?
(use-package org-brain
  :init
  (setq org-brain-path "~/Document/org/org-brain")
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  :config
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t))
;  (setq org-id-locations-file "~/Document/org/org-id")
;    (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
;  (push '("b" "Brain" plain (function org-brain-goto-end)
;          "* %i%?" :empty-lines 1)
;        org-capture-templates)
;  (setq org-brain-visualize-default-choices 'all)
;  (setq org-brain-title-max-length 12)
;  (setq org-brain-include-file-entries nil
;        org-brain-file-entries-use-title nil))

(use-package polymode
  :config
  (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode))

;;Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;pdf-tools
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode))

(add-hook 'pdf-view-mode-hook
  (lambda ()
    (set (make-local-variable 'evil-emacs-state-cursor) (list nil))))

(use-package clean-buffers 
  :custom
  ((clean-buffers-useless-buffer-time-out (* 24 3600))
   (clean-buffers-useless-buffer-names
    (list "*Buffer List*" "*Backtrace*" "*Apropos*" "*Completions*"
          "*Help*" "\\.~master~" "\\*vc-dir\\*" "\\*tramp/.+\\*"
          "\\*vc-git.+\\*" "\\*helm.+\\*")))
  :config
  (clean-buffers-turn-on-auto-clean-buffers))

;;lsp-mode

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred) 
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c C-l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy
:after lsp)

(use-package dap-mode
  :config
  (require 'dap-node)
  (dap-node-setup)
   (setq lsp-keymap-prefix "C-c C-d"))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))
(setq python-shell-interpreter "python3")
(setq dap-python-executeable "python3")

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

(setq custom-file (concat user-emacs-directory "custom.el"))

;;exception number
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		eww-mode-hook))
  (add-hook mode(lambda ()
		  (display-line-numbers-mode 0))))


					;end of init.el
