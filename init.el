;;; init.el --- Personal configuration
;;; Commentary:
;;; Code:

;;; initial configuration
(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
;; (setq-default default-fill-column 80)	; toggle wrapping text at the 80th character
(setq initial-scratch-message "Welcome in Emacs") ; print a default message in the empty scratch buffer opened at startup
(defalias 'yes-or-no-p 'y-or-n-p)       ; must have
(setq-default truncate-lines t)         ; straight lines
(global-hl-line-mode 1)                 ; highlight current line
;; (setq default-frame-alist '((font . "Source Code Pro-12")))
;; (set-frame-font "Source Code Pro-12")   ; set default font
;; (set-frame-font "Noto Mono-10.5" t t)   ; set default font
;; (setq default-frame-alist '((font . "Noto Mono-10.5")))
(set-frame-font "Source Code Pro Medium-11" t t)   ; set default font
(setq default-frame-alist '((font . "Source Code Pro Medium-11")))

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
;; (setq-default package-check-signature nil)


;;; Apperiance settings (minimal mode)
;;(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)


;;; additional editing configuration
(electric-pair-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(setq-default indent-tabs-mode -1)
(setq-default tramp-default-method "ssh")


;;; disable init.el modifying by custom system
(defvar custom-file-path "~/.emacs.d/custom.el" )
(if (not (file-exists-p custom-file-path))
    (write-region "" "" custom-file-path))
(setq custom-file custom-file-path)
(load custom-file)

;;; packaging
(setq-default gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ;; bug in emacs-26.2
(require 'package)
(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
(setq package-check-signature nil)   ; allow unsigned

;; use gpg2
(setq epg-gpg-program "gpg2")


;;; the following lines tell emacs where on the internet to look up
;;; for new packages.
(setq package-archives '(("org"       . "https://orgmode.org/elpa/")
                         ("gnu"       . "https://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")))
(package-initialize)

;; refresh
(when (not package-archive-contents)
  (package-refresh-contents))

;;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package) ; guess what this one does too ?


;;; auto update packages
(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))


;;; libs
(use-package dash
  :ensure t)
(use-package s
  :ensure t)
(use-package monitor
  :ensure t)


;;; setup theme

;; (use-package almost-mono-themes
;;   :ensure t
;;   :config (load-theme 'almost-mono-white t))

(use-package eziam-theme
    :defer t
    :init (load-theme 'eziam-light))

(use-package eziam-light-theme
    :ensure eziam-theme)
;; (use-package nord-theme
;;  :ensure t
;;  :config (load-theme 'nord t))

;; (use-package busybee-theme
;;   :ensure t
;;   :config (load-theme 'busybee t))

(use-package rich-minority
  :ensure t
  :config (rich-minority-mode t))

(use-package smart-mode-line
  :ensure t
  :init (setq sml/no-confirm-load-theme t) (setq sml/theme 'light)
  :config (sml/setup))

;; (use-package smart-mode-line-light-powerline-theme
;;   :config (setq sml/theme 'powerline-light))

;; (use-package gotham-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gotham t))


(use-package mode-icons
  :ensure t
  :config (mode-icons-mode))

;;; show key bindings in popup
(use-package which-key
  :ensure t
  :config (progn
	    (which-key-mode)
	    (which-key-setup-side-window-bottom)
	    (which-key-setup-minibuffer)))

;;; golden ratio
(use-package golden-ratio
  :ensure t
  :config (golden-ratio-mode t))


;;; quick jump between windows
(use-package ace-window
  :ensure t
  :config (global-set-key (kbd "C-c C-o") 'ace-window))


;;; more colors
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;; syntax checking
(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C--") 'er/contract-region))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-c C-c m") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(use-package smartparens
  :ensure t
  :config (require 'smartparens-config))

;;; additional languages
(use-package rust-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package nix-mode
  :ensure t)

(use-package nginx-mode
  :ensure t)

;;; autocomplete
(use-package company
  :ensure t
  :init (progn (setq company-idle-delay 0)
	       (setq company-dabbrev-downcase nil))
  :config (global-company-mode))


(use-package python-pytest
  :ensure t
  :bind (:map python-mode-map ("C-c t" . python-pytest-popup)))

;;; tooltips
(use-package company-quickhelp
  :ensure t
  :after company
  :config (company-quickhelp-mode 1))

;;; language server protocol
(use-package lsp-mode
  :ensure t
  :commands lsp
  :init (setq-default lsp-response-timeout 25)
  :hook (prog-mode . lsp)
  :config (lsp-mode t))

;; (use-package lsp-python
;;   :hook (python-mode . #'lsp-python-enble))

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :config (lsp-ui-mode))

(use-package company-lsp
  :ensure t
  :after (company lps-mode)
  :config (push 'company-lsp company-backends))

(use-package anaconda-mode
  :ensure t
  :config
  (use-package company-anaconda
    :ensure t
    :config (add-to-list 'company-backends 'company-anaconda))
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))

(use-package company-ansible
  :ensure t
  :after (company)
  :config (push 'company-ansible company-backends))

(use-package company-terraform
  :ensure t
  :after company
  :config (company-terraform-init))

;;; debugger
(use-package dap-mode
  :ensure t
  :config (progn
	    (dap-mode 1)
	    (require 'dap-python)))

(use-package dap-ui
  :after dap-mode
  :config (progn (tooltip-mode 1)
		 (dap-ui-mode 1)
		 (dap-tooltip-mode 1)))

(use-package dap-python
  :config (setq dap-python-executable "python3"))

;;; copy mode for command line
(use-package xclip
  :ensure t
  :config (xclip-mode 1))

;;; better code folding
(use-package yafolding
  :ensure t
  :bind ("C-c <C-return>" . yafolding-toggle-element)
  :hook (prog-mode . yafolding-mode))

(use-package helpful
  :ensure t)

;;; fuzzy finder
(use-package ivy
  :demand
  :config (progn
	  (ivy-mode t)
  	  (setq ivy-use-virtual-buffers t)
	  (setq ivy-height 15)
  	  (setq enable-recursive-minibuffers t)
	  (setq ivy-re-builders-alist
		'((swiper . ivy--regex-plus)
		  (t      . ivy--regex-fuzzy)))
  	  (global-set-key "\C-s" 'swiper)
  	  (global-set-key (kbd "C-c C-r") 'ivy-resume)
	  (global-set-key (kbd "C-c s") 'counsel-rg)
	  (global-set-key (kbd "C-c l") 'counsel-locate)
	  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
	  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  	  (global-set-key (kbd "<f2> l") 'counsel-find-library)
  	  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  	  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)))

(use-package counsel
  :after (ivy)
  :ensure t
  :config (progn
	    (setq counsel-describe-function-function #'helpful-callable)
	    (setq counsel-describe-variable-function #'helpful-variable)
	    (counsel-mode t)))

(use-package ivy-rich
  :ensure t
  :after (ivy counsel)
  :config (ivy-rich-mode t))

(use-package counsel-projectile
  :ensure t
  :after (ivy projectile)
  :config (progn
	    (global-set-key (kbd "C-c p p") 'counsel-projectile-switch-project)
	    (global-set-key (kbd "C-c p f") 'counsel-projectile-find-file)))

(use-package projectile
  :ensure t
  :config (progn (projectile-mode t)
		 (setq projectile-completion-system 'ivy)
		 (global-set-key (kbd "C-c p d") 'projectile-dired-other-window)
		 (global-set-key (kbd "C-c p s") 'projectile-run-eshell)))

;;; git frontend
(use-package magit
  :bind ("C-c g" . magit)
  :ensure t)


(use-package pass
  :ensure t
  :bind ("C-c C-u p" . pass))

(use-package ivy-pass
  :ensure t)

;; fun

(use-package md4rd
  :ensure t
  :config (add-hook 'md4rd-mode-hook 'md4rd-indent-all-the-lines))

(use-package twittering-mode
  :ensure t
  :config (progn (setq twittering-icon-mode t) (setq twittering-reverse-mode t) ))

(global-set-key (kbd "C-c C-b") 'ibuffer-other-window)

(use-package smart-comment
  :ensure t
  :bind ("M-;" . smart-comment))


(use-package pyenv-mode
  :ensure t
  :hook (python-mode . pyenv-mode)
  :config
  (defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))
  (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set))

(use-package pyenv-mode-auto
   :ensure t)
;;(set-frame-parameter (selected-frame) 'alpha '(90 . 30))
;;(add-to-list 'default-frame-alist '(alpha . (90 . 30)))
;;; TODO: dap-mode
(provide 'init)
;;; init.el ends here
