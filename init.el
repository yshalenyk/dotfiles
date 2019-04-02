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
(set-frame-font "Noto Mono-12" t t)   ; set default font
(setq default-frame-alist '((font . "Noto Mono-12")))
;; (setq-default package-check-signature nil)


;;; Apperiance settings (minimal mode)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)


;;; additional editing configuration
(electric-pair-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(setq-default indent-tabs-mode -1)


;;; disable init.el modifying by custom system
(defvar custom-file-path "~/.emacs.d/custom.el" )
(if (not (file-exists-p custom-file-path))
    (write-region "" "" custom-file-path))
(setq custom-file custom-file-path)
(load custom-file)

;;; packaging

(require 'package)
(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up


;;; the following lines tell emacs where on the internet to look up
;;; for new packages.
(setq package-archives '(("org"       . "https://orgmode.org/elpa/")
                         ("gnu"       . "https://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")))
(package-initialize)

;;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package) ; guess what this one does too ?


;;; libs
(use-package dash
  :ensure t)
(use-package s
  :ensure t)
(use-package monitor
  :ensure t)


;;; Setup evil

;;; basic package
(use-package evil
  :ensure t
  :init (setq evil-want-keybinding nil)
  :config (evil-mode t))
(use-package org-evil
  :ensure t)
(use-package evil-leader
  :ensure t
  :config (progn
	    (evil-leader/set-leader "C-p")
	    (global-evil-leader-mode)))

;;; redefine keybindings
(use-package evil-collection
  :after evil
  :ensure t
  :custom (evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

;;; must have stuff from vim
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode 1))


;;; setup theme
;; (load-theme 'wombat)
;;; still can't choose propper theme for myself
;; (use-package gruvbox-theme
;;   :ensure t
;;   :config (load-theme 'gruvbox-light-soft t))
;; (use-package zeno-theme 
;;   :ensure t
;;   :config (load-theme 'zeno t))
(use-package material-theme
  :ensure t
  :config (load-theme 'material-light t))



;; (use-package busybee-theme
;;   :ensure t
;;   :config (load-theme 'busybee t))
;; (use-package gruvbox-theme
;;   :ensure t
;;   :config (load-theme 'gruvbox-light-soft t))
;; (use-package base16-theme
;;   :ensure t 
;;   :config
;;   (load-theme 'base16-zenburn t))
;; (use-package kaolin-themes
;;   :ensure t
;;   :config
;;   (load-theme 'kaolin-dark t))

;; (use-package spacemacs-theme
;;   :ensure t
;;   :defer t
;;   :init
;;   (load-theme 'spacemacs-dark t))
;; (use-package tao-theme
;;   :ensure t
;;   :defer t
;;   :init (load-theme 'tao-yang  t))
;; (use-package anti-zenburn-theme
;;   :ensure t
;;   :defer t
;;   :init (load-theme 'anti-zenburn t))
;; (use-package  brilliance-dull-theme
;;   :ensure t
;;   :defer t
;;   :init (load-theme 'brilliance-dull t))


;; (use-package gotham-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gotham t))

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
  :config (global-set-key (kbd "C-x o") 'ace-window))


;;; bottom line
(use-package smart-mode-line
  :ensure t
  :init (setq sml/no-confirm-load-theme t)
  :config (progn
	    (sml/setup)
	    (setq sml/theme 'respectful)))

;;; line numbers
(global-linum-mode)
;; (use-package nlinum 
;;   :ensure t
;;   :config (global-nlinum-mode))


;;; more colors
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;; syntax checking
(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))


;;; additional languages
(use-package rust-mode
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package terraform-mode
  :ensure t)
(use-package nix-mode
  :ensure t)


;;; autocomplete
(use-package company
  :ensure t
  :init (progn (setq company-idle-delay 0)
	       (setq-default company-dabbrev-downcase nil))
  :config (global-company-mode))

;;; tooltips
(use-package company-quickhelp
  :ensure t
  :after company
  :config (company-quickhelp-mode 1))

;;; language server protocol
(use-package lsp-mode
  :commands lsp
  :init (setq-default lsp-response-timeout 25)
  :config (lsp-mode t))

;; (use-package lsp-ui
;;   :ensure t
;;   :after (lsp-mode)
;;   :config (lsp-ui-mode))
;; (use-package company-lsp :commands company-lsp)
;; (use-package lsp-mode
;;   :ensure t
;;   :init 
;;   :config (lsp-mode t))

(use-package company-lsp
  :ensure t
  :after (company lps-mode)
  :config (add-to-list 'company-backends 'company-lsp))


;;; copy mode for command line
(use-package xclip
  :ensure t
  :config (xclip-mode 1))


;;; better code folding
(use-package yafolding
  :ensure t
  :config (progn
	    (define-key evil-normal-state-map (kbd "SPC") 'yafolding-toggle-element)))

;;; fuzzy finder
(use-package helm
  :ensure t
  :init (progn
	  (global-set-key (kbd "M-x") 'helm-M-x)
	  (global-set-key (kbd "C-x C-f") 'helm-find-files)
	  (global-set-key (kbd "C-x C-b") 'helm-mini)
	  (setq-default helm-M-x-fuzzy-match t)
	  (require 'helm-config)
	  (evil-leader/set-key
	    "b" 'helm-buffers-list
	    "y" 'helm-show-kill-ring
	    "r" 'helm-register
	    "o" 'helm-occur
	    "T" 'helm-top
	    "f" 'helm-find-files))
  :config (progn
	    (helm-mode)
	    (helm-autoresize-mode t)))

(use-package projectile
  :ensure t
  :config (projectile-mode t))

(defun do-grep()
  "Perform rg search on project."
  (interactive)
  (helm-projectile-rg))

(use-package helm-projectile
  :ensure t
  :after projectile
  :config (progn
	    (use-package helm-rg
	      :ensure t)
	    (evil-leader/set-key
	      "s" #'do-grep)
	    (evil-leader/set-key
	      "d" 'projectile-dired)
	    (evil-leader/set-key
	      "i" 'imenu)
	    (evil-leader/set-key
	      "C-p" 'helm-projectile-switch-project)
	    (evil-leader/set-key
	      "p" 'helm-projectile-find-file)
	    (evil-leader/set-key
	      "h" 'helm-apropos)
	    (helm-projectile-on)))

;;; git frontend
(use-package magit
  :ensure t)
(use-package evil-magit
  :ensure t
  :after magit
  :config (evil-leader/set-key
	    "m" 'magit))

(set-frame-parameter (selected-frame) 'alpha '(90 . 30))
(add-to-list 'default-frame-alist '(alpha . (90 . 30)))
;;; TODO: dap-mode
(provide 'init)
;;; init.el ends here
