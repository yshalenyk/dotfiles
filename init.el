;;; init.el --- Personal configuration
;;; Commentary:
;;; Code:

(defvar custom-file-path "~/.emacs.d/custom.el" )
;;; disable init.el modifying by custom system
(if (not (file-exists-p custom-file-path))
    (write-region "" "" custom-file-path))

(setq custom-file custom-file-path)
(load custom-file)

;;; Import a package
(require 'package)

;;; Plugins list
(setq package-list '(evil
                     evil-leader
                     evil-org
                     evil-surround
                     xclip
                     helm
                     rust-mode
                     gruvbox-theme
                     smart-mode-line
                     company
;;                     company-jedi
		     company-anaconda
                     company-racer
		     company-quickhelp
                     yafolding
                     yasnippet
                     flycheck
                     git-gutter
                     neotree
                     all-the-icons
                     rainbow-delimiters

                    ))

;;; list the repositories containing them
;;(add-to-list 'load-path "~/.emacs.d/custom"
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;;; activate all the packages (in particular autoloads)
(package-initialize)

;;; fetch the list of packages available
(unless package-archive-contents
    (package-refresh-contents))

;;; install the missing packages
(dolist (package package-list)
    (unless (package-installed-p package)
	  (package-install package)))

;;; helm configuration
(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

;;; enables copy and paste in terminal
(xclip-mode 1)

;;; yafolding activation
(require 'yafolding)
(yafolding-mode t)

;;; Vim emulation
(require 'evil-surround)
(global-evil-surround-mode 1)
;;; evil-org
(require 'evil-org)

;;; Vim leader
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

;;; Enable plugin
(require 'evil)
(evil-mode t)

;;; Splits
(evil-leader/set-key "v" 'split-window-right)
(evil-leader/set-key "s" 'split-window-below)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-q") 'evil-window-delete)

(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)

(define-key evil-normal-state-map (kbd "C-P") 'helm-M-x)

;; git highlightings
(require 'git-gutter)
(global-git-gutter-mode t)


;;;;; Apperiance settings
(load-theme 'gruvbox t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-frame-font "Source Code Pro-12")
(setq inhibit-startup-message t)
(setq company-selection-wrap-around t)
(sml/setup)
(setq sml/theme 'respectful)
(global-hl-line-mode 1)
(setq-default truncate-lines t)
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Backups placing
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups


;;; additional editing configuration
(electric-pair-mode 1)
(show-paren-mode 1)
(global-linum-mode 1)
(setq linum-format "%4d \u2502 ") ;;; for terminal
(delete-selection-mode 1)
(setq-default indent-tabs-mode -1)

;;; pretty lisp
(require 'rainbow-delimiters)


;;; syntax checking
(eval-after-load 'flycheck
  '(progn
	 (global-flycheck-mode)))

;;; autocomplete
(setq company-idle-delay 0)
(setq-default c-basic-offset 4)
(setq-default company-dabbrev-downcase nil)


(eval-after-load 'company
  '(progn
     (define-key company-active-map [tab] 'company-select-next)
     (define-key company-active-map (kbd "TAB") 'company-select-next)
     (add-to-list 'company-backends 'company-racer)
     (add-to-list 'company-backends 'company-anaconda)
     (company-quickhelp-mode 1)
     ))

;;;;; mappings
(evil-leader/set-key "t" 'undo-tree-visualize)
(evil-leader/set-key "k" 'kill-buffer)
(evil-leader/set-key "d" 'neotree)

;;; Code: folding
(define-key evil-normal-state-map (kbd "SPC") 'yafolding-toggle-element)



;;; Code: Hooks
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'org-mode-hook 'evil-org-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
;;; Code: templates
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-'") 'yas-expand)

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(require 'all-the-icons)
;;; init.el ends here
