;;; init.el --- Personal configuration
;;; Commentary:
;;; Code:


;;; disable init.el modifying by custom system
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; Import a package
(require 'package)

;;; Plugins list
(setq package-list '(evil
                     evil-leader
                     evil-org
                     xclip
                     rust-mode
                     gruvbox-theme
                     powerline
                     powerline-evil
                     company
                     company-jedi
                     yafolding
                     flycheck
                     ido-vertical-mode
                     flx-ido
                     smex
                     git-gutter
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

;;; enables copy and paste in terminal
(xclip-mode 1)

;;; yafolding activation
(require 'yafolding)
(yafolding-mode t)


;;; Vim emulation
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

;; git highlightings
(require 'git-gutter)
(global-git-gutter-mode t)

;;;;; Interactive do things
(require 'ido)
(require 'ido-vertical-mode)
(require 'smex)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-vertical-mode 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)
(smex-initialize)
(setq smex-prompt-string ">> ")
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;;;; Apperiance settings
(load-theme 'gruvbox t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-frame-font "Source Code Pro-10")
(setq inhibit-startup-message t)
(require 'powerline)
(powerline-default-theme)
(global-hl-line-mode 1)

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
(setq-default tab-width 4)

;;; pretty lisp
(require 'rainbow-delimiters)


;;;;functions
;;(defun duplicate-line()
;;  (interactive)
;;  (move-beginning-of-line 1)
;;  (kill-line)
;;  (yank)
;;  (open-line 1)
;;  (next-line 1)
;;  (yank)
;;  )
;;
(defun for-prog/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(defun for-prog/checking ()
  (global-flycheck-mode))

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-select-next)
     (define-key company-active-map [tab] 'company-select-next)))

;;;;; mappings
;;(evil-leader/set-key "a" 'duplicate-line)
(evil-leader/set-key "b" 'ido-switch-buffer)
(evil-leader/set-key "f" 'ido-find-file)
(evil-leader/set-key "t" 'undo-tree-visualize)
(evil-leader/set-key "k" 'kill-buffer)
(evil-leader/set-key "x" 'smex-major-mode-commands)
(evil-leader/set-key "d" 'dired)

;;; folding
(define-key evil-normal-state-map (kbd "SPC") 'yafolding-toggle-element)


;;; Code: Hooks
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'python-mode-hook 'for-prog/python-mode-hook)
(add-hook 'after-init-hook 'for-prog/checking)
(add-hook 'org-mode-hook 'evil-org-mode)

(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
