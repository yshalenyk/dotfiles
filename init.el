;;; CODEj:

;;; import a package
(require 'package)
;;; requred packages
(setq package-list '(evil
					 evil-leader
					 evil-org
					 rust-mode
					 xclip
					 leuven-theme
					 gruvbox-theme
					 yasnippet
					 solarized-theme
					 company
					 company-jedi
					 yafolding
					 flycheck
					 ido-vertical-mode
					 flx-ido
					 smex
					 go-mode
					 company-go
					 ido-hacks
					 powerline
					 powerline-evil
					 noctilux-theme
					 git-gutter
					 ace-jump-mode
					 ))

;;; list the repositories containing them
;;(add-to-list 'load-path "~/.emacs.d/custom"
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;;(add-to-list 'load-path "~/.emacs.d/evil/")
;;; activate all the packages (in particular autoloads)
(package-initialize)

;;; fetch the list of packages available 
(unless package-archive-contents
    (package-refresh-contents))

;;;;; install the missing packages
(dolist (package package-list)
    (unless (package-installed-p package)
          (package-install package)))

(xclip-mode 1)
;;; yafolding activation
(require 'yafolding)

;;; evil-org
(require 'evil-org)
;;
;;;;; YASnippets 
(require 'yasnippet)
(yas-global-mode t)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
;;
;;
;;;;; vim emulation emable
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(require 'evil)
(evil-mode t)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-q") 'evil-window-delete)

(evil-leader/set-key "v" 'split-window-right)
(evil-leader/set-key "s" 'split-window-below)

(add-hook 'ibuffer-mode-hook (lambda () (setq-local display-buffer-base-action '(display-buffer-at-bottom))))

;; git integrations
(require 'git-gutter)
(global-git-gutter-mode t)


;;;;; interactive do things
(require 'ido)
(require 'ido-hacks)
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



;;;;; apperiance settings
(load-theme 'gruvbox t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-frame-font "Source Code Pro-10")
(setq inhibit-startup-message t)
(require 'powerline)
(powerline-default-theme)
(global-hl-line-mode 1)


(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;;;;additional editing configuration
(electric-pair-mode 1)
(show-paren-mode 1)
(global-linum-mode 1)
(setq linum-format "%4d \u2502 ")
(delete-selection-mode 1)

(setq-default indent-tabs-mode -1)
(setq-default tab-width 4)

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
(defun for-prog/go-mode-hook ()
  (add-to-list 'company-mode-backends 'company-go))
(defun for-prog/checking ()
  (global-flycheck-mode))

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-select-next)
     (define-key company-active-map [tab] 'company-select-next)))
;;;;(defun for-prog/c-mode-heeol ()
;;;;  (add-to-list 'company-backends 'company-irony))
;;
;;;;; mappings
;;(evil-leader/set-key "a" 'duplicate-line)
(evil-leader/set-key "b" 'ido-switch-buffer)
(evil-leader/set-key "f" 'ido-find-file)
(evil-leader/set-key "t" 'undo-tree-visualize)
(evil-leader/set-key "k" 'kill-buffer)
(evil-leader/set-key "x" 'smex-major-mode-commands)
(evil-leader/set-key "d" 'dired)
(evil-leader/set-key "j" 'ace-jump-mode)
(evil-leader/set-key "w" 'save-buffer)
;;
(define-key evil-normal-state-map (kbd "zf") 'yafolding-toggle-element)
;;
;;
;;
;;;; hooks
;;
(add-hook 'after-init-hook 'global-company-mode)
;;;;(add-hook 'python-mode-hook 'anaconda-mode)
;;(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'python-mode-hook 'for-prog/python-mode-hook)
;;(add-hook 'python-mode-hook 'jedi:setup)
;;(setq jedi:setup-keys t)
;;(setq jedi:complete-on-dot t)
(add-hook 'after-init-hook 'for-prog/checking)
;;(add-hook 'org-mode-hook 'evil-org-mode)

(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
;;
;;

(load-library "hideshow")
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (toggle-selective-display column)))
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(define-key evil-normal-state-map (kbd "SPC") 'toggle-hiding)

;;(add-hook 'prog-mode-hook 'yafolding-mode)
