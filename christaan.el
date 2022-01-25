(require 'package)

(setq package-archives
      '(("mepla" . "http://melpa.org/packages/")
        ("org"       . "http://orgmode.org/elpa/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("gnu"       . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; Bootstrap 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq delete-old-versions -1)    ;; delete excess backup versions silently
(setq version-control t)    ; use version control
(setq vc-make-backup-files t)    ; make backups file even when in version controlled dir
(setq backup-directory-alist 
  '(("." . "~/.emacs.d/backups"))) ; which directory to put backups file
(setq vc-follow-symlinks t)               ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms 
  '((".*" "~/.emacs.d/auto-save-list/" t))) ;transform backups file name
(setq inhibit-startup-screen t)  ; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore)  ; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8)  ; use utf-8 by default
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)  ; sentence SHOULD end with only a point.
(setq fill-column 80)    ; toggle wrapping text at the 80th character

;; == [ GNU elpa GPG error work around ] ==
;; WARNIGN: This will disable signature check on install for any package
;;(setq package-check-signature nil)

(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)


(add-to-list 'load-path "~/.emacs.d/custom-packages/company-ctags")
(add-to-list 'load-path "~/.emacs.d/custom-packages/emacs-cquery")

(let ((file "~/.emacs.d/packages.el"))
  (when (file-exists-p file)
      (load-file file)))

(let ((file "~/.emacs.d/chef.el"))
  (when (file-exists-p file)
      (load-file file)))

(let ((file "~/.emacs.d/lass.el"))
  (when (file-exists-p file)
      (load-file file)))

(let ((file "~/.emacs.d/utils.el"))
  (when (file-exists-p file)
    (load-file file)))

(let ((file "~/.emacs.d/themes.el"))
  (when (file-exists-p file)
    (load-file file)))

(let ((file "~/.emacs.d/org.el"))
  (when (file-exists-p file)
    (load-file file)))


(org-babel-do-load-languages
'org-babel-load-languages '((shell . t)))

(load-config-from-file "keybindings.el")

(setq exec-path (append exec-path '("~/.bin" "~/tools/flutter/bin")))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-show-major-mode)
  (setq which-key-separator " | "))

(require 'company-ctags)
(require 'helm-config)
(helm-mode 1)

(use-package yasnippet :ensure t
  :config
  (yas-global-mode 1))

(eval-after-load 'company
  '(progn
     (company-ctags-auto-setup)))

;; == [ Company ] =================================================
(use-package company
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode 1)
  (global-set-key (kbd "C-<tab>") 'company-complete))

(use-package company-lsp
  :ensure t
  :requires company
  :config
  (push 'company-lsp company-backends)
  ;; Disable client side cache because the lsp does a beter job
  (setq company-transformers nil
	company-lsp-async t
	company-lsp-cache-candidates nil))

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
     (define-key company-active-map (kbd "<backtab>") 'company-select-previous)))

(use-package company-terraform
  :ensure t
  :config
  (company-terraform-init))

(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

(setq python-shell-interpreter "python3")

(use-package emmet-mode :ensure t)

;;;; == [ SML/NJ ] ============
(use-package sml-mode :ensure t)

;;;; == [ Helm ] ==============
(helm-autoresize-mode t)
(setq helm-autoresize-max-height 40)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)

(winner-mode 1)

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
;(require 'mu4e)

(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-maildir "/home/christiaan/Mail/Transip")
(setq mu4e-sent-folder "/Sent")
(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-trash-folder "/Trash")

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-center-content t)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-navigator t)
(setq show-week-agenda-p t)

(setq dashboard-items '((projects . 5)
                        (agenda . 10)))

(setq dashboard-org-agenda-categories '("gtd" "sprint"))

(use-package cquery :ensure t)
(setq cquery-executable "/usr/bin/cquery")
(use-package lsp-ui :ensure t)
(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c C-l")
  :hook ((python-mode . lsp)
	 (c++-mode . lsp)
	 (c-mode . lsp)
	 (js-mode . lsp)
         (javascript-ide . lsp)
	 (terraform-mode . lsp)
	 (elixir-mode . lsp)
	 (ruby-mode . lsp)
	 (dart-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :config  (setq lsp-prefer-flymake nil) ;; Prefer using lsp-ui (flycheck) over flymake.
  :commands lsp)
(setq lsp-dart-sdk-dir "/home/christiaan/tools/flutter/bin/cache/dart-sdk")

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(autoload 'enable-paredit-mode "paredit" t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'racket-mode #'enable-paredit-mode)

;;;; == [ JavaScript ] ========
(use-package js2-mode :ensure t)
(use-package js2-refactor :ensure t)
(use-package xref-js2 :ensure t)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook
	  (lambda ()
	    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(use-package indium :ensure t)
(add-hook 'js2-mode-hook #'indium-interaction-mode)


(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

(use-package jest
  :ensure t
  :after (js2-mode)
  :hook (js2-mode . jest-minor-mode))

(add-hook 'js2-mode-hook
  (lambda ()
    (local-umset-ket (kbd "C-c C-e"))))t

(use-package robe
  :ensure t
  :init
  (push 'company-robe company-backends))

(setenv "GOPATH" "/home/christiaan/go")

(defun my-go-mode-hook ()
  "Personal gomode hook."
  
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark))

(add-hook 'go-mode-hook 'my-go-mode-hook)

(use-package nasm-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))

(global-set-key [f3] 'neotree-toggle)
(global-set-key (kbd "M-o") 'ace-window)

(general-define-key
 "C-s" 'swiper
 "M-S-x" 'smex
 "M-x" 'helm-M-x
 "C->" 'mc/mark-next-like-this
 "C-<" 'mc/mark-previous-like-this
 "C-c C->" 'mc/mark-all-like-this
"C-c C-c" 'indium-eval-last-node
 "C-;" 'avy-goto-subword-1)

(general-define-key
 :prefix "C-c"
 "f"  '(:ignore t :which-key "files")
 "ff" 'helm-find-files
 "fr" 'helm-recentf
 "ft" 'neotree-toggle
 "o" '(:ignore t :which-key "org")
 "oa" 'org-agenda
 "p" 'projectile-command-map
 "." 'avy-goto-subword-0)

(general-define-key
 :prefix "C-x"
 "b" 'helm-buffers-list
 "C-b" 'helm-buffers-list)

(general-define-key
 "C-s" 'swiper
 "M-x" 'smex
 "M-S-x" 'helm-M-x
 "C->" 'mc/mark-next-like-this
 "C-<" 'mc/mark-previous-like-this
 "C-c C->" 'mc/mark-all-like-this)

(general-define-key
 :prefix "C-c"
 "f"  '(:ignore t :which-key "files")
 "ff" 'helm-find-files
 "fr" 'helm-recentf
 "ft" 'neotree-toggle)
