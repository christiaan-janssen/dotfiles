;; Initialize package sources
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(set-face-attribute 'default nil :font "Fira Code" :height 120)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode 1)

;; Disable linenumbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                treemacs-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                mrepl-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 120)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 120 :weight 'regular)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; (use-package general
;;   :config
;;   (general-create-definer efs/leader-keys
;;     :prefix "C-;")

(use-package general
  :after evil
  :config
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  
  (efs/leader-keys
    ";"  '(ace-window :which-key "ace-window")
    "b"  '(:ignore t :which-key "buffer")
    "bb" '(counsel-switch-buffer :which-key "load file")
    "bk" '(kill-this-buffer :which-key "kill this buffer")
    "c"  '(:ignore t :which-key "compile")
    "cc" '(eval-last-sexp :which-key "eval last sexp")
    "d"  '(:ignore t :which-key "desktop")
    "dc" '(desktop-clear :which-key "clear desktop")
    "ds" '(desktop-save :which-key "save desktop")
    "f"  '(:ignore t :which-key "compile")
    "ff" '(counsel-find-file :which-key "load file")
    "fs" '(save-buffer :which-key "save-buffer")
    "fp" '(projectile--find-file :which-key "load project file")
    "g"  '(:ignore t :which-key "git")
    "gs" '(magit :which-key "git status")
    "o"  '(:ignore t :which-key "org")
    "oa" '(org-agenda :which-key "agenda")
    "oc" '(org-capture :which-key "org capture")
    "p"  'projectile-command-map
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "w"  '(:ignore t :which-key "window")
    "ws" '(split-window-below :which-key "split")
    "wv" '(split-window-right :which-key "split vertical")
    "wo" '(ace-window :which-key "ace-window")
    "w=" '(balance-windows :which-key "balance windows")))

(general-define-key 
    :keymap 'company-active-map 
    "TAB" 'company-complete-common-or-cycle)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package command-log-mode)

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package doom-themes
  :config (load-theme 'doom-palenight t))

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(desktop-read)

(use-package which-key
  :init
  (which-key-mode 1)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package swiper)
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

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

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(efs/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 200
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
  (expand-file-name "~/.emacs.d/"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

      (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package pdf-tools)

(use-package paredit
  :hook
  (emacs-lisp-mode-hook . enable-paredit-mode)
  (eval-expression-minibuffer-setup-hook . enable-paredit-mode)
  (ielm-mode-hook . enable-paredit-mode)
  (lisp-mode-hook . enable-paredit-mode)
  (lisp-interaction-mode-hook . enable-paredit-mode)
  (scheme-mode-hook . enable-paredit-mode))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :config
  (global-company-mode 1)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-selection-wrap-around 1)) 

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-lsp)

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((zig-mode . lsp)
         (lsp-mode . efs/lsp-mode-setup))
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))
  ;;:custom
  ;;(lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package python-mode
  :ensure nil
  :custom
  (python-shell-interpreter "python3"))

(use-package lsp-jedi
  :ensure t
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))

(use-package pyenv-mode
  :hook (python-mode . pyenv-mode))

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package jinja2-mode)
(use-package terraform-mode)

(use-package zig-mode)
(setq lsp-zig-zls-executable "~/bin/zls")

(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "ccls")
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'ccls)))

(use-package geiser)

(use-package sly)
(use-package paredit)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'python-mode-hook           #'enable-paredit-mode)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;(use-package evil-magit
;;  :after magit)

;;(use-package forge)

(use-package term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-color-mode))

(use-package vterm)

(defun efs/configure-eshell ()
   ;; Save command history when commands are entered
   (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

   ;; Truncate buffer for performance
   (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)


   (setq eshell-history-size         10000
         eshell-buffer-maximum-lines 10000
         eshell-hist-ignoredups t
         eshell-scroll-to-bottom-on-input t))

 (use-package eshell-git-prompt)

 (use-package eshell
   :hook (eshell-first-time-mode . efs/configure-eshell)
   :config

   (with-eval-after-load 'esh-opt
     (setq eshell-destroy-buffer-when-process-dies t)
     (setq eshell-visual-commands '("htop" "zsh" "vim")))  ; run these commands in term-mode

   (eshell-git-prompt-use-theme 'powerline))



(use-package realgud)
