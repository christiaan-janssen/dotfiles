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

(setq version-control t)

;; == [ GNU elpa GPG error work around ] ==
;; WARNIGN: This will disable signature check on install for any package
;;(setq package-check-signature nil)


;; == [ Setup some defaults ] ==
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq delete-old-versions -1)    ;; delete excessclojure play sound backup versions silently
(setq version-control t)    ; use version control
(setq vc-make-backup-files t)    ; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))) ; which directory to put backups file
(setq vc-follow-symlinks t)               ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))) ;transform backups file name
(setq inhibit-startup-screen t)  ; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore)  ; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8)  ; use utf-8 by default
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)  ; sentence SHOULD end with only a point.
(setq fill-column 80)    ; toggle wrapping text at the 80th character

(set-face-attribute 'default t :font "Source Code Pro" :height 100)

(use-package exec-path-from-shell :ensure t)
(use-package eyebrowse :ensure t)
(use-package helm :ensure t)
(require 'helm-config)
(use-package general :ensure t)
(use-package smex :ensure t)
(use-package magit :ensure t)
(use-package multiple-cursors :ensure t)
(use-package swiper :ensure t)
(use-package org :ensure t)

(use-package company :ensure t)
(add-hook 'after-init-hook 'global-company-mode)

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t))

(use-package paredit
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'enable-paredit-mode))

(use-package sly :ensure t)
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (sly paredit projectile company swiper multiple-cursors magit smex general helm eyebrowse exec-path-from-shell use-package bind-key))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
