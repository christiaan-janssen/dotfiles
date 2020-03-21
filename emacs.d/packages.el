(use-package restclient :ensure t)
(use-package eyebrowse :ensure t)
(use-package helm-posframe :ensure t)
(use-package general :ensure t)
(use-package smex :ensure t)
(use-package magit :ensure t)
(use-package multiple-cursors :ensure t)
(use-package swiper :ensure t)
(use-package org :ensure t)
(use-package neotree :ensure t)
(use-package avy :ensure t)
(use-package ace-window :ensure t)
(use-package exec-path-from-shell :ensure t)
(use-package helm :ensure t)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package dap-mode)
(use-package sly :ensure t)
(use-package racket-mode :ensure t)
(use-package geiser :ensure t)
(use-package lastfm :ensure t)
(use-package vuiet :ensure t)

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package company-quickhelp
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'company-quickhelp-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
