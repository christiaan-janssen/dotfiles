
;;;; == [ Themes ] ============
(set-face-attribute 'default nil :font "Fira Code" :height 120)

(use-package all-the-icons :ensure t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-outrun-electric t)
  (setq doom-outrun-electric-brighter-comments t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))

(global-display-line-numbers-mode) ;; Enable line numbers in all buffers

