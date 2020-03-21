
;;;; == [ Key Bindings ] ======

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
