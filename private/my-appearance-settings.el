;; ------------------------------------------------------------
;; Appearance

;; Not for puTTY
(when window-system
    (load-theme 'zenburn t)
   (if (> emacs-major-version 23)
        (load-theme 'zenburn t)
      (progn
        (require 'color-theme)
        (color-theme-initialize)
        (color-theme-clarity))))

(if (member "Consolas" (font-family-list))
  (set-face-attribute 'default nil :font "Consolas")
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-face-attribute 'default nil :font "DejaVu Sans Mono")))
;; (set-face-attribute 'default nil :height 110)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(provide 'my-appearance-settings)
