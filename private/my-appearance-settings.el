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

(set-cursor-color "#FFFF00")

(if (member "Consolas" (font-family-list))
  (set-face-attribute 'default nil :font "Consolas")
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 140)))
;; (set-face-attribute 'default nil :height 110)

(tool-bar-mode -1)
;; (menu-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell 1)

(provide 'my-appearance-settings)
