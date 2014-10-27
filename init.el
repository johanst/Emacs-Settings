(require 'package)
(require 'cl)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar ensure-packages
  '(zenburn-theme column-marker protobuf-mode ace-jump-mode key-chord auto-complete
		  ac-etags magit virtualenvwrapper)
  "A list of packages to ensure are installed at launch.")

(defun ensure-packages-package-installed-p (p)
  (cond ((package-installed-p p) t)
	(t nil)))

(defun ensure-packages-installed-p ()
  (mapcar 'ensure-packages-package-installed-p ensure-packages))

(defun ensure-packages-install-missing ()
  (interactive)
  (unless (every 'identity (ensure-packages-installed-p))
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p ensure-packages)
    (when (not (package-installed-p p))
      (package-install p)))))

;; (provide 'ensure-packages)
(ensure-packages-install-missing)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/private")

(require 'my-ido-settings)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(require 'etags-select)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

(require 'protobuf-mode)

(setq org-startup-indented t)
(global-set-key "\C-cl" 'org-store-link)

(require 'my-putty-settings)
(require 'my-general-editing-settings)
(require 'my-ccmode-settings)
(require 'my-rst-settings)
(require 'my-python-settings)
(require 'my-nsp-settings)
(require 'my-appearance-settings)

;; ------------------------------------------------------------
;; Global keymap modifications

(key-chord-mode 1)

;; Navigation
(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key [M-left]  'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up]    'windmove-up)
(global-set-key [M-down]  'windmove-down)

(key-chord-define-global "jj" 'ace-jump-char-mode)
(key-chord-define-global "JJ" 'ace-jump-line-mode)

;; Anti electricity
(key-chord-define-global "{{" '(lambda () (interactive) (insert-char ?{)))
(key-chord-define-global "}}" '(lambda () (interactive) (insert-char ?})))
(key-chord-define-global ";;" '(lambda () (interactive) (insert-char ?\;)))

;; Editing
(global-set-key (kbd "M-$") 'query-replace-regexp)

;; gdb settings

;; (if (> emacs-major-version 23)
;;    (require 'gdb-ui))

;; (setq gdb-many-windows t)
(setq gdb-show-main t)
(setq gdb-create-source-file-list nil)

;; Byoubu leaves just a few keys for me
(global-set-key [f7] 'recompile)
(global-set-key [M-f7] 'in-directory)
(global-set-key [f10] 'gdb-restore-windows)
(global-set-key [f11] 'magit-status)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; ------------------------------------------------------------
;; Emacs customization

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-todo-keywords (quote ((sequence "TODO" "WAITING" "DONE"))))
 '(show-trailing-whitespace t)
 '(sql-sqlite-program "sqlite3"))
 '(custom-safe-themes (quote ("f715f948867d85fa384b6c20d793dfd126d71996afd62f9d003705c960929977" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)
