(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'package)
(require 'cl-lib)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;;                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar ensure-packages
  '(
    zenburn-theme
;;    column-marker
    ace-jump-mode
    key-chord
    clang-format
    magit
    pyvenv
    adoc-mode
    company
    flycheck
    helm
    rtags
    flycheck-rtags
    helm-rtags
    company-rtags
    irony
    irony-eldoc
    company-irony
    flycheck-irony
    rust-mode
    rustic
    lsp-mode
    )
  "A list of packages to ensure are installed at launch.")

(defun ensure-packages-package-installed-p (p)
  (cond ((package-installed-p p) t)
	(t nil)))

(defun ensure-packages-installed-p ()
  (mapcar 'ensure-packages-package-installed-p ensure-packages))

(defun ensure-packages-install-missing ()
  (interactive)
  (unless (cl-every 'identity (ensure-packages-installed-p))
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

(add-to-list 'load-path "~/.emacs.d/private")
(add-to-list 'load-path "~/.emacs.d/external") ;; manually downloaded as marmalade is broken

;; Use bash to avoid all zsh fanciness in Emacs
(setq shell-file-name "bash")
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(defun my-revert-buffer-preserve-modes ()
  (interactive)
  (if (eq major-mode 'compilation-mode)
      (progn
	(revert-buffer t t t)
	(compilation-mode))
    (revert-buffer nil nil t)))

(global-set-key [f10] 'nil)

(require 'my-ido-settings)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq org-startup-indented t)
(global-set-key "\C-cl" 'org-store-link)

(require 'company)
(global-company-mode)

(require 'flycheck)
(global-flycheck-mode)

(require 'column-marker)

(require 'my-putty-settings)
(require 'my-general-editing-settings)
(require 'my-ccmode-settings)
(require 'my-rst-settings)
(require 'my-python-settings)
(require 'my-appearance-settings)
(require 'my-rust-settings)

;; ------------------------------------------------------------
;; Global keymap modifications

(global-unset-key "\C-z") ;; Just annoying when hit by mistake

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

;; clang-format
(require 'clang-format)
(global-set-key (kbd "C-c i") 'clang-format-region)
(global-set-key (kbd "C-c u") 'clang-format-buffer)

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)


;; gdb settings

;; (if (> emacs-major-version 23)
;;    (require 'gdb-ui))

;; (setq gdb-many-windows t)
(setq gdb-show-main t)
(setq gdb-create-source-file-list nil)

;; Byoubu leaves just a few keys for me
(global-set-key [f7] 'recompile)
(global-set-key [M-f7] 'in-directory)
(global-set-key [f9] 'my-revert-buffer-preserve-modes)
(if (not (global-key-binding [f10] nil))
    (global-set-key [f10] 'gdb-restore-windows))
(global-set-key [f11] 'magit-status)
(global-set-key (kbd "C-x G") 'magit-status)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; ------------------------------------------------------------
;; Emacs customization

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-todo-keywords '((sequence "TODO" "WAITING" "DONE")))
 '(package-selected-packages
   '(dash lsp-mode flycheck-irony company-irony irony-eldoc irony helm-rtags company-rtags flycheck-rtags helm flycheck company rtags rust-mode go-mode meson-mode typescript-mode markdown-mode json-mode adoc-mode plantuml-mode zenburn-theme pyvenv magit key-chord clang-format ace-jump-mode))
 '(show-trailing-whitespace t)
 '(sql-sqlite-program "sqlite3")
 '(warning-suppress-types '(((package reinitialization))))
 '(woman-locale "en_US.UTF-8"))
 '(custom-safe-themes (quote ("f715f948867d85fa384b6c20d793dfd126d71996afd62f9d003705c960929977" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)

(setq magit-last-seen-setup-instructions "1.4.0")
