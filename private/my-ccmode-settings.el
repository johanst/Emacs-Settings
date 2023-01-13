;; ------------------------------------------------------------
;; CC mode customizations.

(require 'column-marker)

(setq my-ccmode-settings-project-path "")

(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)
(setq c-max-one-liner-length 80)

(defconst my-c-style
  '("k&r"
    (c-basic-offset        . 4)
    (c-hanging-braces-alist     . ((defun-open after)
                                   (brace-list-open)
                                   (brace-entry-open)
                                   (statement-cont)
                                   (substatement-open after)
                                   (block-close . c-snug-do-while)
                                   (extern-lang-open after)
                                   (namespace-open after)
                                   (namespace-close)
                                   (inline-open)
                                   (inline-close after)
                                   (inexpr-class-open after)
                                   (inexpr-class-close before)))
    (c-cleanup-list             . (scope-operator
                                   brace-else-brace
                                   brace-elseif-brace
                                   brace-catch-brace
                                   one-liner-defun
                                   empty-defun-braces
                                   defun-close-semi))
    (c-hanging-semi&comma-criteria . (c-semi&comma-no-newlines-for-oneline-inliners
                                      c-semi&comma-inside-parenlist))
    (c-offsets-alist            . ((innamespace . 0)
                                   (arglist-intro . 4)
                                   (arglist-cont . 0)
                                   (arglist-cont-nonempty . 4)))
    )
  "My prefered C Programming Style")
(c-add-style "sternerup" my-c-style)

(defconst horrible-c-style
  '("bsd" (c-basic-offset        . 2)
    (c-hanging-braces-alist     . ((statement-cont)
                                   (block-close . c-snug-do-while)
                                   (namespace-open)
                                   (namespace-close)
                                   (inline-open)
                                   (inline-close after)))
    (c-cleanup-list             . (scope-operator
                                   one-liner-defun
                                   empty-defun-braces
                                   defun-close-semi))
    (c-hanging-semi&comma-criteria . (c-semi&comma-no-newlines-for-oneline-inliners
                                      c-semi&comma-inside-parenlist))
    (c-offsets-alist            . ((innamespace . 0)
                                   (case-label . 2)
                                   (arglist-intro . 4)
                                   (statement-cont . 4)))

)
  "Horrible indentation C Programming Style")
(c-add-style "horrible" horrible-c-style)

(defconst jonasc-c-style
  '("bsd" (c-basic-offset        . 4)
    (c-hanging-braces-alist     . ((statement-cont)
                                   (block-close . c-snug-do-while)
                                   (namespace-open)
                                   (namespace-close)
                                   (inline-open)
                                   (inline-close after)))
    (c-cleanup-list             . (scope-operator
                                   one-liner-defun
                                   empty-defun-braces
                                   defun-close-semi))
    (c-hanging-semi&comma-criteria . (c-semi&comma-no-newlines-for-oneline-inliners
                                      c-semi&comma-inside-parenlist))
    (c-offsets-alist            . ((innamespace . 0)
                                   (case-label . 2)
                                   (arglist-intro . 4)
                                   (statement-cont . 4)))

)
  "JonasC indentation C Programming Style")
(c-add-style "jonasc" jonasc-c-style)


(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (protobuf-mode . "sternerup")
                        (other . "sternerup")))

;; This allows quick buffer local switching between different
;; indentation styles.
(defun c-set-horrible-indentation ()
(interactive)
  (c-set-style "horrible"))
(defun c-set-readable-indentation ()
(interactive)
  (c-set-style "sternerup"))
(defun c-set-jonasc-indentation ()
(interactive)
  (c-set-style "jonasc"))


(defun c-default-include-guard-name(fullpath)
  (file-name-nondirectory (file-name-sans-extension fullpath)))

(setq c-custom-include-guard-name nil)

(defun c-include-guard-name(fullpath)
  (if c-custom-include-guard-name
      (funcall c-custom-include-guard-name fullpath)
    (c-default-include-guard-name fullpath)))

(defun c-insert-include-guard ()
  (interactive)
  (if (buffer-file-name)
      (let*
          ((fName (upcase (c-include-guard-name buffer-file-name)))
           (ifDef (concat "#ifndef " fName "_H_" "\n#define " fName "_H_" "\n"))
           (begin (point-marker))
           )
        (progn
          (goto-char (point-min))
          (insert ifDef)
          (goto-char (point-max))
          (insert "\n#endif" " /* " fName "_H_ */\n")
          (goto-char begin))
        )
                                        ;else
    (message (concat "Buffer " (buffer-name) " must have a filename"))
    )
)

;; LISP-hacked formatting tool for fixing up long arguments lists in function calls.
;; clang-format does it much better...

(defun mycc-next-arg-in-scope(begin)
  (interactive "p")
  (let ((match nil))
    (while
        (and (not match)
             (re-search-forward "[,)]" (point-max) t))
      (let ((pointsave (point)))
        (backward-char)
        (backward-up-list)
        (when (= begin (point))
          (setq match (match-string 0)))
        (goto-char pointsave)))
    match))

(defun mycc-has-multiple-args(&optional begin)
  (interactive "p")
  (save-excursion
    (not (equal ")" (mycc-next-arg-in-scope begin)))))

(defun mycc-delete-newlines()
  (interactive)
  (while (= 10 (following-char))
    (delete-char 1)))

(defun mycc-format-paren (num-lines begin)
  (let ((match ""))
    (goto-char begin)
    (forward-char)
    (mycc-delete-newlines)
    (unless (= num-lines 0) (newline))
    (while (and match (not (equal ")" match)))
      (let ((pos (point)))
        (setq match (mycc-next-arg-in-scope begin))
        (when match
          (let ((bound (point)))
            (goto-char pos)
            (mycc-delete-newlines)
            (re-search-forward "\\s-*\\(.*[,)]\\)" bound t)
            (replace-match "\\1"))
          (when (equal "," match)
            (if (= num-lines 2)
                (newline)
              (insert " "))))))
    (c-indent-region begin (point))))

(defun c-format-parentheses-contents ()
  (interactive)
  (save-excursion
    (backward-up-list)
    (let ((begin (point))
          (begin-line-number (line-number-at-pos (point))))
      (forward-list)
      (let ((num-lines (- (line-number-at-pos (point)) begin-line-number))
            (end (point)))
        (mycc-format-paren
         (cond
          ((= num-lines 0) 1)
          ((> num-lines 1) 0)
          (t (if (mycc-has-multiple-args begin) 2 0)))
         begin)))))

;; rtags
;; Build and install rtags from https://github.com/Andersbakken/rtags
;; Make sure rtags-path is correct.
;; Generate compilation database with
;; ninja -C $NINJA_BUILD_DIR -t compdb -x c_COMPILER cpp_COMPILER >$SRC_DIR/compile_commands.json

(require 'rtags)
(require 'flycheck-rtags)

;; ensure that we use only rtags checking
;; https://github.com/Andersbakken/rtags#optional-1
(defun setup-flycheck-rtags ()
  (interactive)
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))

(defun within-dir-p(dir)
  (string-prefix-p dir (buffer-file-name)))

(setq my-rtags-projects nil)
(defun setup-rtags ()
  (interactive)
  (when (cl-find-if 'within-dir-p my-rtags-projects)
    (setq-local rtags-completions-enabled t)
    (rtags-start-process-unless-running)
    (rtags-enable-standard-keybindings)
    (local-set-key (kbd "M-.") (function rtags-find-symbol-at-point))
    (local-set-key (kbd "M-,") (function rtags-find-references-at-point))
    (setq-local rtags-use-helm t)
    (setq-local rtags-display-result-backend 'helm)
    ;; company completion setup
    (setq-local rtags-autostart-diagnostics t)
    (rtags-diagnostics)
    (setup-flycheck-rtags)))

(setq rtags-path "/home/johast/hacking/rtags/build/bin")
(setq rtags-process-flags "-j 4 --no-filesystem-watcher")
(push 'company-rtags company-backends)
(add-hook 'c-mode-common-hook (function setup-rtags))

;; Use Auto-newline by default
(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))
;; will introduce spaces instead of tabs by default.
(add-hook 'c-mode-common-hook '(lambda () (setq indent-tabs-mode nil)))
(add-hook 'c-mode-common-hook '(lambda () (setq c-tab-always-indent t)))

(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

(define-key c-mode-base-map (kbd "C-c o") (function ff-find-other-file))
(define-key c-mode-base-map (kbd "C-c 1") (function c-set-readable-indentation))
(define-key c-mode-base-map (kbd "C-c 2") (function c-set-horrible-indentation))
(define-key c-mode-base-map (kbd "C-c g") (function c-insert-include-guard))
(define-key c-mode-base-map (kbd "C-c C-f") (function c-format-parentheses-contents))
(add-hook 'c-mode-common-hook '(lambda () (column-marker-3 80)))

(setq ff-other-file-alist
      '(("\\.cc\\'"  (".hh" ".h"))
        ("\\.hh\\'"  (".cpp" ".cc" ".C"))

        ("\\.c\\'"   (".h"))
        ("\\.h\\'"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m" ".mm"))

        ("\\.m\\'"    (".h"))
        ("\\.mm\\'"    (".h"))

        ("\\.C\\'"   (".H"  ".hh" ".h"))
        ("\\.H\\'"   (".C"  ".CC"))

        ("\\.CC\\'"  (".HH" ".H"  ".hh" ".h"))
        ("\\.HH\\'"  (".CC"))

        ("\\.c\\+\\+\\'" (".h++" ".hh" ".h"))
        ("\\.h\\+\\+\\'" (".c++"))

        ("\\.cpp\\'" (".hpp" ".hh" ".h"))
        ("\\.hpp\\'" (".cpp"))

        ("\\.cxx\\'" (".hxx" ".hh" ".h"))
        ("\\.hxx\\'" (".cxx"))))

(provide 'my-ccmode-settings)
