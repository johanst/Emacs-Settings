;; ------------------------------------------------------------
;; rst mode

(require 'rst)
(add-hook 'rst-mode-hook 'turn-on-orgtbl)

(defun find-top-left-table-marker(top-left-marker)
  (save-excursion
    (backward-paragraph)
    (search-forward top-left-marker (save-excursion (end-of-paragraph-text) (point)) t)))

(defun is-org-table()
  (interactive)
  (save-excursion
    (search-forward "|-" (save-excursion (forward-paragraph) (point)) t)))

(defun seq-replace(token-alist)
  (mapcar '(lambda (token)
             (if (re-search-forward (car token) nil t)
                 (replace-match (cdr token))))
          token-alist))

(defun toggle-rst-org-table()
  (interactive)
  (save-excursion
     (backward-paragraph)
     (if (is-org-table)
        (seq-replace '(("|-" . "+-") ("-|" . "-+") ("|-" . "+-") ("-|" . "-+")))
        (seq-replace '(("\\([^-]\\)\\+-" . "\\1|-") ("-\\+\\([^-]\\)" . "-|\\1") ("\\([^-]\\)\\+-" . "\\1|-") ("-\\+\\([^-]\\)" . "-|\\1"))))))

;; +----+---+---+---+---+
;; |    |   |   |   |   |
;; +----+---+---+---+---+

;; |----+---+---+---+---|
;; |    |   |   |   |   |
;; |----+---+---+---+---|

(add-hook 'rst-mode-hook '(lambda ()
 (define-key rst-mode-map [f9] 'toggle-rst-org-table)))

(provide 'my-rst-settings)
