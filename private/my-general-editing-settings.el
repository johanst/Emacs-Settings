;; ------------------------------------------------------------
;; General Editing

;; Copy whole line with M-w when there is no selection
(put 'kill-ring-save 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

;; Cut whole line with C-w when there is no selection
(put 'kill-region 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

;; Improve zap-to-char (M-z) by not deleting the boundary character
;; I.e zap-to-char '>' on <Xabcdef> (cursor on X) gives <>.
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

;;
;; Jump to matching paren if on a paren
;;
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)

(require 'auto-complete)
(global-auto-complete-mode t)

(require 'ac-etags)
(eval-after-load "etags"
  '(progn
     (ac-etags-setup)))

(provide 'my-general-editing-settings)
