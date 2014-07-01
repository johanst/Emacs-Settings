;; ----------------------------------------------------------------
;; Key translation magic for transferring Function keys using PuTTY
;; configured as:
;; The Function keys and keypad = XFree86 XTerm
;; The Home and End keys        = rxvt

(if (eq system-uses-terminfo t)
    (progn ;; PuTTY hack - needs to be in XTerm XFree86 mode
      (define-key key-translation-map [\e] [\M])

      (define-key input-decode-map "\e[1;2A" [S-up])
      (define-key input-decode-map "\e[1;2B" [S-down])
      (define-key input-decode-map "\e[1;2C" [S-right])
      (define-key input-decode-map "\e[1;2D" [S-left])
      (define-key input-decode-map "\e[1;3A" [M-up])
      (define-key input-decode-map "\e[1;3B" [M-down])
      (define-key input-decode-map "\e[1;3C" [M-right])
      (define-key input-decode-map "\e[1;3D" [M-left])
      (define-key input-decode-map "\e[1;4A" [S-M-up])
      (define-key input-decode-map "\e[1;4B" [S-M-down])
      (define-key input-decode-map "\e[1;4C" [S-M-right])
      (define-key input-decode-map "\e[1;4D" [S-M-left])
      (define-key input-decode-map "\e[1;5A" [C-up])
      (define-key input-decode-map "\e[1;5B" [C-down])
      (define-key input-decode-map "\e[1;5C" [C-right])
      (define-key input-decode-map "\e[1;5D" [C-left])
      (define-key input-decode-map "\e[1;6A" [S-C-up])
      (define-key input-decode-map "\e[1;6B" [S-C-down])
      (define-key input-decode-map "\e[1;6C" [S-C-right])
      (define-key input-decode-map "\e[1;6D" [S-C-left])
      (define-key input-decode-map "\e[1;7A" [M-C-up])
      (define-key input-decode-map "\e[1;7B" [M-C-down])
      (define-key input-decode-map "\e[1;7C" [M-C-right])
      (define-key input-decode-map "\e[1;7D" [M-C-left])
      (define-key input-decode-map "\e[1;8A" [S-M-C-up])
      (define-key input-decode-map "\e[1;8B" [S-M-C-down])
      (define-key input-decode-map "\e[1;8C" [S-M-C-right])
      (define-key input-decode-map "\e[1;8D" [S-M-C-left])


      (define-key input-decode-map "\e[H" [home])
      (define-key input-decode-map "\e[F" [end])

      ;; f1
      (define-key input-decode-map "\eOP" [f1])
      (define-key input-decode-map "\eO2P" [S-f1])
      (define-key input-decode-map "\eO3P" [M-f1])
      (define-key input-decode-map "\eO4P" [S-M-f1])
      (define-key input-decode-map "\eO5P" [C-f1])
      (define-key input-decode-map "\eO6P" [S-C-f1])
      (define-key input-decode-map "\eO7P" [M-C-f1])
      (define-key input-decode-map "\eO8P" [S-M-C-f1])
      ;; f2
      (define-key input-decode-map "\eOQ" [f2])
      (define-key input-decode-map "\eO2Q" [S-f2])
      (define-key input-decode-map "\eO3Q" [M-f2])
      (define-key input-decode-map "\eO4Q" [S-M-f2])
      (define-key input-decode-map "\eO5Q" [C-f2])
      (define-key input-decode-map "\eO6Q" [S-C-f2])
      (define-key input-decode-map "\eO7Q" [M-C-f2])
      (define-key input-decode-map "\eO8Q" [S-M-C-f2])
      ;; f3
      (define-key input-decode-map "\eOQ" [f3])
      (define-key input-decode-map "\eO2Q" [S-f3])
      (define-key input-decode-map "\eO3Q" [M-f3])
      (define-key input-decode-map "\eO4Q" [S-M-f3])
      (define-key input-decode-map "\eO5Q" [C-f3])
      (define-key input-decode-map "\eO6Q" [S-C-f3])
      (define-key input-decode-map "\eO7Q" [M-C-f3])
      (define-key input-decode-map "\eO8Q" [S-M-C-f3])
      ;; f4
      (define-key input-decode-map "\eOR" [f4])
      (define-key input-decode-map "\eO2R" [S-f4])
      (define-key input-decode-map "\eO3R" [M-f4])
      (define-key input-decode-map "\eO4R" [S-M-f4])
      (define-key input-decode-map "\eO5R" [C-f4])
      (define-key input-decode-map "\eO6R" [S-C-f4])
      (define-key input-decode-map "\eO7R" [M-C-f4])
      (define-key input-decode-map "\eO8R" [S-M-C-f4])
      ;; f5
      (define-key input-decode-map "\e[15~" [f5])
      (define-key input-decode-map "\e[15;2~" [S-f5])
      (define-key input-decode-map "\e[15;3~" [M-f5])
      (define-key input-decode-map "\e[15;4~" [S-M-f5])
      (define-key input-decode-map "\e[15;5~" [C-f5])
      (define-key input-decode-map "\e[15;6~" [S-C-f5])
      (define-key input-decode-map "\e[15;7~" [M-C-f5])
      (define-key input-decode-map "\e[15;8~" [S-M-C-f5])
      ;; f6
      (define-key input-decode-map "\e[17~" [f6])
      (define-key input-decode-map "\e[17;2~" [S-f6])
      (define-key input-decode-map "\e[17;3~" [M-f6])
      (define-key input-decode-map "\e[17;4~" [S-M-f6])
      (define-key input-decode-map "\e[17;5~" [C-f6])
      (define-key input-decode-map "\e[17;6~" [S-C-f6])
      (define-key input-decode-map "\e[17;7~" [M-C-f6])
      (define-key input-decode-map "\e[17;8~" [S-M-C-f6])
      ;; f7
      (define-key input-decode-map "\e[18~" [f7])
      (define-key input-decode-map "\e[18;2~" [S-f7])
      (define-key input-decode-map "\e[18;3~" [M-f7])
      (define-key input-decode-map "\e[18;4~" [S-M-f7])
      (define-key input-decode-map "\e[18;5~" [C-f7])
      (define-key input-decode-map "\e[18;6~" [S-C-f7])
      (define-key input-decode-map "\e[18;7~" [M-C-f7])
      (define-key input-decode-map "\e[18;8~" [S-M-C-f7])
      ;; f8
      (define-key input-decode-map "\e[19~" [f8])
      (define-key input-decode-map "\e[19;2~" [S-f8])
      (define-key input-decode-map "\e[19;3~" [M-f8])
      (define-key input-decode-map "\e[19;4~" [S-M-f8])
      (define-key input-decode-map "\e[19;5~" [C-f8])
      (define-key input-decode-map "\e[19;6~" [S-C-f8])
      (define-key input-decode-map "\e[19;7~" [M-C-f8])
      (define-key input-decode-map "\e[19;8~" [S-M-C-f8])
      ;; f9
      (define-key input-decode-map "\e[20~" [f9])
      (define-key input-decode-map "\e[20;2~" [S-f9])
      (define-key input-decode-map "\e[20;3~" [M-f9])
      (define-key input-decode-map "\e[20;4~" [S-M-f9])
      (define-key input-decode-map "\e[20;5~" [C-f9])
      (define-key input-decode-map "\e[20;6~" [S-C-f9])
      (define-key input-decode-map "\e[20;7~" [M-C-f9])
      (define-key input-decode-map "\e[20;8~" [S-M-C-f9])
      ;; f10
      (define-key input-decode-map "\e[21~" [f10])
      (define-key input-decode-map "\e[21;2~" [S-f10])
      (define-key input-decode-map "\e[21;3~" [M-f10])
      (define-key input-decode-map "\e[21;4~" [S-M-f10])
      (define-key input-decode-map "\e[21;5~" [C-f10])
      (define-key input-decode-map "\e[21;6~" [S-C-f10])
      (define-key input-decode-map "\e[21;7~" [M-C-f10])
      (define-key input-decode-map "\e[21;8~" [S-M-C-f10])
      ;; f11
      (define-key input-decode-map "\e[23~" [f11])
      (define-key input-decode-map "\e[23;2~" [S-f11])
      (define-key input-decode-map "\e[23;3~" [M-f11])
      (define-key input-decode-map "\e[23;4~" [S-M-f11])
      (define-key input-decode-map "\e[23;5~" [C-f11])
      (define-key input-decode-map "\e[23;6~" [S-C-f11])
      (define-key input-decode-map "\e[23;7~" [M-C-f11])
      (define-key input-decode-map "\e[23;8~" [S-M-C-f11])
      ;; f12
      (define-key input-decode-map "\e[24~" [f12])
      (define-key input-decode-map "\e[24;2~" [S-f12])
      (define-key input-decode-map "\e[24;3~" [M-f12])
      (define-key input-decode-map "\e[24;4~" [S-M-f12])
      (define-key input-decode-map "\e[24;5~" [C-f12])
      (define-key input-decode-map "\e[24;6~" [S-C-f12])
      (define-key input-decode-map "\e[24;7~" [M-C-f12])
      (define-key input-decode-map "\e[24;8~" [S-M-C-f12])))

(provide 'my-putty-settings)
