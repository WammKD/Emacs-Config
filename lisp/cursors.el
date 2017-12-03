;; Change cursor color according to mode; inspired by
;; http://www.emacswiki.org/emacs/ChangingCursorDynamically
;; valid values are t, nil, box, hollow, bar, (bar . WIDTH), hbar,
;; (hbar. HEIGHT); see the docs for set-cursor-type

(setq djcb-read-only-color       "gray")
(setq djcb-read-only-cursor-type 'hbar)
(setq djcb-overwrite-color       "red")
(setq djcb-overwrite-cursor-type 'box)
(setq djcb-normal-color          "#74BB2F")   ;; green
(setq djcb-normal-cursor-type    'bar)

(defun djcb-set-cursor-according-to-mode ()
  "Change cursor color and type according to some minor modes."
  (cond
    (buffer-read-only (if (display-graphic-p)
                          (progn
                            (set-cursor-color djcb-read-only-color)
                            (setq cursor-type djcb-read-only-cursor-type))
                        (send-string-to-terminal "\033]12;yellow\007")))
    (overwrite-mode   (if (display-graphic-p)
                          (progn
                            (set-cursor-color djcb-overwrite-color)
                            (setq cursor-type djcb-overwrite-cursor-type))
                        (send-string-to-terminal "\033]12;red\007")))
    (t                (if (display-graphic-p)
                          (progn
                            (set-cursor-color djcb-normal-color)
                            (setq cursor-type djcb-normal-cursor-type))
                        (send-string-to-terminal "\033]12;lightgreen\007")))))

(add-hook 'post-command-hook 'djcb-set-cursor-according-to-mode)

(defun correct-cursor-color ()
  "Change cursor back to White if quiting Emacs from terminal."
  (interactive)
  (unless (display-graphic-p)
    (send-string-to-terminal "\033]12;white\007")))

(add-hook 'kill-emacs-hook 'correct-cursor-color)
