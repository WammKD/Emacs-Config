(define-minor-mode window-size-minor-mode
  "A minor mode to adjust window size with quick key commands in one go."
  :lighter " win"
  :keymap  (let ((map (make-keymap)))
             (set-char-table-range (nth 1 map) t (lambda ()
                                                   (interactive)))
             (define-key map (kbd "-")         'shrink-window)
             (define-key map (kbd "=")         'enlarge-window)
             (define-key map (kbd "DEL")       'balance-windows)
             (define-key map (kbd "[")         'shrink-window-horizontally)
             (define-key map (kbd "]")         'enlarge-window-horizontally)
             (define-key map (kbd "q")         'window-size-minor-mode)
             (define-key map (kbd "C-g")       'window-size-minor-mode)
             (define-key map (kbd "RET")       'window-size-minor-mode)
             (define-key map (kbd "TAB")       (lambda ()
                                                 (interactive)

                                                 (window-size-minor-mode -1)
                                                 (other-window 1)
                                                 (window-size-minor-mode t)))
             (define-key map (kbd "<backtab>") (lambda ()
                                                 (interactive)

                                                 (window-size-minor-mode -1)
                                                 (other-window -1)
                                                 (window-size-minor-mode t)))
             (define-key map (kbd "ESC")       nil)
             (define-key map (kbd "C-M-i")     (lambda ()
                                                 (interactive)

                                                 (window-size-minor-mode -1)
                                                 (other-window -1)
                                                 (window-size-minor-mode t)))
             (define-key map (kbd "ESC")       (lambda ()
                                                 (interactive)

                                                 (window-size-minor-mode -1)
                                                 (other-window -1)
                                                 (window-size-minor-mode t)
                                                 (enlarge-window-horizontally 1)))
             map))

(global-set-key (kbd "M-W") 'window-size-minor-mode)

(provide 'window-size)
