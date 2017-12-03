(defun text-reading-mode-recenter ()
  (interactive)
  (recenter-top-bottom 0))

(defvar text-reading-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'text-reading-mode-recenter)
    map)
  "Keymap used in `text-reading-mode' buffers.")

(define-derived-mode text-reading-mode text-mode "Text Reading"
  "Mode for reading text documents."
  0
  0
  text-reading-mode-map
  (read-only-mode 1))

(provide 'text-reading-mode)

;;; text-reading-mode.el ends here
