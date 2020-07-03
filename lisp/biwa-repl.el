(defgroup biwa-repl nil
  "Major mode for biwa-repl"
  :group 'languages)

(defcustom biwa-repl-command "biwas"
  "The 'biwa-repl' command.  Invoke the 'biwa-repl' via a wrapper that execs.
#!/usr/bin/env bash
exec /usr/bin/biwa -jar $BIWAREPLPATH/bin/biwa-repl.jarlocalhost:biwa-repl
BIWAREPLPATH is installed directory.
Customize this variable to point to the wrapper script."
  :type 'string
  :group 'biwa-repl)

;;; Code:
(defvar biwa-repl-file-path "/usr/local/bin/biwas"
  "Path to the program used by `run-biwa-repl'.")
;
(defvar biwa-repl-cli-arguments '()
  "Commandline arguments to pass to `biwa-repl-cli'.")
;
(defvar biwa-repl-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-biwa-repl'.")
;
(defvar biwa-repl-prompt-regexp "^biwascheme> " ;; "^\\(?:\\[[^@]+@[^@]+\\]\\)"
  "Prompt for `run-biwa-repl'.")
;
(defun run-biwa ()
  "Run an inferior instance of `biwa-repl' inside Emacs."
  (interactive)
  (let* ((biwa-repl-program biwa-repl-file-path)
         (buffer (comint-check-proc "biwa-repl")))
    ;; pop to the "*biwa-repl*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer
     (if (or buffer (not (derived-mode-p 'biwa-repl-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*biwa-repl*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "biwa-repl" buffer
             biwa-repl-program biwa-repl-cli-arguments)
      (biwa-repl-mode))))
(defun biwa-repl--initialize ()
  "Helper function to initialize biwa-repl."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))
;
(define-derived-mode biwa-repl-mode comint-mode "biwa-repl"
  "Major mode for `run-biwa-repl'.

\\<biwa-repl-mode-map>"
  nil "biwa-repl"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp biwa-repl-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(biwa-repl-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) biwa-repl-prompt-regexp))

;; this has to be done in a hook. grumble grumble.
(add-hook 'biwa-repl-mode-hook 'biwa-repl--initialize)
;;;; END BIWA-REPL

(provide 'biwa-repl)

;;; biwa-repl.el ends here
