;;; rust-repl --- Major mode for rust-repl

;; Copyright 2018 Jonathan Schmeling
;; Majoridly derived from java-repl.el, copyright 2014 Colin Carr
;;
;; Author: jaft.r@outlook.com
;; Time-stamp: <02/04/2018 15:30:57 CST>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'rust-repl)

;
; CUSTOM  -
(defgroup rust-repl nil
  "Major mode for rust-repl"
  :group 'languages)

(defcustom rust-repl-command "rust-repl"
  "The 'rust-repl' command.  Invoke the 'rust-repl' via a wrapper that execs.
the rusti REPL.
Customize this variable to point to the executable."
  :type 'string
  :group 'rust-repl)
(defcustom rust-repl-font-lock-keywords '("as"       "box"    "break"   "const"
                                          "continue" "crate"  "do"      "else"
                                          "enum"     "extern" "false"   "fn"
                                          "for"      "if"     "impl"    "in"
                                          "let"      "loop"   "match"   "mod"
                                          "move"     "mut"    "priv"    "pub"
                                          "ref"      "return" "self"    "static"
                                          "struct"   "super"  "true"    "trait"
                                          "type"     "use"    "virtual" "where"
                                          "while"    "yield")
  "Rust keywords, lifted entirely from `rust-mode-keywords`.
Didn't bother using the variable directly on the off-chance it's inaccessible
while rust-mode isn't running; this'll be revisited, later."
  :type  'list
  :group 'rust-repl)

;;; Code:
(defvar rust-repl-file-path "~/.cargo/bin/rust-repl"
  "Path to the program used by `run-rust-repl'.")
;
(defvar rust-repl-cli-arguments '()
  "Commandline arguments to pass to `rust-repl-cli'.")
;
(defvar rust-repl-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-rust-repl'.")
;
(defvar rust-repl-prompt-regexp "^\\(?:\\[[^@]+@[^@]+\\]\\)"
  "Prompt for `run-rust-repl'.")
;
(defun run-rust ()
  "Run an inferior instance of `rust-repl' inside Emacs."
  (interactive)
  (let* ((rust-repl-program rust-repl-file-path)
         (buffer (comint-check-proc "rust-repl")))
    ;; pop to the "*rust-repl*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer
     (if (or buffer (not (derived-mode-p 'rust-repl-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*rust-repl*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "rust-repl" buffer
             rust-repl-program rust-repl-cli-arguments)
      (rust-repl-mode))))
(defun rust-repl--initialize ()
  "Helper function to initialize rust-repl."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))
;
(define-derived-mode rust-repl-mode comint-mode "rust-repl"
  "Major mode for `run-rust-repl'.

\\<rust-repl-mode-map>"
  nil "rust-repl"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp rust-repl-prompt-regexp)

  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  ;; (setq comint-prompt-read-only t)

  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(rust-repl-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) rust-repl-prompt-regexp))

;; this has to be done in a hook. grumble grumble.
(add-hook 'rust-repl-mode-hook 'rust-repl--initialize)
;;;; END RUST-REPL

(provide 'rust-repl)

;;; rust-repl.el ends here
