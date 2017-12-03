;; Loadpaths
(add-to-list 'load-path              "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Packages
(require 'package)

(let* ((no-ssl   (and
                   (memq system-type '(windows-nt ms-dos))
                   (not (gnutls-available-p))))
       (melpaURL (concat (if no-ssl "http" "https") "://melpa.org/packages/"))
       (gnuURL   (concat (if no-ssl "http" "https") "://elpa.gnu.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" melpaURL) t)
  ;; For important compatibility libraries like cl-lib
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives (cons "gnu" gnuURL))))

(defconst packagesList "~/.emacs.d/packages.txt"
  "The file location/name of where the package list is stored.")

(defun save-packages ()
  (with-temp-file packagesList
    (insert (format "%S" package-activated-list))))
; keep package-sync-list up to date when exiting emacs
(add-hook 'kill-emacs-hook 'save-packages)

(defun require-package (package)
  "Install given PACKAGE. This is from Bling's config"
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))

    (package-install package)))
(with-eval-after-load "package.el"
  (mapc
    'require-package
    (car (read-from-string (with-temp-buffer
                             (insert-file-contents packagesList)
                             (buffer-string))))))

;; (defun package-list-updated-packages ()
;;   (package-refresh-contents)
;;   (package-list-packages))

;; Emacs Shit
(load-file "~/.emacs.d/lisp/cursors.el")
(delete-selection-mode t)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq indent-tabs-mode nil)))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string "\\(^[[:space:]\\n]*\\|[[:space:]\\n]*$\\)" "" str))
  ;; Wrap highlighted text in pair
(defun insert-square-brackets (&optional arg)
  "Enclose following ARG sexps in square brackets.
Leave point after open-paren."
  (interactive "*P")

  (insert-pair arg ?\[ ?\]))
(defun insert-curly-braces (&optional arg)
  "Enclose following ARG sexps in curly braces.
Leave point after open-paren."
  (interactive "*P")

  (insert-pair arg ?\{ ?\}))
(defun join-next-line ()
  "Join this line to following and fix up whitespace at join.
If there is a fill prefix, delete it from the beginning of the following line."
  (interactive)

  (join-line -1))
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)

  (message "Buffer reverting")
  (revert-buffer t t)
  (message "Buffer reverted"))

(global-set-key (kbd "M-n")        'next-line)
(global-set-key (kbd "M-p")        'previous-line)
(global-set-key (kbd "RET")        'newline-and-indent)
(global-set-key (kbd "M-j")        'join-next-line)
(global-set-key (kbd "M-RET")      'join-next-line)
(global-set-key (kbd "C-M-j")      'indent-new-comment-line)
(global-set-key (kbd "<C-return>") 'indent-new-comment-line)
;; (global-set-key (kbd "<M-return>") 'indent-new-comment-line)
(global-set-key (kbd "M-]")        'insert-square-brackets)
(global-set-key (kbd "M-{")        'insert-curly-braces)
(global-set-key (kbd "M-\"")       'insert-pair)
(global-set-key (kbd "C-x M-r")    'revert-buffer-no-confirm)
(global-set-key [f7]               'ispell)

(setq column-number-mode t)

  ;; Clipboard Shit
(defun copy-to-clipboard ()
  (interactive)

  (if (display-graphic-p)
      (progn
        (message "Yanked region to x-clipboard!")
        (call-interactively 'clipboard-kill-ring-save))
    (if (region-active-p)
        (progn
          ;; (shell-command-on-region (region-beginning) (region-end) "gpaste add ")
          (shell-command (concat
                           "gpaste-client add \""
                           (replace-regexp-in-string
                             "`"
                             "\\\\`"
                             (replace-regexp-in-string
                               "\""
                               "\\\\\""
                               (buffer-substring-no-properties
                                 (region-beginning)
                                 (region-end))))
                           "\""))
          (message "Yanked region to clipboard!")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard!"))))
(defun paste-from-clipboard ()
  (interactive)

  (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "graphics active"))
    (insert (shell-command-to-string "gpaste-client get 0"))))

(global-set-key [?\C-c ?c] 'copy-to-clipboard)
(global-set-key [?\C-c ?v] 'paste-from-clipboard)

  ;; Eshell Shit
(setq shell-file-name    "bash")

  ;; Buffers Shit
; Turn on iswitchb to change buffers with [C-x b]
(iswitchb-mode t)

(global-set-key (kbd "C-x C-b") 'iswitchb-buffer)
(setq iswitchb-buffer-ignore (append
                               '("\\*Completions\\*" "\\*Messages\\*"
                                 "\\*epc con"        "\\*Minibuf-"
                                 "*epc:server:"      "\\*code-conversion-work\\*"
                                 "\\*Echo Area"      "\\*VC-Git\\* tmp status-"   "\\*vc\\*")
                               iswitchb-buffer-ignore))
; Allow keys to work like dzen because I'm lazy
(defun iswitchb-local-keys ()
  (mapc
    (lambda (K)
      (let* ((key (car K)) (fun (cdr K)))
        (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
    '(("<right>" . iswitchb-next-match) ("<left>"  . iswitchb-prev-match)
      ("<next>"  . iswitchb-next-match) ("<prior>" . iswitchb-prev-match)
      ("<up>"    . ignore)              ("<down>"  . ignore))))
(defun ido-local-keys ()
  (mapc
    (lambda (K)
      (let* ((key (car K)) (fun (cdr K)))
        (define-key ido-completion-map (edmacro-parse-keys key) fun)))
    '(("<right>" . ido-next-match) ("<left>"  . ido-prev-match)
      ("<next>"  . ido-next-match) ("<prior>" . ido-prev-match)
      ("<up>"    . ignore)         ("<down>"  . ignore))))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)
(add-hook                'ido-setup-hook      'ido-local-keys)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Custom screen splitting functions ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vsplit-last-buffer ()
  (interactive)

  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))
(defun hsplit-last-buffer ()
  (interactive)

  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

  ;; Display Shit
(defun switch-fullscreen nil
  (interactive)

  (let* ((modes '(nil fullboth))
         (cm    (cdr (assoc 'fullscreen (frame-parameters))))
	 (nl    (if cm
		    1
		  -1))
         (next  (cadr (member cm modes))))
    (menu-bar-mode nl)
    (tool-bar-mode nl)
    (modify-frame-parameters
      (selected-frame)
      (list (cons 'fullscreen next)))))

(define-key global-map [f11] 'switch-fullscreen)

;; Theme Shit
(defun on-after-init ()
  (when (display-graphic-p (selected-frame))
    (load-theme 'klere2 t)))
(add-hook 'window-setup-hook 'on-after-init)

;; Tramp Shit
(require 'tramp)

(setq tramp-default-method "scp")
(setq recentf-auto-cleanup 'never)
(defadvice save-buffer (around save-buffer-as-root-around activate)
  "Use sudo to save the current buffer."
  (interactive "p")

  (if (and (buffer-file-name) (not (file-writable-p (buffer-file-name))))
      (let ((buffer-file-name (format "/sudo::%s" buffer-file-name)))
	ad-do-it)
    ad-do-it))

(add-to-list 'tramp-connection-properties      (list
                                                 (regexp-quote "192.168.1.109")
                                                 "remote-shell"
                                                 "sh"))
;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(add-to-list 'tramp-remote-path                "/system/xbin")
(add-to-list 'tramp-remote-process-environment "TMPDIR=$HOME")

;; Coding Shit?
(with-eval-after-load "smart-tabs-mode.el"
  (smart-tabs-add-language-support lua lua-mode-hook
    ((lua-indent-line . lua-indent-level)))
  (smart-tabs-add-language-support ceylon ceylon-mode-hook
    ((ceylon-indent-line   . tab-width)
     (ceylon-format-region . tab-width)))
  (smart-tabs-insinuate 'c 'c++ 'java 'python 'ruby 'javascript 'ceylon ;; 'lua
                        ))

  ;; Scheme Shit
(put 'if 'scheme-indent-function 2)
(setq scheme-program-name "guile")
(global-set-key (kbd "C-M-r") 'run-scheme)
(add-hook 'scheme-mode-hook (lambda ()
                              (setq indent-tabs-mode nil)))

  ;; Text Shit
(require 'text-reading-mode)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'visual-line-mode)
(defun my-scratch-flyspell-mode ()
  (interactive)

  (with-current-buffer "*scratch*"
    (flyspell-mode 1)))
(add-hook 'after-init-hook 'my-scratch-flyspell-mode)
