;; Loadpaths
(add-to-list 'load-path              "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Packages
(require 'package)

(let* ((http     (if (and
                       (memq system-type '(windows-nt ms-dos))
                       (not (gnutls-available-p))) "http" "https"))
       (melpaURL (concat http "://melpa.org/packages/"))
       (gnuURL   (concat http "://elpa.gnu.org/packages/")))
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

;; (auto-package-update-maybe)

(global-set-key (kbd "C-x p l") 'list-packages)

;; Emacs Shit
(load-file "~/.emacs.d/lisp/cursors.el")
(delete-selection-mode t)
(add-hook      'after-init-hook (lambda ()
                                  (when (and
                                          (string= "*scratch*" (buffer-name))
                                          (not (buffer-file-name)))
                                    (display-splash-screen))))
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq indent-tabs-mode nil)))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string "\\(^[[:space:]\\n]*\\|[[:space:]\\n]*$\\)" "" str))
  ;; Wrap highlighted text in pair
(defun insert-square-brackets (&optional arg)
  "Enclose following ARG sexps in square brackets.
Leave point after open-bracket."
  (interactive "*P")

  (insert-pair arg ?\[ ?\]))
(defun insert-curly-braces (&optional arg)
  "Enclose following ARG sexps in curly braces.
Leave point after open-brace."
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
(defun browse-url-qutebrowser (url &optional new-window)
  "Open URL in Qutebrowser"
  (interactive (browse-url-interactive-arg "URL: "))

  (start-process (concat "qutebrowser " url) nil "qutebrowser" url))

(unless (display-graphic-p)
  (global-set-key (kbd "M-[ z") (kbd "<backtab>")))
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
(global-set-key (kbd "M-'")        'insert-pair)
(global-set-key (kbd "M-\-")       'abbrev-prefix-mark)
(global-set-key (kbd "C-x M-r")    'revert-buffer-no-confirm)
(global-set-key [f7]               'ispell)

(setq browse-url-browser-function     'browse-url-qutebrowser)
(setq column-number-mode              t)
(setq scroll-preserve-screen-position t)
(setq backup-directory-alist          '(("." . "~/.emacs.d/backup")))
(setq version-control                 t)   ; Use version numbers on backups
(setq kept-new-versions               20)  ; how many of the newest versions to keep
(setq kept-old-versions               5)   ; and how many of the old

(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

  ;; Window Shit
(require 'window-size)

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
(setq shell-file-name "bash")

  ;; Buffers Shit
; Turn on ido to change buffers with [C-x b]
(ido-mode t)

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(setq ido-ignore-buffers (append
                           '("\\*Completions\\*" "\\*Messages\\*"
                             "\\*epc con"        "\\*Minibuf-"
                             "*epc:server:"      "\\*code-conversion-work\\*"
                             "\\*Echo Area"      "\\*VC-Git\\* tmp status-"   "\\*vc\\*")
                           ido-ignore-buffers))
(set-face-attribute 'ido-first-match nil :inherit 'font-lock-function-name-face)
(define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)
(define-key (cdr ido-minor-mode-map-entry) [remap  find-file] nil)

; Allow keys to work like dzen because I'm lazy
(defun ido-local-keys ()
  (mapc
    (lambda (K)
      (let* ((key (car K)) (fun (cdr K)))
        (define-key ido-completion-map (edmacro-parse-keys key) fun)))
    '(("<right>" . ido-next-match) ("<left>"  . ido-prev-match)
      ("<next>"  . ido-next-match) ("<prior>" . ido-prev-match)
      ("<up>"    . ignore)         ("<down>"  . ignore))))
(add-hook 'ido-setup-hook 'ido-local-keys)

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
         (nl    (if cm 1 -1))
         (next  (cadr (member cm modes))))
    (menu-bar-mode nl)
    (tool-bar-mode nl)
    (modify-frame-parameters
      (selected-frame)
      (list (cons 'fullscreen next)))))

(define-key global-map [f11] 'switch-fullscreen)

  ;; Games Shit
    ;; Tetris Shit
(setq tetris-score-file "~/.emacs.d/game_scores/tetris-scores")
    ;; Snake Shit
(setq  snake-score-file "~/.emacs.d/game_scores/snake-scores")

;; Theme Shit
; Mode-line
(add-hook 'after-init-hook
  (lambda ()
    (when (display-graphic-p)
      (require 'all-the-icons)

      (defun custom-modeline-modified ()
        (let* ((config-alist '(("*" all-the-icons-faicon-family
                                    all-the-icons-faicon
                                    "chain-broken"
                                    :height    1.2
                                    :v-adjust -0.0)
                               ("-" all-the-icons-faicon-family
                                    all-the-icons-faicon
                                    "link"
                                    :height    1.2
                                    :v-adjust -0.0)
                               ("%" all-the-icons-octicon-family
                                    all-the-icons-octicon
                                    "lock"
                                    :height    1.2
                                    :v-adjust  0.1)))
               (result       (cdr (assoc
                                    (format-mode-line "%*")
                                    config-alist))))
          (propertize (apply (cadr result) (cddr result))
                      'face       `(:family ,(funcall (car result)))
                      'mouse-face `mode-line-highlight
                      'local-map  (if (string= "%" (format-mode-line "%*"))
                                      `(keymap (mode-line keymap (mouse-1 . mode-line-toggle-read-only)))
                                    `(keymap (mode-line keymap (mouse-1 . mode-line-toggle-modified))))
                      'help-echo  (if (string= "%" (format-mode-line "%*"))
                                      `mode-line-read-only-help-echo
                                    `mode-line-modified-help-echo))))

      (defun custom-modeline-region-info ()
        (let ((words (count-words (point-min) (point-max))))
          (concat
            (propertize (format "  %s words " words)
                        'face `(:background "#007bb3" :height 0.9))
            (propertize (format "  %s " (all-the-icons-octicon "pencil") words)
                        'face `(:family ,(all-the-icons-octicon-family) :background "#007bb3")
                        'display '(raise -0.0))
            (propertize " %l:"
                        'face `(:background "#007bb3" :height 0.9))
            (propertize "%c"
                        'face (if (> (current-column) 80)
                                  `(:foreground "#101010" :background "#007bb3" :height 0.9)
                                `(:background "#007bb3" :height 0.9)))
            (propertize " "
                        'face `(:background "#007bb3" :height 0.9))
            (propertize "%p"
                        'help-echo  "Size indication mode
mouse-1: Display Line and Column Mode Menu"
                        'mouse-face `mode-line-highlight
                        'local-map  `(keymap
                                       (mode-line
                                         keymap
                                         (down-mouse-1
                                           keymap
                                           (column-number-mode
                                             menu-item
                                             "Display Column Numbers"
                                             column-number-mode
                                             :help   "Toggle displaying column numbers in the mode-line"
                                             :button (:toggle . column-number-mode))
                                           (line-number-mode
                                             menu-item
                                             "Display Line Numbers"
                                             line-number-mode
                                             :help   "Toggle displaying line numbers in the mode-line"
                                             :button (:toggle . line-number-mode))
                                           (size-indication-mode
                                             menu-item
                                             "Display Size Indication"
                                             size-indication-mode
                                             :help   "Toggle displaying a size indication in the mode-line"
                                             :button (:toggle . size-indication-mode))
                                           "Toggle Line and Column Number Display")))
                        'face       `(:background "#007bb3" :height 0.9))
            (propertize "  "
                        'face `(:background "#007bb3" :height 0.9)))))
      ;; mode-line-position

      (defun -custom-modeline-github-vc ()
        (let ((branch (mapconcat 'concat (cdr (split-string
                                                vc-mode
                                                "[:-]")) "-")))
          (concat
            (propertize (format " %s" (all-the-icons-alltheicon "git"))
                        'face `(:height 0.9) 'display '(raise -0.1))
            " · "
            (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                        'face `(:height 1.0 :family ,(all-the-icons-octicon-family))
                        'display '(raise -0.1))
            (propertize (format " %s" branch) 'face `(:height 0.9)))))
      (defun -custom-modeline-svn-vc ()
        (let ((revision (cadr (split-string vc-mode "-"))))
          (concat
            (propertize (format " %s" (all-the-icons-faicon "cloud"))
                        'face `(:height 1.2) 'display '(raise -0.1))
            (propertize (format " · %s" revision)
                        'face `(:height 0.9)))))
      (defun custom-modeline-icon-vc ()
        (when vc-mode
          (cond
           ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
           ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
           (t (format "%s" vc-mode)))))

      (defun custom-modeline-file-icon ()
        (if (buffer-file-name)
            (concat
              (propertize (all-the-icons-icon-for-file (buffer-file-name))
                          'face       `(:bold t :foreground "#007bb3" :background nil :height 1.0)
                          'display    '(raise -0.1)
                          'local-map  `(keymap
                                         (header-line
                                           keymap
                                           (mouse-3      . mode-line-next-buffer)
                                           (down-mouse-3 . ignore)
                                           (mouse-1      . mode-line-previous-buffer)
                                           (down-mouse-1 . ignore))
                                         (mode-line
                                           keymap
                                           (mouse-3 . mode-line-next-buffer)
                                           (mouse-1 . mode-line-previous-buffer)))
                          'mouse-face `mode-line-highlight
                          'help-echo  "Buffer name
mouse-1: Previous buffer
mouse-3: Next buffer")
              " ")
          ""))

      (setq-default mode-line-format '("%e" mode-line-front-space
                                            ;; mode-line-mule-info
                                            mode-line-client
                                            (:eval (custom-modeline-modified))
                                            mode-line-frame-identification
                                            (:eval (custom-modeline-file-icon))
                                            mode-line-buffer-identification
                                            "  "
                                            (:eval (custom-modeline-region-info))
                                            "  "
                                            (:eval (custom-modeline-icon-vc))
                                            "  "
                                            mode-line-modes
                                            mode-line-misc-info
                                            mode-line-end-spaces)))))

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
(require 'smart-tabs-mode)

;; (smart-tabs-add-language-support web    web-mode-hook
;;   ((web-mode-indent-line . web-mode-code-indent-offset)))
(smart-tabs-add-language-support bash   sh-mode-hook
  ((sh-indent-line       . sh-basic-offset)
   (sh-basic-indent-line . sh-basic-offset)))
(smart-tabs-add-language-support lua    lua-mode-hook
  ((lua-indent-line . lua-indent-level)))
(smart-tabs-add-language-support ceylon ceylon-mode-hook
  ((ceylon-indent-line   . tab-width)
   (ceylon-format-region . tab-width)))
(smart-tabs-add-language-support scala  scala-mode-hook
  ((scala-indent:indent-line . scala-indent:step)))
(smart-tabs-insinuate 'c 'c++ 'java 'python 'ruby 'javascript 'ceylon 'scala ;; 'lua
                      )

  ;; Scheme Shit
(put 'if 'scheme-indent-function 2)

(setq scheme-program-name "guile")
(defun run-guile ()
  "Run Guile via Geiser, input and output via buffer `* Guile REPL *'.
If there is a process already running in `* Guile REPL *', switch to that buffer."
  (interactive)

  (geiser 'guile))

(global-set-key (kbd "C-M-r") 'run-guile)
(add-hook 'scheme-mode-hook (lambda ()
                              (setq indent-tabs-mode nil)
                              (auto-complete-mode t)
                              (ac-geiser-setup)))

  ;; Text Shit
(require 'text-reading-mode)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'visual-line-mode)
(defun my-scratch-flyspell-mode ()
  (interactive)

  (with-current-buffer "*scratch*"
    (flyspell-mode 1)))
(add-hook 'after-init-hook 'my-scratch-flyspell-mode)

    ;; LaTeX Shit
; Make sure to install auctex for any of this to work
(defun LaTeX-insert-pair (&optional arg)
  "Enclose following ARG sexps in LaTeX quotations (`` and '').
Leave point after open-paren."
  (interactive "*P")

  (if (not mark-active)
      (progn
        (insert-pair arg ?\` ?\')
        (insert "`'")
        (backward-char))
    (progn
      (setq re (region-end))
      (setq rb (region-beginning))

      (insert-pair arg ?\` ?\')

      (goto-char (1+ rb))
      (insert "`")

      (goto-char (+ 2 re))
      (insert "'")
      (backward-char))))
(defun my-LaTeX-mode ()
  (setq TeX-auto-save  t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  (setq TeX-PDF-mode   t)

  (add-to-list 'TeX-view-program-list
               '("Xreader" "xreader --page-index=%(outpage) %o"))
  (setq TeX-view-program-selection '((output-pdf "Xreader"))))

(add-hook 'LaTeX-mode-hook (lambda ()
                             (local-set-key (kbd "M-\"") #'LaTeX-insert-pair)))
(add-hook 'LaTeX-mode-hook 'my-LaTeX-mode)

    ;; Word Shit
(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))

  ;; QML Shit
; I'm putting this here in event that I add a QML mode, again
;; (autoload 'qml-mode "qml-mode")
;; (add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))
;; (defun qml-mode-setup-folding ()
;;   (local-set-key (kbd "C-x w f") 'hs-hide-block)
;;   (local-set-key (kbd "C-x w s") 'hs-show-block)
;;   (local-set-key (kbd "C-x w i") 'qml-add-import)
;;   (local-set-key (kbd "C-M-SPC") 'qml-mark-defun)

;;   (setq-local js-indent-level 2)
;;   (setq-local css-indent-offset 2))
;; (add-hook 'qml-mode-hook 'qml-mode-setup-folding)

  ;; Web Shit
(add-to-list 'auto-mode-alist '("\\.js\\'"        . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml?\\'"      . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'"      . web-mode))

(eval-after-load 'web-mode
  (lambda ()
    ;; (setq web-mode-indent-style 2)

    (unless (display-graphic-p)
      (set-face-attribute 'web-mode-html-tag-face nil         :foreground "blue3"   :weight 'bold)
      (set-face-attribute 'web-mode-doctype-face nil          :foreground "#9FE55B" :weight 'bold)
      (set-face-attribute 'web-mode-html-attr-name-face nil   :foreground "purple3")
      (set-face-attribute 'web-mode-css-function-face nil     :foreground "purple3" :weight 'normal)
      (set-face-attribute 'web-mode-css-pseudo-class-face nil :foreground "cyan3"   :weight 'normal))

    (defun web-mode-setup-folding ()
      (let ((ext (file-name-extension (buffer-name))))
        (cond
         ((or
            (string-equal ext "css")
            (string-equal ext "php")) (setq-local comment-start "/* ")
                                      (setq-local comment-end   " */"))
         ((string-equal ext  "js")    (setq-local comment-start "// ")
                                      (setq-local comment-end   ""))
         ((or
            (string-equal ext "html")
            (string-equal ext "xml")) (setq-local comment-start "<!-- ")
                                      (setq-local comment-end   " -->"))))
      (hs-minor-mode t)

      (defun unnec1 ()
        (interactive)

        (ignore-errors (web-mode-fold-or-unfold))
        (hs-hide-block))
      (defun unnec2 ()
        (interactive)

        (ignore-errors (web-mode-fold-or-unfold))
        (hs-show-block))

      (local-set-key (kbd "C-x w f") 'unnec1)
      (local-set-key (kbd "C-x w s") 'unnec2))
    (add-hook 'web-mode-hook 'web-mode-setup-folding)

    (add-hook 'web-mode-hook
              (lambda ()
                (setq web-mode-enable-auto-closing  t
                      web-mode-style-padding        2
                      web-mode-markup-indent-offset 2
                      ;; web-mode-attr-indent-offset   2  ; Html attribute indentation level
                      web-mode-code-indent-offset   2
                      web-mode-css-indent-offset    2
                      ;; web-mode-sql-indent-offset    2  ; Sql (inside strings) indentation level
                      tab-width                     2
                      indent-tabs-mode              t)))))

  ;; Python Shit
(add-hook 'python-mode-hook (lambda ()
                              (setq python-indent-level 4
                                    tab-width           4
                                    indent-tabs-mode    t)))

  ;; Lua Shit
(add-to-list        'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua"     . lua-mode))
(eval-after-load 'lua-mode
  (lambda ()
    (add-hook 'lua-mode-hook (lambda ()
                               (setq lua-indent-level 3
                                     tab-width        3
                                     indent-tabs-mode t)))))

  ;; Bash Shit
(defun gker-setup-sh-mode ()
  "My own personal preferences for `sh-mode'.

This is a custom function that sets up the parameters I usually
prefer for `sh-mode'.  It is automatically added to
`sh-mode-hook', but is can also be called interactively."
  (interactive)

  (setq sh-basic-offset  2
        sh-indentation   2
        tab-width        2
        indent-tabs-mode t))
(add-hook 'sh-mode-hook 'gker-setup-sh-mode)

  ;; Ruby Shit
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
;; (add-hook    'enh-ruby-mode-hook 'ac-robe-setup)

(defun ruby-send-region-custom ()
  (interactive)
  (setq cb (current-buffer))
  (if (not (get-buffer "*ruby*"))
      (progn
        (inf-ruby)
        (switch-to-buffer-other-window cb)))
  (if (use-region-p)
      (ruby-send-region (region-beginning) (region-end))
    (ruby-send-region (line-beginning-position) (line-end-position))))

(add-hook 'enh-ruby-mode-hook (lambda ()
                                (local-set-key
                                  (kbd "C-x r e")
                                  #'ruby-send-region-custom)))
(add-hook 'enh-ruby-mode-hook (lambda ()
                                (local-set-key (kbd "C-x r q") #'inf-ruby)))
(add-hook 'enh-ruby-mode-hook (lambda ()
                                (setq ruby-indent-level 2
                                      tab-width         2
                                      indent-tabs-mode  t)))

  ;; Rust Shit
;; (require 'rusti)
;; (setq rusti-program "~/.cargo/bin/rust-repl")

(defun rust-new (exec? name)
  (interactive (list
                 (y-or-n-p    "Executable: ")
                 (read-string "Project name: ")))

  (delete-other-windows)

  (let ((height (window-height)))
    (cargo-process-new name exec?)

    (enlarge-window (- height (round (* height .7)))))
  (sleep-for 0 500)
  (find-file (concat default-directory name "/src/main.rs"))
  (tabify (point-min) (point-max))
  (save-buffer))
(global-set-key (kbd "C-x r n") #'rust-new)

(setq racer-cmd           "~/.cargo/bin/racer")
(setq racer-rust-src-path "~/.cargo/rust/src")

(add-hook 'rust-mode-hook     'cargo-minor-mode)
(add-hook 'rust-mode-hook     #'racer-mode)
(add-hook 'rust-mode-hook     #'eldoc-mode)
(add-hook 'rust-mode-hook     #'company-mode)
(add-hook 'rust-mode-hook     (lambda ()
                                (defun run-command (command)
                                  (delete-other-windows)
                                  (let ((height (window-height)))
                                    (funcall command)
                                    (enlarge-window
                                      (- height (round (* height .7))))))

                                (defun rustCompile ()
                                  (interactive)

                                  (run-command 'rust-compile))
                                (defun rustRun ()
                                  (interactive)

                                  (let ((name (buffer-name)))
                                    (run-command
                                      (lambda ()
                                        (async-shell-command
                                          (substring
                                            name
                                            0
                                            (- (length name) 3)))))))

                                (local-set-key (kbd "C-x r d") #'racer-describe)
                                (local-set-key (kbd "C-x r c") #'rustCompile)
                                (local-set-key (kbd "C-x r r") #'cargo-process-run)

                                (setq c-basic-offset   4
                                      tab-width        4
                                      indent-tabs-mode t)))
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

  ;; C/C++/Java Shit
(setq-default c-basic-offset 4)
(add-hook 'c-initialization-hook (lambda ()
                                   (setq c-basic-offset   4
                                         tab-width        4
                                         indent-tabs-mode t)))

  ;; JVM Shit
    ;; Java Shit
(eval-after-load 'javadoc-lookup
  (lambda ()
    (javadoc-add-roots "/usr/lib/jvm/java-8-openjdk-amd64/docs/api/")
    ;; (javadoc-add-roots "/usr/local/java/jdk1.8.0_45/api")
    ;; (javadoc-add-roots "/usr/local/java/jdk1.8.0_45/api-junit")
    ;; (javadoc-add-roots "/usr/local/java/jdk1.8.0_45/api-javaslang")
    ;; (javadoc-add-roots "/usr/local/java/jdk1.8.0_45/api-commonsCodec")
    ))
(global-set-key (kbd "C-h j") 'javadoc-lookup)
(add-hook 'java-mode-hook '(lambda ()
                             (define-key java-mode-map (kbd "C-x j i")
                               'javadoc-add-import)
                             (define-key java-mode-map (kbd "C-x j s")
                               'javadoc-sort-imports)))

    ;; Scala Shit
(add-hook 'scala-mode-hook
  (lambda ()
    (setq scala-indent:step 4
          tab-width         4
          indent-tabs-mode  t)))

    ;; Ceylon Shit
(defconst ceylon-compileRun-buffer   "*Ceylon Program*"
  "The name of the buffer where output from compiling or running a Ceylon file goes.")
(setq     same-window-buffer-names   (cons
                                       ceylon-compileRun-buffer
                                       same-window-buffer-names))
(defun    ceylon-helper~buffer-size  ()
  (floor (* .75 (window-height))))
(defun    ceylon-startCompile-manual (command win-height buffer error-message)
  (if (get-buffer-process buffer)
      (progn
        (message error-message)
        (get-buffer-process buffer))
    (progn
      (setq buff-win (get-buffer-window buffer))
      (setq orig-sts (not (equal buff-win (get-buffer-window))))

      (when (not buff-win)
        (split-window-vertically win-height))

      (when orig-sts (other-window 1))
      (async-shell-command command buffer)
      (when orig-sts (other-window -1))

      (get-buffer-process buffer))))
(defun    ceylon-compile-manual      ()
  (interactive)

  (let* ((source                  "source")
         (get-directory-file-name (lambda (dir)
                                    (directory-file-name (file-name-directory
                                                           dir))))
         (file-name               (buffer-file-name))
         (curr-dir                (funcall
                                    get-directory-file-name
                                    file-name)))
    (while (not (string-equal (file-name-nondirectory curr-dir) source))
      (setq curr-dir (funcall get-directory-file-name curr-dir)))

    (ceylon-startCompile-manual
      (concat "cd " curr-dir "/../; ceylon compile " file-name)
      (ceylon-helper~buffer-size)
      ceylon-compileRun-buffer
      "A Ceylon process is currently running!")))
(defun    ceylon-run-manual          (funct-or-class module)
  (interactive (list
                 (read-from-minibuffer
                   "Function or class name to run: " nil nil
                   nil                               nil (file-name-base))
                 (read-from-minibuffer
                   "Module to use: "                 nil nil
                   nil                               nil "default")))

  (let* ((source                  "source")
         (get-directory-file-name (lambda (dir)
                                    (directory-file-name (file-name-directory
                                                           dir))))
         (file-name               (buffer-file-name))
         (curr-dir                (funcall get-directory-file-name file-name)))
    (while (not (string-equal (file-name-nondirectory curr-dir) source))
      (setq curr-dir (funcall get-directory-file-name curr-dir)))

    (ceylon-startCompile-manual
      (concat "cd " curr-dir "/../; ceylon run --run " funct-or-class " " module)
      (ceylon-helper~buffer-size)
      ceylon-compileRun-buffer
      "A Ceylon process is currently running!")))
(defun    ceylon-run-module-manual   (module)
  (interactive (list (read-from-minibuffer "Module to use: " nil
                                           nil               nil
                                           nil               "default")))

  (let* ((source                  "source")
         (get-directory-file-name (lambda (dir)
                                    (directory-file-name (file-name-directory
                                                           dir))))
         (file-name               (buffer-file-name))
         (curr-dir                (funcall
                                    get-directory-file-name
                                    file-name)))
    (while (not (string-equal (file-name-nondirectory curr-dir) source))
      (setq curr-dir (funcall get-directory-file-name curr-dir)))

    (ceylon-startCompile-manual
      (concat "cd " curr-dir "/../; ceylon run " module)
      (ceylon-helper~buffer-size)
      ceylon-compileRun-buffer
      "A Ceylon process is currently running!")))
(add-hook 'ceylon-mode-hook (lambda ()
                              (setq electric-indent-chars
                                (append electric-indent-chars '(?})))))
(add-hook 'ceylon-mode-hook (lambda ()
                              (define-key ceylon-mode-map (kbd "C-x c c")
                                'ceylon-compile-manual)))
(add-hook 'ceylon-mode-hook (lambda ()
                              (define-key ceylon-mode-map (kbd "C-x c r")
                                'ceylon-run-manual)))
(add-hook 'ceylon-mode-hook (lambda ()
                              (define-key ceylon-mode-map (kbd "C-x c m")
                                'ceylon-run-module-manual)))

    ;; Android Shit
(setq android-mode-builder (quote gradle)
      android-mode-sdk-dir "~/Android/Sdk")
(defun android-global-create-project ()
  (interactive)

  (android-mode t)
  (call-interactively 'android-create-project))
(global-set-key (kbd "C-x a C-f") 'android-global-create-project)
(global-set-key (kbd "C-x a o s") 'android-logcat)

(eval-after-load 'android-mode
  (lambda ()
    (add-hook 'android-mode-hook (lambda ()
                                   (setq compile-command "gradle compileDebugAidl")

                                   (defun android-compile-project ()
                                     (interactive)

                                     (save-some-buffers)
                                     (shell-command (concat
                                                      compile-command
                                                      " "
                                                      (if (= (string-match "gradle_*" compile-command) 0)
                                                          "-p "
                                                        "")
                                                      (android-root)
                                                      " &") "*compilation*"))
                                   (defun android-gradle-build-apk ()
                                     (interactive)

                                     (save-some-buffers)
                                     (shell-command (concat
                                                      "gradle compileDebugAidl assembleRelease -p "
                                                      (android-root)
                                                      " &") "*compilation*"))
                                   (defun android-start-genymotion-emulator (device_name)
                                     (interactive "sAndroid Virtual Device Name: ")

                                     (if (executable-find "player")
                                         (call-process-shell-command
                                           (concat (executable-find "player") " --vm-name " device_name " &")
                                           nil
                                           0)
                                       (message
                                         "Genymotion or the corresponding command \"player\" is either not "
                                         "installed or not within your PATH.")))

                                   (local-set-key (kbd "C-x a c")     'android-compile-project)
                                   (local-set-key (kbd "C-x a b")     'android-gradle-build-apk)
                                                     ;; C-c a g
                                   (local-set-key (kbd "C-x a m")     'android-start-genymotion-emulator)
                                   (local-set-key (kbd "C-x a o f s") 'android-logcat-set-filter)
                                   (local-set-key (kbd "C-x a o f c") 'android-logcat-clear-filter)))))

(add-hook 'gud-mode-hook (lambda ()
                           (add-to-list
                             'gud-jdb-classpath
                             "~/Android/Sdk/platforms/android-24/android.jar")
                           (add-to-list
                             'gud-jdb-classpath
                             "~/Android/Sdk/platforms/android-26/android.jar")))

  ;; Version Control
(defun vc-next-action-new ()
  (interactive)
  (when (not (eq (get-buffer "*vc-dir*") (current-buffer)))
    (if (get-buffer "*vc-dir*")
        (progn
          (setq vc_buf
                (with-current-buffer "*vc-dir*"
                  (buffer-substring-no-properties (point-min) (point-max))))
          (kill-buffer "*vc-dir*")
          (vc-dir (substring vc_buf (+ (string-match-p
                                         "Working dir: "
                                         vc_buf) 13) (string-match-p
                                                       "\nBranch     : "
                                                       vc_buf))))
    (call-interactively 'vc-dir)))
  (when (string-equal (vc-next-action nil) "Fileset is up-to-date")
    (message "Pushing to Git")
    (shell-command "git push &" "*Git Push Output*")
    ;; (enlarge-window 20)
    (run-at-time "20 sec" nil (lambda ()
                                (setq buf_conts
                                      (with-current-buffer "*Git Push Output*"
                                        (buffer-substring-no-properties
                                          (point-min)
                                          (point-max))))
                                (setq buf_sub
                                      (substring
                                        buf_conts
                                        0
                                        (+ (string-match-p
                                             ": "
                                             (substring
                                               buf_conts
                                               0
                                               (string-match-p
                                                 "\n"
                                                 buf_conts))) 2)))
                                (when (not (string-equal buf_sub
                                        "Username for 'https://github.com': "))
                                  (kill-buffer "*Git Push Output*")
                                  (delete-other-windows))))))
(global-set-key (kbd "C-x v v") 'vc-next-action-new)
(put 'dired-find-alternate-file 'disabled nil)






(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t (:foreground "white"))))
 '(company-tooltip-selection ((t (:background "green" :foreground "black")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
