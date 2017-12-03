;; Loadpaths
(add-to-list 'load-path              "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Emacs Shit
(setq column-number-mode t)

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
(mapc
  'require-package
  (car (read-from-string (with-temp-buffer
                           (insert-file-contents packagesList)
                           (buffer-string)))))

;; (package-initialize)

;; (defun package-list-updated-packages ()
;;   (package-refresh-contents)
;;   (package-list-packages))

;; Eshell Shit
(setq shell-file-name    "bash")

;; Theme Shit
(defun on-after-init ()
  (when (display-graphic-p (selected-frame))
    (load-theme 'klere2 t)))
(add-hook 'window-setup-hook 'on-after-init)


