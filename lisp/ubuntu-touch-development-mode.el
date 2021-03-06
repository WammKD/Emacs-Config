(defconst ut-compile-buffer        "*Ubuntu Touch Program*"
  "The name of the buffer where output from compiling or running a Ubuntu Touch project goes.")
(defconst ut-logs-buffer           "*Ubuntu Touch Logs*"
  "The name of the buffer where output from the logs of the running Ubuntu Touch app. goes.")
(setq     same-window-buffer-names (cons
                                     ut-logs-buffer
                                     (cons
                                       ut-compile-buffer
                                       same-window-buffer-names)))

(defun ut-helper~buffer-size ()
  (floor (* .75 (window-height))))
(defun ut-helper~find-file-directory (fileRegexp directory)
  (if (delq nil (mapcar
                  (lambda (file)
                    (string-match fileRegexp file))
                  (cddr (directory-files directory))))
      directory
    (ut-helper~find-file-directory
      fileRegexp
      (concat directory (if (string-equal (substring directory -1) "/")
                            ""
                          "/")                                          ".."))))

(defun ut-start-compile (command winHeight buffer errorMessage)
  (if (get-buffer-process buffer)
      (progn
        (message errorMessage)
        (get-buffer-process buffer))
    (progn
      (setq buffWin (get-buffer-window buffer))
      (setq origWin (get-buffer-window))

      (if buffWin
          (select-window buffWin)
        (split-window-vertically winHeight)
        (other-window 1))

      (async-shell-command command buffer)

      (if buffWin
          (select-window origWin)
        (other-window -1))

      (get-buffer-process buffer))))

(defun ut-create-project ()
  (interactive)

  (setq appname (read-string         (concat
                                       "Appname (make sure this is the "
                                       "same as you give to clickable): "))
        path    (read-directory-name (concat
                                       "In which directory would you like to "
                                       "create your new Ubuntu Touch app.: ")))
  (defun sentinel (p e)
    (split-window-vertically (ut-helper~buffer-size))

    (find-file
      (concat path (if (string-equal (substring path -1) "/") "" "/") appname)))

  (set-process-sentinel
    (ut-start-compile
      (concat "cd " path "; clickable init")
      (ut-helper~buffer-size)
      ut-compile-buffer
      "An Ubuntu Touch process is currently running!")
    'sentinel)

  (delete-window))

(defun ut-compile-project ()
  (interactive)

  (ut-start-compile
    (concat "cd " (ut-helper~find-file-directory
                    "^manifest.json\\(\\.in\\)*$"
                    default-directory)             "; clickable")
    (ut-helper~buffer-size)
    ut-compile-buffer
    "An Ubuntu Touch process is currently running!"))

(defun ut-log-project ()
  (interactive)

  (ut-start-compile
    (concat "cd " (ut-helper~find-file-directory
                    "^manifest.json\\(\\.in\\)*$"
                    default-directory)             "; clickable logs")
    (ut-helper~buffer-size)
    ut-logs-buffer
    "An Ubuntu Touch process is currently running!"))
