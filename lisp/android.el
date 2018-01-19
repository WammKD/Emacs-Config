(defun android-generate-keystore ()
  (interactive)

  (defun get-passwords (count)
    (setq firstPassword    (if (> count 0)
                               (read-passwd
                                 (string-join (list
                                                "Passwords didn't match! "
                                                "Enter keystore password: ")))
                             (read-passwd "Enter keystore password: "))
          repeatedPassword (read-passwd "Re-enter new password: ")
          countPlusOne     (1+ count))

    (cond
     ((= countPlusOne 3)                                              nil)
     ((string-equal
        firstPassword
        repeatedPassword) (list firstPassword "\n" repeatedPassword "\n"))
     (t                                      (get-passwords countPlusOne))))
  (defun get-rest (a p)
    (setq name      (read-string "What is your first and last name? ")
          orgUnit   (read-string
                      "What is the name of your organizational unit? ")
          org       (read-string "What is the name of your organization? ")
          city      (read-string "What is the name of your City or Locality? ")
          state     (read-string "What is the name of your State or Province? ")
          twoLetter (read-string
                      "What is the two-letter country code for this unit? "))

    (if (yes-or-no-p (string-join (list "Is CN=" name
                                        ", OU="  orgUnit
                                        ", O="   org
                                        ", L="   city
                                        ", ST="  state
                                        ", C="   twoLetter " correct? ")))
        (list
          name      "\n"
          orgUnit   "\n"
          org       "\n"
          city      "\n"
          state     "\n"
          twoLetter "\nyes\n"
          (read-string
            (string-join (list
                           "Enter key password for <"
                           a
                           "> (RETURN if same as keystore password): "))
            nil
            nil
            p)      "\n")
      (get-rest a p)))

  (setq filePath   (read-directory-name "Directory to create keystore in: ")
        fileName   (read-string
                     "Keystore filename (\"release.keystore\"): "
                     nil
                     nil
                     "release.keystore")
        alias      (read-string "Keystore alias (\"example\"): " nil
                                nil                              "example")
        keytoolCmd (string-join (list
                                  " | keytool -genkey -v -keystore "
                                  fileName
                                  " -alias "
                                  alias
                                  " -keyalg RSA -keysize "
                                  (read-string
                                    "Keystore size (\"2048\"): "
                                    nil
                                    nil
                                    "2048")
                                  " -validity "
                                  (read-string
                                    "Keystore validity (\"10000\"): "
                                    nil
                                    nil
                                    "10000")
                                  " -deststoretype pkcs12"))
        passwords  (get-passwords 0))

  (if passwords
      (progn
        (setq command (string-join (append
                                     (list "cd " filePath "; printf \"")
                                     passwords
                                     (get-rest alias (car passwords))
                                     (list "\"" keytoolCmd))))

        (split-window-vertically (- (window-height) 12))
        (set-window-buffer (next-window) (get-buffer-create
                                           "*Keystore Output*"))
        (shell-command command "*Keystore Output*"))
      (message "Mismatched passwords three times; aborting…")))
