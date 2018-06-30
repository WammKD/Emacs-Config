(require 'parse-csv)

(setq file-as-list (parse-csv-string-rows (buffer-string)       (string-to-char ",")
                                          (string-to-char "\"") "\n"))

(defun csv-align-document ()
  "Shitter"

  (interactive)

  (save-excursion
    (save-restriction
      "Shitter")))
