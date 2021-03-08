(require 'flycheck)
(defvar gcc-checker-py)

;;; Code:

(flycheck-define-checker gcc-checker
  "Compile with compile_file.py wrapper to g++"
  :command ("python3" (eval gcc-checker-py) source-original)
  :standard-input nil
  :error-patterns
    ((info line-start (or "<stdin>" (file-name))
         ":" line (optional ":" column)
         ": note: " (message) line-end)
   (warning line-start (or "<stdin>" (file-name))
            ":" line (optional ":" column)
            ": warning: " (message (one-or-more (not (any "\n["))))
            (optional "[" (id (one-or-more not-newline)) "]") line-end)
   (error line-start (or "<stdin>" (file-name))
          ":" line (optional ":" column)
          ": " (or "fatal error" "error") ": " (message) line-end))
  :modes (c++-mode)
  )

;;; c-guidelines.el ends here
