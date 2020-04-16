(require 'flycheck)
(defvar coding-guidelines-py)

;;; Code:

(flycheck-define-checker c-guidelines
  "Veoneer C Coding Guidlines"
  :command ("python" (eval coding-guidelines-py) source-original)
  :standard-input nil
  :error-patterns
  (
   (warning line-start (file-name) ":" line ": warning: Coding Guidelines: "
            (message) line-end)
   )
  :modes (c-mode)
  )

;;; c-guidelines.el ends here
