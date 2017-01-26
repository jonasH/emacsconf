;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(require 'cl)

(setq linez '("        self.chart_toolbar = NavigationToolbar2Wx(self.canvas)"
"        self.canvas.mpl_connect('button_press_event', self.mouse_clicked)"
"        self.canvas.mpl_connect('button_release_event',"
"                                self.mouse_released)"
"        self.canvas.mpl_connect('motion_notify_event', self.mouse_moved)"))


(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (* (any " \t\n")))
			    ""
			    str))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))


(defun extract-parts-of-line (line)
  (let ((parts (split-string (chomp line) "(\\|=\\|)\\|,")))
    (remove-if (lambda (x) (string= x "")) parts)
  ))


(defun extract-variables-from-line (line)
  (let ((parts (extract-parts-of-line line)))
    (remove-if (lambda (x) (string-prefix-p "'" x))
	       (mapcar 'car (mapcar (lambda (x) (split-string x "\\.")) parts)))
    )
  )


(defun extract-variables-from-lines (lines)
  (delete-dups (flatten (mapcar 'extract-variables-from-line lines)))
  )


(defun extract-entire-function ()
  (python-nav-beginning-of-defun)
  (let ((start-position (point)))
    (progn
    (python-nav-end-of-block)
    (buffer-substring-no-properties start-position (point)))))

  
(mapcar 'concat '("a" "b"))
("a" "b")



(defun py-extract-method ()
  (interactive)
  (let ((extracted-variables (extract-variables-from-lines
			      (split-string (delete-and-extract-region 
			   (region-beginning) (region-end)) "\n")))
	(remaining-variables (extract-variables-from-lines
			      (cdr (split-string (extract-entire-function) "\n"))))
	)
    (message (mapcar 'concat extracted-variables))

  ))

