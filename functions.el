;;; Code:
(defun python-run ()
  (interactive)
    (pdb (format "python %s" (dired-get-filename) )))


(defun super-isearch-forward ()
  (interactive)
  (if (region-active-p)
      (search-selection (region-beginning) (region-end))
    (isearch-forward)
  ))

(defun search-selection (beg end)
      "search for selected text"
      (let (
            (selection (buffer-substring-no-properties beg end))
           )
        (deactivate-mark)
        (isearch-mode t nil nil nil)
        (isearch-yank-string selection)
      )
      )

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank)
  )

(defun duplicate-region ()
  (interactive)
  (kill-region (region-beginning) (region-end))
  (yank)
  (yank)
  )

(defun super-duplicate ()
  (interactive)
  (if (region-active-p)
      (duplicate-region)
    (duplicate-line))
  )

(defun super-delete ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1) 
    ))


(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    ((> arg 0) ;; rad ner
     (beginning-of-line)
     (forward-line)
     (transpose-lines 1)
     (forward-line -1))
    ((and (< arg 0) (> (line-number-at-pos) 1))
     (beginning-of-line)
     (transpose-lines 1)
     (forward-line -2))
    ))


(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))


(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )


(defun my/vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (if (= prefix 1)
    (switch-to-next-buffer)))


(defun my/hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (if (= prefix 1) (switch-to-next-buffer)))


(defun move-word-backward()
  (interactive)
  (transpose-words -1)
  (backward-word 2)
  (forward-char))
