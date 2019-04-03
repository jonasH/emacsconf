;; Put in .emacs (add-hook 'after-init-hook (lambda () (load "~/.emacs.d/elisp/real-init.el")))
;; Requirements
;; autopair
;; flycheck
;; yasnippet
;; browse-kill-ring
;; magit
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(load "~/.emacs.d/elisp/functions.el")
(load "~/.emacs.d/elisp/highlight-selection.el")
(highlight-selection-mode 1)
(add-to-list 'load-path "~/.emacs.d/elisp")
(autopair-global-mode 1)
(setq autopair-autowrap 1)

(autoload 'find-file-in-tags "~/.emacs.d/elisp/find-files-in-tags.el")
(global-set-key (read-kbd-macro "C-,") 'find-file-in-tags)

(global-flycheck-mode 1)
(setq flycheck-check-syntax-automatically '(mode-enabled save))

(require 'windmove)
(windmove-default-keybindings 'meta)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(add-hook 'org-mode-hook
	  (lambda ()
	    (local-unset-key (kbd "C-,"))
	    (local-unset-key (kbd "M-<up>"))
	    (local-unset-key (kbd "M-<down>"))
	    (local-unset-key (kbd "M-<left>"))
	    (local-unset-key (kbd "M-<right>"))
	    ))


(global-set-key (kbd "C-x 2") 'my/vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'my/hsplit-last-buffer)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
;;magit
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "M-<f1>") 'magit-status)

(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))
(global-set-key (kbd "M-y") 'browse-kill-ring)

(require 'linum)
(global-linum-mode 1)

(yas-global-mode 1)
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f12>") 'kill-this-buffer)
(global-set-key (kbd "<f8>") 'gud-cont)
(global-set-key (kbd "<f7>") 'gud-next)
(global-set-key (kbd "<f6>") 'gud-step)
(global-unset-key (kbd "C-v"))
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-<next>") 'super-duplicate)
(global-set-key (kbd "C-w") 'super-delete)
(global-set-key (kbd "C-s") 'super-isearch-forward)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "<f11>") 'mc/edit-lines)
(global-set-key (kbd "C-<f11>") 'mc/mark-all-like-this)
(global-set-key (kbd "<f9>") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<f9>") 'mc/skip-to-next-like-this)
(global-set-key (kbd "<f10>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-<f10>") 'mc/skip-to-previous-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)
(global-set-key (kbd "C--") (make-hippie-expand-function
			     '(try-expand-dabbrev
			       try-expand-dabbrev-all-buffers) t))
(global-set-key (kbd "C-å") 'revert-buffer)
(global-set-key (kbd "<f2>") 'flycheck-next-error)


(defun py-extract-variable (var_name)
  (interactive "sVar Name: ")
  (when (region-active-p)
	(kill-region (region-beginning) (region-end))
	(insert var_name)
	(python-nav-end-of-statement 1)
	(forward-char 1)
	(python-nav-backward-statement 1)
	(move-beginning-of-line 1)
	(open-line 1)
	(indent-according-to-mode)
	(insert (format "%s = " var_name))
	(yank)
      )
  )

(defun py-extract-function (var_name)
  (interactive "sVar Name: ")
  (when (region-active-p)
    (kill-region (region-beginning) (region-end))
    (indent-according-to-mode)
    (insert (format "%s()" var_name))
    (beginning-of-defun 1)
    (open-line 2)
    (indent-according-to-mode)
    (insert (format "def %s():\n" var_name))
    (yank)
    )
  )

(defun py-copy-to-import ()
  (interactive)
  (when (region-active-p)
    (kill-ring-save (region-beginning) (region-end))
    (beginning-of-buffer)
    (open-line 1)
    (insert "import ")
    (yank)
    (set-mark-command -1)
    (pop-global-mark)))


(defun py-ev-string ()
  (interactive)
  (when (region-active-p)
    (let ((reg (buffer-substring (region-beginning) (region-end))))
      (python-shell-send-string reg))))

(defun py-run-current-buffer()
  (interactive)
  (let ((script-buffer (buffer-file-name (current-buffer))))
    (other-window 1)
    (pdb (concat "python " script-buffer))
    (other-window -1)))

(add-hook 'python-mode-hook
	  (lambda () (local-set-key (kbd "C-c C-e") 'py-extract-variable)))
;; (add-hook 'python-mode-hook
;; 	  (lambda () (local-set-key (kbd "<f6>") 'py-run-current-buffer)))
(add-hook 'python-mode-hook
	  (lambda () (local-set-key (kbd "C-c i") 'py-copy-to-import)))



(fset 'py-swap-assignment
   [?\C-a C-right C-left ?\C-  ?\C-s ?= left left ?\C-x ?\C-x ?\C-w C-right C-left ?\C-  end ?\C-w left left left ?\C-y end ?\M-2 ?\C-y])

(add-hook 'python-mode-hook
	  (lambda () (local-set-key (kbd "C-c C-w") 'py-swap-assignment)))

(defun kill-trailing-whitespace ()
     (interactive)
     (query-replace-regexp " +$" ""))

(fset 'kill-whitespace-rows
   [?\M-< ?\C-\M-% ?^ ?  ?+ ?$ return return ?!])

(fset 'camel_to_lower
   [?\C-\M-s ?\[ ?A ?- ?Z ?\] left ?_ ?\M-l])


(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))


(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(tool-bar-mode -1)
(column-number-mode 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 2) ;; keyboard scroll one line at a time
(scroll-bar-mode 0)
(menu-bar-mode 0)
