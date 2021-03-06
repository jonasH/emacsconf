;; Put in .emacs (add-hook 'after-init-hook (lambda () (load "~/.emacs.d/elisp/real-init.el")))
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
(show-paren-mode 1)

;; store all backup and autosave files in the tmp dir
(setq temporary-file-directory "~/Temp/emacs")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-always-ensure 't)
;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))


;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)
(set-default-coding-systems 'utf-8)
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; (use-package weyland-yutani-theme
;;   :config (load-theme 'weyland-yutani))
;; (add-hook 'term-mode-hook
;;           (lambda nil (color-theme-buffer-local 'color-theme-retro-orange (current-buffer))))

(load "~/.emacs.d/elisp/functions.el")
(load "~/.emacs.d/elisp/highlight-selection.el")
(highlight-selection-mode 1)
(require 'diminish)
(diminish 'highlight-selection)

(add-to-list 'load-path "~/.emacs.d/elisp")
(use-package autopair
  :diminish
  :init (autopair-global-mode 1)
  :config (setq autopair-autowrap 1)
  )



;; (autoload 'find-file-in-tags "~/.emacs.d/elisp/find-files-in-tags-linux.el")
;; (global-set-key (read-kbd-macro "C-,") 'find-file-in-tags)
(global-set-key (read-kbd-macro "C-,") 'project-find-file)
(require 'flycheck)

;;(defvar coding-guidelines-py "C:\\Users\\jonas.hesselryd\\Project\\CodingGuidelines\\Language\\C\\coding_guidelines\\coding_guidelines.py")
;(load "~/.emacs.d/elisp/c-guidelines.el")
;;(load "~/.emacs.d/elisp/c-tasking-checker.el")
;(add-to-list 'flycheck-checkers 'c-guidelines)
;; (add-to-list 'flycheck-checkers 'c-tasking)
;; (global-flycheck-mode 1)
(defvar gcc-checker-py "/home/ejonhes/scripts/compile_file.py")
;;(defvar tidy-checker-py "/home/ejonhes/scripts/tidy_file.py")
(load "~/.emacs.d/elisp/gcc-checker.el")
;;(load "~/.emacs.d/elisp/tidy-file.el")
(add-to-list 'flycheck-checkers 'gcc-checker)
(add-to-list 'flycheck-checkers 'tidy-checker)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
 (add-hook 'c++-mode-hook
           (lambda () (flycheck-mode 1)))
 (add-hook 'c++-mode-hook
           (lambda () (flycheck-select-checker 'gcc-checker)))

(use-package windmove
  :config  (windmove-default-keybindings 'meta)
  )

(use-package ace-window
  :bind (("M-o" . ace-window)))
(use-package ido
  :init (ido-mode 1)
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-separator "\n")
  )


(add-hook 'org-mode-hook
	  (lambda ()
	    (local-unset-key (kbd "C-,"))
	    (local-unset-key (kbd "M-<up>"))
	    (local-unset-key (kbd "M-<down>"))
	    (local-unset-key (kbd "M-<left>"))
	    (local-unset-key (kbd "M-<right>"))
	    ))



;; (use-package org-bullets
;;     :config
;;     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(let* ((variable-tuple
          (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

(use-package hl-todo
  :ensure t
  :diminish
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)))

(use-package protobuf-mode
  )

(global-set-key (kbd "C-x 2") 'my/vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'my/hsplit-last-buffer)

(use-package browse-kill-ring
  :bind (("M-y" . browse-kill-ring))
  :config (browse-kill-ring-default-keybindings)
  )
(use-package multiple-cursors
  :bind (("<f9>" . mc/mark-next-like-this)
         ("C-<f9>" . mc/skip-to-next-like-this)))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (add-to-list #'yas-snippet-dirs "~/.emacs.d/elisp/snippets")
  (yas-reload-all)
  )
(global-unset-key (kbd "M-z"))
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f12>") 'kill-this-buffer)
(global-set-key (kbd "<f8>") 'gud-cont)
(global-set-key (kbd "<f7>") 'gud-next)
(global-set-key (kbd "<f6>") 'gud-step)
(global-set-key (kbd "<f5>") 'jh-switch-to-terminal)
(global-unset-key (kbd "C-v"))
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-<next>") 'super-duplicate)
(global-set-key (kbd "C-w") 'super-delete)
(global-set-key (kbd "C-s") 'super-isearch-forward)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)
(global-set-key (kbd "<f2>") 'flycheck-next-error)
;; (add-hook 'python-mode-hook 'blacken-mode)

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

;; Add CodingGuidelines Flychecker
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
(add-hook 'python-mode-hook
          (lambda () (display-line-numbers-mode 1)))

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))
;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Use spaces instead of tab
(setq-default indent-tabs-mode nil)
   
(defvar c-default-style)
(defvar c-basic-offset)
(setq c-default-style "linux"
      c-basic-offset 4)



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


(when (executable-find "python3")
  (setq python-shell-interpreter "python3"))


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
