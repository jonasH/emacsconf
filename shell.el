(add-hook 'term-mode-hook 'compilation-shell-minor-mode)
(add-hook 'term-mode-hook '(lambda ()
                             (modify-syntax-entry ?. "w")
                             (modify-syntax-entry ?/ "w")
                             (modify-syntax-entry ?- "w")
                             ))
(add-hook 'term-mode-hook  (lambda ()
	                     (define-key (current-local-map) (kbd "C-c C-y") 'term-paste)))
