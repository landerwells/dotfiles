(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(direnv))
 '(safe-local-variable-values
   '((eval setq-local display-buffer-alist
           '(("^sicp$" (display-buffer-in-side-window) (side . bottom)
              (window-height . 0.25))))
     (eval set-popup-rule! "^sicp$" :side bottom :size 0.25)
     (eval set-repl-handler! 'scheme-mode #'run-scheme)
     (eval set-repl-handler! 'org-mode (lambda nil (run-geiser 'racket)))
     (eval set-repl-handler! 'org-mode #'run-geiser)
     (eval set-popup-rule! "^\\*sicp\\*$" :side 'bottom :size 0.25 :select nil
           :quit nil :ttl 0)
     (eval set-repl-handler! 'org-mode #'scheme)
     (eval set-repl-handler! 'org-mode #'racket)
     (eval set-repl-handler! 'org-mode #'geiser-racket)
     (eval set-repl-handler! 'org-mode #'geiser))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
