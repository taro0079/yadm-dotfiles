(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
	  (expand-file-name
	   (file-name-directory (or load-file-name byte-compile-current-file))))))
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
		       ("melpa" . "https://melpa.org/packages/")
		       ("org" . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))
  (leaf leaf-keywords
	:ensure t
	:init
	(leaf hydra :ensure t)
	(leaf el-get :ensure t)
	(leaf blackout :ensure t)

	:config
	(leaf-keywords-init)))
(leaf lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
	 (php-mode . lsp)
	 (lsp-mode . lsp-enabmel-which-key-integration))
  :commands lsp)
(leaf lsp-ui :commands lsp-ui-mode)
(leaf lsp-ivy :commands lsp-ivy-workspace-symbol)
(leaf lsp-treemacs :commands lsp-treemacs-errors-list)

(leaf which-key :config (which-key-mode))
(leaf company
  :ensure t
  :leaf-defer nil
  :blackout t)

(leaf flycheck
  :ensure t
  :bind (("M-n" . flycheck-next-error)
	 ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode)

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(which-key treemacs lsp-ivy company lsp-ui blackout el-get hydra leaf-keywords)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
