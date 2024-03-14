(set-frame-font "UbuntuMono Nerd Font-18" nil t)
(global-set-key "\C-cm" 'set-mark-command)
(line-number-mode 1)
(global-display-line-numbers-mode)
(tool-bar-mode 0)
(with-eval-after-load "eglot"
  (add-to-list 'eglot-server-programs '(php-mode "intelephense" "--stdio")))

(defun finder-current-dir-open()
  (interactive)
  (shell-command "open ."))

(defun yadm-add()
  (interactive)
  (shell-command "yadm add -u"))

(defun my-php-mode-setup ()
  "My PHP-mode hook."
  (interactive)
  (require 'flycheck-phpstan)
  (flycheck-mode t)
  (flycheck-select-checker 'phpstan)
  (setq indent-tabs-mode nil
	tab-width 4
	c-basic-offset 4))

(add-hook 'php-mode-hook 'my-php-mode-setup)


(add-hook 'php-mode-hook 'my-php-mode-setup)
(global-auto-revert-mode t)

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

(leaf tree-sitter
  :ensure t
  :config
  (leaf tree-sitter-langs
    :ensure t)
  (tree-sitter-require 'php)
  :hook
  (php-mode-hook . tree-sitter-mode))

(leaf org-bullets
  :ensure t
  :hook
  (org-mode-hook . (lambda () (org-bullets-mode t))))

(leaf eglot
  :ensure t
  :require t
  :hook
  (php-mode-hook . eglot-ensure))

(leaf which-key
  :ensure t
  :config (which-key-mode))

(leaf company
  :ensure t
  :leaf-defer nil
  :blackout t
  :hook ((after-init-hook . global-company-mode)))

(leaf skk
  :ensure ddskk
  :custom ((default-input-method . "japanese-skk"))
  :config
  (leaf ddskk-posframe
    :ensure t
    :global-minor-mode t))
(global-set-key "\C-x\C-j" 'skk-mode)

(leaf flycheck
:ensure t
:bind (("M-n" . flycheck-next-error)
	 ("M-p" . flycheck-previous-error))
:global-minor-mode global-flycheck-mode)

(leaf helm
  :ensure t
  :require t
  :bind ("\C-c h" . helm-for-files))

(leaf web-mode
  :ensure t)

(leaf yaml-mode
  :ensure t)

(leaf projectile
  :ensure t
  :config
  (when (require 'projectile nil t)
    (projectile-mode)
    (add-to-list
     'projectile-globally-ignored-directories
     "node_modules")
    (setq projectile-enable-caching t))
  )
;;  Color scheme
(leaf doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold)
  (doom-themes-enable-italic)
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  :custom-face
  (doom-modeline-bar '((t (:background "#6272a4")))))
;; (leaf srcery-theme
;;   :ensure t
;;   :config
;;   (load-theme 'srcery t))
;; 
(leaf magit
  :ensure t)

(leaf git-gutter
  :ensure t
  :custom
  (git-gutter:modified-sign . "~")
  (git-gutter:added-sign . "+")
  (git-gutter:deleted-sign . "-")
  :custom-face
  (git-gutter:modified . '((t (:foreground "#404040" :background "#c0fc7f"))))
  (git-gutter:added    . '((t (:foreground "#108a3b" :background "#50fc7f"))))
  (git-gutter:deleted  . '((t (:foreground "#8f2986" :background "#ff79c6"))))
  :global-minor-mode global-git-gutter-mode)



(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")))
 '(package-selected-packages
   '(flycheck-phpstan lsp-mode php-mode magit which-key treemacs lsp-ivy company lsp-ui blackout el-get hydra leaf-keywords)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
