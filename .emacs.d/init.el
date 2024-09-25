(defun shell-other-window ()
  (interactive)
  (let ((buf (shell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

(setenv "LIBRARY_PATH" "/opt/homebrew/lib/gcc/14/:/opt/homebrew/lib/gcc/14/gcc/aarch64-apple-darwin23/14")
(exec-path-from-shell-initialize)
(add-to-list 'exec-path "/Users/taro_morita/.volta/bin")

(set-frame-font "0xProto Nerd Font-14" nil t)
(global-set-key "\C-cm" 'set-mark-command)
(global-set-key "\C-t" 'other-window)
(line-number-mode 1)
(global-display-line-numbers-mode)
(tool-bar-mode 0)
(with-eval-after-load "eglot"
  (add-to-list 'eglot-server-programs '(php-mode "intelephense" "--stdio"))
  (add-to-list 'eglot-server-programs '(ruby-mode "solargraph" "--stdio")))

(defun finder-current-dir-open()
  (interactive)
  (shell-command "open ."))

; 現在のファイルをphp-unitする。oms用
(defun php-unit()
  (interactive)
  (shell-command (concat "docker compose run --rm php-dev symfony php bin/phpunit " (replace-regexp-in-string "/Users/taro_morita/dev/rpst-oms-backend/" "/srv/" buffer-file-name))))

(defun yadm-add()
  (interactive)
  (shell-command "yadm add -u"))

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

(leaf ruby-mode
  :ensure t)

(leaf clojure-mode
  :ensure t)

(leaf eglot
  :ensure t
  :require t
  :hook
  
(leaf eglot-booster
  :when (executable-find "emacs-lsp-booster")
  :vc ( :url "https://github.com/jdtsmith/eglot-booster")
  :global-minor-mode t)

(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :ensure t
  :global-minor-mode t)

(leaf which-key
  :ensure t
  :config (which-key-mode))

(leaf company
  :ensure t
  :leaf-defer nil
  :blackout t
  :hook ((after-init-hook . global-company-mode)))

(leaf dracula-theme
  :ensure t)
(load-theme 'dracula t)

(leaf skk
  :ensure ddskk
  :custom ((default-input-method . "japanese-skk"))
  :config
  (leaf ddskk-posframe
    :ensure t
    :global-minor-mode t))
(global-set-key "\C-x\C-j" 'skk-mode)

(leaf web-mode
  :ensure t)
(leaf ripgrep
  :ensure t)

(leaf yaml-mode
  :ensure t)

(leaf exec-path-from-shell
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

(leaf org
  (org-babel-do-load-languages
   'org-babel-do-load-languages '((python . t)
				  ))
  (leaf ob-python
    :ensure t)
  :ensure t)

(leaf org-roam
  :ensure t)

(leaf magit
  :ensure t)

(leaf tramp
  :ensure t
  :custom
  (tramp-default-method . "sshx")
  :init
  (with-eval-after-load "tramp"
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
    (add-to-list 'tramp-remote-path "/home/taro/.volta/bin/intelephense")
    (add-to-list 'tramp-connection-properties
             (list (regexp-quote "/ssh:oms-dev:")
                   "remote-shell" "/usr/bin/bash"))
    ))

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
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     default))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")))
 '(package-selected-packages
   '(blackout clojure-mode company ddskk-posframe el-get
	      exec-path-from-shell git-gutter hydra leaf-keywords
	      magit org-roam php-mode projectile ripgrep web-mode
	      yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added ((t (:foreground "#108a3b" :background "#50fc7f"))) nil "Customized with leaf in `git-gutter' block")
 '(git-gutter:deleted ((t (:foreground "#8f2986" :background "#ff79c6"))) nil "Customized with leaf in `git-gutter' block")
 '(git-gutter:modified ((t (:foreground "#404040" :background "#c0fc7f"))) nil "Customized with leaf in `git-gutter' block"))

(defun php-cs-fixer ()
  (interactive)
  (when (eq major-mode 'php-mode)
    (let ((php-cs-fixer-directory "~/dev/rpst-oms-backend/app/"))
      (shell-command-to-string
       (concat "php-cs-fixer fix "
	       (shell-quote-argument (buffer-file-name))
	       " --path-mode=intersection --working-dir="
	       (shell-quote-argument php-cs-fixer-directory)))
     (revert-buffer t t t)))

(defun my-php-mode-hook ()
  (add-hook 'after-save-hook 'php-cs-fixer nil t))

(add-hook 'php-mode-hook 'my-php-mode-hook)
