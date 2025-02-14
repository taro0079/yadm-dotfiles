(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
; native-compの警告を表示しない
(setq native-comp-async-report-warnings-errors 'silent)
(setq make-backup-files nil)
(setq backup-inhibited nil)
(setq create-lockfiles nil)

(defun rpst-api-project-root ()
  "Locate the project root directory by looking for composer.json."
  (locate-dominating-file default-directory "composer.json"))

(defun php-cs-fixer-current-file ()
  "Run php-cs-fixer on the current file."
  (interactive)
  (let ((project-root (rpst-api-project-root)))
    (if project-root
        (let ((default-directory project-root))
          ;; PHP-CS-Fixerのコマンドを実行
          (when buffer-file-name
            (shell-command (format "php-cs-fixer fix %s" (shell-quote-argument buffer-file-name)))
            (revert-buffer :ignore-auto :noconfirm :preserve-modes)
            (message "php-cs-fixer applied on %s" buffer-file-name)))
    (message "Project root not found. php-cs-fixer not applied."))))


(setenv "LIBRARY_PATH" "/opt/homebrew/lib/gcc/14/:/opt/homebrew/lib/gcc/14/gcc/aarch64-apple-darwin23/14")
(exec-path-from-shell-initialize)

(setq custom-file "~/.emacs.custom.el")

;; key bindings
(global-set-key "\C-t" 'other-window)


;; (add-to-list 'default-frame-alist `(font . "JetBrainsMono Nerd Font-16"))
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
;(ido-mode 1)
;(ido-everywhere 1)
;(setq ido-show-dot-for-dired t)
(global-display-line-numbers-mode)

(load-file custom-file)

(defvar package-contents-refreshed nil)

(defun package-refresh-contents-once ()
  (when (not package-contents-refreshed)
    (setq package-contents-refreshed t)
    (package-refresh-contents)))

(defun require-one-package (package)
  (when (not (package-installed-p package))
    (package-refresh-contents-once)
    (package-install package)))

(defun rc-require (&rest packages)
  (dolist (package packages)
    (require-one-package package)))

; (defun require-theme (theme)
;   (let ((theme-package (->> theme
; 			    (symbol-name)
; 			    (funcall (-flip #'concat) "-theme")
; 			    (intern))))
;     (rc-require theme-package)
;     (load-theme theme t)))

(rc-require 'php-mode)
(require 'php-mode)
(rc-require 'leaf)

(leaf ruby-mode
  :ensure t)

(leaf company
  :ensure t
  :leaf-defer nil
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))  


(leaf eglot
  :ensure t
  :require t
  :config
  (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "M-,") 'pip-tag-mark)
  
  )

(leaf eglot-booster
  :when (executable-find "emacs-lsp-booster")
  :vc ( :url "https://github.com/jdtsmith/eglot-booster")
  :global-minor-mode t)
			 
(leaf vertico
  :ensure t
  :global-minor-mode t)

; 括弧の自動補完
(leaf elec-pair
  :config
  (electric-pair-mode +1))

; 他プロセスの編集をバッファに反映する
(leaf autorevert
  :init
  (global-auto-revert-mode +1))

(leaf csv-mode
  :ensure t)

(leaf go-mode
  :ensure t)

(leaf tramp
  :ensure t
  :init
  (with-eval-after-load "tramp"
;    (add-to-list 'tramp-remote-path "home/taro_morita/.npm-global/bin")
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;    (add-to-list 'tramp-remote-path "/home/taro_morita/.volta/bin/intelephense")
    )
  )


(require 'eglot)
(with-eval-after-load "eglot"
  (add-to-list 'eglot-server-programs '(php-mode "/Users/taro_morita/.volta/bin/intelephense" "--stdio")))


;(with-eval-after-load "eglot"
 ; (add-to-list 'eglot-server-programs '(php-mode "intelephense" "--stdio")))

;(require 'eglot)
;(add-to-list 'eglot-server-programs
;	     '(php "intelephense" "--stdio"))
(add-hook 'php-mode-hook 'eglot-ensure)



(leaf skk
  :ensure ddskk
  :custom ((default-input-method . "japanese-skk"))
  :config
  (leaf ddskk-posframe
    :ensure t
    :global-minor-mode t))

(global-set-key "\C-x\C-j" 'skk-mode)

(leaf exec-path-from-shell
  :ensure t)

(leaf magit
  :ensure t)

(leaf org
  :ensure t)

(leaf org-modern
  :ensure t
  :config
  (with-eval-after-load 'org (global-org-modern-mode))
  (setq
   org-pretty-entities t
   org-insert-heading-respect-content t
   org-hide-emphasis-markers t))


(leaf blamer
  :ensure t
  :config
  (global-blamer-mode 1))
  
(leaf ripgrep
  :ensure t)

;; (leaf dracula-theme
;;   :ensure t)
(leaf multiple-cursors
  :ensure t)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(leaf consult
  :ensure t
  :bind (
   ("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ; :map goto-map
   ; ("e" . consult-compile-error)
   ; ("f" . consult-flymake)
   ; ("g" . consult-goto-line)
   ; ("o" . consult-outline)
   ; :map search-map
   ; ("d" . consult-fd)
   ; ("g" . consult-grep)
   ; ("G" . consult-git-grep)
   ; ("r" . consult-ripgrep)
   ))
(leaf avy
  :ensure t
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char))

(leaf diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

; (leaf doom-themes
;   :ensure t
;   :config
;   (setq doom-themes-enable-bold t
; 	doom-themes-enable-italic t)
;   (load-theme 'doom-one t)
;   (doom-themes-org-config))

(leaf solarized-theme
  :ensure t
  :config
;  (load-theme 'solarized-light t)
  )

(defvar my-error-map (make-keymap))

(leaf flymake
  :doc "A universal on-the-fly syntax checker"
  :bind ((prog-mode-map
          ("M-n" . flymake-goto-next-error)
          ("M-p" . flymake-goto-prev-error))))

(leaf affe
  :ensure t
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key "M-."))

(leaf orderless
  :ensure t
  :custom
  (completion-styles . '(orderless)))

(defvar local-project-path "~/dev/rpst-v2/"
  "ローカルのプロジェクトのパス"
  )
(defvar remote-project-path "taro_morita@dev-tmorita-rpst:/var/www/rpst-v2/dev/"
  "リモートのプロジェクトのパス"
  )

(defun transport-v2 ()
  (message "transport-v2: called")
  (when (and (buffer-file-name)
	     (string-prefix-p (expand-file-name local-project-path)
			      (buffer-file-name)))
    (message "File is inside the project directory")
    (let ((relative-path (file-relative-name (buffer-file-name) (expand-file-name local-project-path))))
      (message "Transferring file: %s to remote path: %s" (buffer-file-name)
	       (concat remote-project-path relative-path))

      (start-process "rsync" "*rsync-output*"
		     "rsync" "-avz" (buffer-file-name)
		     (concat remote-project-path relative-path)))))

; (add-hook 'after-save-hook 'transport-v2)

(defvar local-project-path-v1 "~/dev/rpst/"
  "ローカルのプロジェクトのパス"
  )
(defvar remote-project-path-v1 "taro_morita@dev-tmorita:/var/www/precs/dev_tmorita/"
  "リモートのプロジェクトのパス"
  )

(defun transport-v1 ()
  (message "transport-v1: called")
  (when (and (buffer-file-name)
	     (string-prefix-p (expand-file-name local-project-path-v1)
			      (buffer-file-name)))
    (message "File is inside the project directory")
    (let ((relative-path (file-relative-name (buffer-file-name) (expand-file-name local-project-path-v1))))
      (message "Transferring file: %s to remote path: %s" (buffer-file-name)
	       (concat remote-project-path-v1 relative-path))

      (start-process "rsync" "*rsync-output*"
		     "rsync" "-avz" (buffer-file-name)
		     (concat remote-project-path-v1 relative-path)))))

(add-hook 'after-save-hook 'transport-v1)

;(consult-customize
; consult-ripgrep consult-git-grep consult-grep
; consult-bookmark consult-recent-file consult-xref
; consult--source-recent-file
; consult--source-project-recent-file
; consult--source-bookmark
; :preview-key (kbd "M-."))
; (load-theme 'dracula t)
(load-file custom-file)
