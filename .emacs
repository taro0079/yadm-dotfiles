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
(delete-selection-mode 1)

(set-language-environment "Japanese")
;; ファイルを自動で行を折り返す
(global-visual-line-mode 1)

;; kill-ringした内容をOSのクリップボードにもコピーする
(when (eq system-type 'darwin)
  (setq ns-use-pbpaste-pasteboard t)
  )

(setq eshell-path-env (getenv "PATH"))
; key remap
(global-set-key (kbd "C-c r") 'revert-buffer) ; ファイルをディスクの状態に戻す
(global-set-key "\C-t" 'other-window) ; 他のウィンドウに移動する
(global-set-key "\M-*" 'pop-tag-mark)
(which-key-mode) ; キーマップ一覧を表示する

;; diredでファイルをコピー、移動、貼り付けする
(eval-after-load "dired" '(progn
			    (define-key dired-mode-map (kbd "C-x w") 'dired-ranger-copy)
			    (define-key dired-mode-map (kbd "C-x x") 'dired-ranger-move)
			    (define-key dired-mode-map (kbd "C-x y") 'dired-ranger-paste)			    
			    ))

(defun rpst-api-project-root ()
  "Locate the project root directory by looking for composer.json."
  (locate-dominating-file default-directory "composer.json"))

;; 現在のファイルをphp-cs-fixerで整形する
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

(setq custom-file "~/.emacs.custom.el")

;; TRAMP modeでremoteサーバに接続したときにeglotでphpactorを見つけれるように設定
(defun my/eglot-tramp-phpactor-init ()
  (when (and buffer-file-name
	     (string-match "^/ssh:" buffer-file-name))
    (setq-local phpactor-executable "/home/taro_morita/.local/bin/phpactor")
    ))

;; (add-hook 'php-mode-hook 'my/eglot-tramp-phpactor-init)


; (add-to-list 'default-frame-alist '(font . "Moralerspace Radon NF"))

					; (set-face-attribute 'default nil :family "Moralerspace Radon NF" :height 120)

(set-frame-font "Moralerspace Radon NF-14")
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


(rc-require 'leaf)

(leaf ruby-mode
  :ensure t)
(leaf visual-regexp
  :ensure t
  :bind
  ("C-c r" . vr/replace)
  ("C-c q" . vr/query-replace)
  )

(leaf slack 
  :ensure t
  :config
  (slack-register-team
   :name "rpst"
   :cookie "xoxd-t8ecqTAQK%2FkxKnUOoe05U1e62Auw%2F2BdmL8F20QZJsrvZ3SP%2BvVEMPCqYxxgwPVs7BKwCYT%2BcWwBa16YfGZlsQgLFUmTkVn0KZ8Q%2F1fHIgisM2GlvIzFxno5IGbUoOXt9thDXYCpfXQYHqCNdDy5bbedSvCVKrIWagHNlYequHcTS92kuX7tmr%2FBBEkqP%2BCncQyXjnA%3D"
   :token "xoxc-23010945328-5285987411398-8622282653925-9552f7bd5af48b6fc438cc89759e80c840232a194eb064a10d7c8c5df1abc8e5"
   :default t)
)
(leaf vertico
  :ensure t
  :global-minor-mode t)

(defun my-php-mode ()
  (setq show-trailing-whitespace t)
  )

(leaf php-mode
  :hook ((php-mode . my-php-mode))
  :ensure t
  :custom
  (php-manual-url 'ja)
  (php-mode-coding-style 'psr2)
  :config
  (bind-key "C-c C--" 'php-current-class php-mode-map)
  (bind-key "C-c C-=" 'php-current-namespace php-mode-map)
  )

;; yasnippet-snippetのディレクトリを見つけるための関数
;; yasnippet-snippetsはleafで管理されているので、ディレクトリ名がダウンロードした日付になる。
;; これは可変なので正規表現でマッチするようにしている
(defun find-yasnippet-snippets-dir ()
  "Find the yasnippet-snippets directory in elpa."
  (let* ((elpa-dir (expand-file-name "~/.emacs.d/elpa"))
         (dirs (directory-files elpa-dir t "^yasnippet-snippets-[0-9]+\\.[0-9]+$"))
         (snippets-dir (cl-first dirs)))
    (when snippets-dir
      (expand-file-name "snippets" snippets-dir))))

(leaf copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el")
  :config
  (leaf editorconfig
    :ensure t
    )
  (leaf s
    :ensure t
    )
  (leaf dash
    :ensure t
    )
  :hook
  (prog-mode-hook .  copilot-mode)
  :bind
  (copilot-completion-map
   ("C-e" . copilot-accept-completion)
   ("M-f" . copilot-accept-completion-by-word)
   ("C-M-f" . copilot-accept-completion-by-paragraph)
   ("M-n" . copilot-accept-completion-by-line)
   ("C-M-n" . copilot-next-completion)
   ("C-M-p" . copilot-previous-completion)
   )
  (copilot-mode-map
   ("M-i" . copilot-complete)
   )
)

;; snippets
(leaf yasnippet
  :ensure t
  :config

  (setq yas-snippet-dirs
	(let ((yasnippet-snippets-dir (find-yasnippet-snippets-dir)))
	  (if yasnippet-snippets-dir
	      (list yasnippet-snippets-dir)
	    nil)))
  (yas-global-mode 1)
  )

(leaf yasnippet-snippets
  :ensure t)


;; (leaf company
;;   :ensure t
;;   :leaf-defer nil
;;   :config
;;   (global-company-mode)
;;   (setq company-idle-delay 0)
;;   (setq company-minimum-prefix-length 2))

(leaf corfu
  :ensure t
  :require t
  :custom
  (corfu-auto . t)
  (corfu-auto-delay . 0)
  (corfu-auto-prefix . 2)
  (corfu-cycle . t)
  (corfu-onexact-match . nil)
  (tab-always-indent . 'complete)
  :init
  (global-corfu-mode +1)
  )

(leaf kind-icon
  :after corfu
  :ensure t
  :custom (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(leaf eglot
  :ensure t
  :require t
  :hook
  (php-mode-hook . eglot-ensure)
  :config
  (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "M-,") 'pip-tag-mark)
;;  (with-eval-after-load "eglot"
;;  (add-to-list 'eglot-server-programs '(php-mode "intelephense" "--stdio")))
  
  )

;;(leaf eglot-booster
;;  :when (executable-find "emacs-lsp-booster")
;;  :vc ( :url "https://github.com/jdtsmith/eglot-booster")
;;  :global-minor-mode t)

(leaf eglot-booster
  :after eglot
  :config (eglot-booster-mode))



(leaf elec-pair
  :config
  (electric-pair-mode +1))

(leaf yatex
  :ensure t
  :commands (yatex-mode)
  :init
  (setq tex-command "lualatex -synctex=1")
  )
;; color theme
(leaf kuronami-theme
  :ensure t
  :config
;; (load-theme 'kuronami t)
)

;; (leaf monokai-theme
;;   :ensure t
;;   :config
;;   (load-theme 'monokai t))

(leaf timu-caribbean-theme
  :ensure t
;  :config
;  (load-theme 'timu-caribbean t)
  )

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
    (add-to-list 'tramp-remote-path "/home/taro_morita/.local/bin/phpactor")
    (add-to-list 'tramp-remote-path "/home/taro_morita/go/bin/gopls")
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
  :config
  (add-to-list 'tramp-connection-properties
		 (list (regexp-quote "/ssh:taro_morita@dev-tmorita:")
		       "remote-shell" "/bin/bash")))

;; 日本語入力
(leaf skk
  :ensure ddskk
  :custom ((default-input-method . "japanese-skk"))
  :config
  (leaf ddskk-posframe
    :ensure t
    :global-minor-mode t)
  (add-hook 'isearch-mode-hook 'skk-isearch-mode-setup) ;; increment searchでskkを利用できるような設定
  (add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup)

  )

(global-set-key "\C-x\C-j" 'skk-mode)

; shellのパスを引き継ぐ
(leaf exec-path-from-shell
  :ensure t
  :defun (exec-path-from-shell-initialize)
  :custom ((exec-path-from-shell-check-startup-files)
	   (exec-path-from-shell-variables . '("PATH" "GOPATH" "JAVA_HOME")))
  :config
  (exec-path-from-shell-initialize))

;; git client
(leaf magit
  :ensure t
;  :init
;  (magit-remote-git-executable "/usr/local/bin/git")
  :bind
  ("C-x g" . magit-status)
  )


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

(leaf dockerfile-mode
  :ensure t)

;; (leaf dracula-theme
;;   :ensure t)
;; マルチカーソル
;; (leaf multiple-cursors
;;   :ensure t)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(leaf consult
  :ensure t
  :bind (
   ("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ("C-x b" . consult-buffer)
   ("C-x r b" . consult-bookmark)
   ("M-s d" . consult-find)
   ("M-s c" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)   
   ("M-s g" . consult-grep)   
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
   )
  :config
  (consult-customize
;   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-find consult-fd
   :preview-key '(:debounce 0.4 any)
   ;:preview-key (kbd "M-.")
   )
)
(leaf avy
  :ensure t
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char-timer))

(leaf diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))


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
  (completion-styles . '(orderless))
  :config
  (with-eval-after-load 'corfu
    (add-hook 'corfu-mode-hook (lambda () (setq-local orderless-matching-styles '(orderless-flex))))))

;; install vterm
(leaf vterm
  :ensure t
  :config
  (global-set-key (kbd "C-c v") 'vterm)
  )

(leaf org-roam
  :if (and (file-directory-p "~/memo/org/")
	   (and (executable-find "rg")
		(executable-find "sqlite3")))
  :ensure t
  :bind
  (("C-c n a" . org-roam-alias-add)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n t" . org-roam-tag-add)
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-x m" . org-roam-dailies-capture-today)
   ("C-c n v" . org-roam-node-visit)
   )
  :config
  (org-roam-db-autosync-mode)
  )
(leaf marginalia
  :ensure t
  :config
  (marginalia-mode))

(leaf embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
	       '("\\ `\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))
  (leaf embark-consult
    :ensure t
    :hook
    (embark-collect-mode . consult-preview-at-point-mode))

(defvar local-project-path "~/ghq/rpst-v2/"
  "ローカルのプロジェクトのパス"
  )
(defvar remote-project-path "taro_morita@dev-tmorita:/var/www/rpst-v2/dev/"
  "リモートのプロジェクトのパス"
  )
(defvar local-project-path-rpst-api "~/ghq/rpst-api/"
  "ローカルのプロジェクトのパス"
  )
(defvar remote-project-path-rpst-api "taro_morita@rpst-api:/var/lib/rpst-api-docker/"
  "リモートのプロジェクトのパス"
  )

(defun transport-project-file (local-project-path remote-project-path)
  "引数で指定されたプロジェクトのファイルをリモートに転送する"
  (when (and (buffer-file-name)
	     (string-prefix-p (expand-file-name local-project-path)
			      (buffer-file-name)))
    (message "File is inside the project directory")
    (let ((relative-path (file-relative-name (buffer-file-name) (expand-file-name local-project-path))))
      (message "Transferring file: %s to remote path: %s" (buffer-file-name)
	       (concat remote-project-path relative-path))
      (start-process "rsync" "*rsync-output*"
		     "rsync" "-avz" (buffer-file-name)
		     (concat remote-project-path relative-path))
      (message "[%s] Complete to sending data." (file-name-nondirectory local-project-path))
      )))
(add-hook 'after-save-hook (lambda () (transport-project-file "~/ghq/rpst-docker/" "taro_morita@dev-tmorita:/var/lib/rpst-docker/")))

(defun transport-rpst-api ()
  (when (and (buffer-file-name)
	     (string-prefix-p (expand-file-name local-project-path-rpst-api)
			      (buffer-file-name)))
    (message "File is inside rpst-api project directory")
    (let ((relative-path (file-relative-name (buffer-file-name) (expand-file-name local-project-path-rpst-api))))
      (message "Transferring file: %s to remote path: %s" (buffer-file-name)
	       (concat remote-project-path-rpst-api relative-path))
      (start-process "rsync" "*rsync-output*"
		     "rsync" "-avx" (buffer-file-name)
		     (concat remote-project-path-rpst-api relative-path))
      (message "[rpst-api] Complete to sending data.")
      )))
(add-hook 'after-save-hook 'transport-rpst-api)

;; rpst-v2をファイル単位でデプロイする関数
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
		     (concat remote-project-path relative-path))
      (message "Complete to sending data.")
      )))

(add-hook 'after-save-hook 'transport-v2)

(defvar local-project-path-v1 "~/ghq/rpst/"
  "ローカルのプロジェクトのパス"
  )
(defvar remote-project-path-v1 "taro_morita@dev-tmorita:/var/www/precs/dev_tmorita/"
  "リモートのプロジェクトのパス"
  )

;; rpst-v1をファイル単位でデプロイする関数
(defun transport-v1 ()
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
(put 'upcase-region 'disabled nil)


(defun copy-file-path-and-line-to-clipboard ()
  "現在のファイルのプロジェクトからの相対パスとカーソル位置の行をクリップボードにコピーします。"
  (interactive)
  (let* ((file-path (buffer-file-name))
         (project-root (cond
                        ;; projectileが利用可能ならそれを使う
                        ((fboundp 'projectile-project-root)
                         (projectile-project-root))
                        ;; projectileがなければvcを試す
                        ((and (fboundp 'vc-root-dir) (vc-root-dir))
                         (vc-root-dir))
                        ;; どちらも利用できない場合は絶対パスを使用
                        (t nil)))
	 l;; 相対パス
         (relative-path (if (and project-root file-path)
                            (file-relative-name file-path project-root)
                          file-path))
	 ;; 行番号
         (line-number (line-number-at-pos))
	 ;; ↓行のテキストを抽出する
         ;; (line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (text (format "%s:%d" relative-path line-number)))
    (kill-new text)
    (message "プロジェクト相対パスと行をクリップボードにコピーしました。")))


(global-set-key (kbd "C-c l c") 'copy-file-path-and-line-to-clipboard)
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))

;; 日付を挿入する関数 主にorg-modeで使う
(defun insert-current-date (&optional diff)
  "現在年月日をカレントバッファに挿入する。引数Nを与えるとN日前の日付を挿入する。"
  (interactive "P")
  (insert
   (shell-command-to-string
    (format "echo -n $(LC_ALL=ja_JP date -v-%dd +'%%Y-%%m-%%d (%%a)' )" (or diff 0)))
   ))

;; 自動でbyte-compileを行う
(if (file-newer-than-file-p "~/.emacs" "~/.emacs.elc")
    (byte-compile-file "~/.emacs" t))
> > 
