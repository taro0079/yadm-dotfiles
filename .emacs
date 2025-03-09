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

; key remap
(global-set-key (kbd "C-c r") 'revert-buffer) ; ファイルをディスクの状態に戻す
(global-set-key "\C-t" 'other-window) ; 他のウィンドウに移動する
(which-key-mode) ; キーマップ一覧を表示する

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


(add-to-list 'default-frame-alist `(font . "Iosevka Nerd Font-16"))
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
(leaf php-mode
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

;; color theme
(leaf kuronami-theme
  :ensure t
  :config
 (load-theme 'kuronami t)
)

(leaf timu-caribbean-theme
  :ensure t
  :config
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
    )
  )

;; (require 'eglot)
;; 
;; (defun my/file-remote-p ()
;;   (and buffer-file-name
;;        (file-remote-p buffer-file-name)))
;; 
;; (defun my/get-remote-host-prefix ()
;;   (when (my/file-remote-p)
;;     (file-remote-p buffer-file-name)))
;; 
;; (defun my/eglot-php-init ()
;;   (if (my/file-remote-p)
;;       (let* ((remote-prefix (my/get-remote-host-prefix))
;;              (remote-phpactor (concat remote-prefix "/home/taro_morita/.local/bin/phpactor")))
;;         (if (and (file-executable-p remote-phpactor)
;;                  (zerop (process-exit-status (start-process "phpactor-test" nil remote-phpactor "--version"))))
;;             (progn
;;               (setq-local eglot-server-programs
;;                           `((php-mode . (,remote-phpactor "language-server"))))
;;               (message "Using remote phpactor at %s" remote-phpactor))
;;           (message "Remote phpactor not found or failed to execute. Please install phpactor on the remote server and ensure it is executable.")))
;;     (let ((phpactor-executable (executable-find "phpactor")))
;;       (if phpactor-executable
;;           (progn
;;             (setq-local eglot-server-programs
;;                         `((php-mode . (,phpactor-executable "language-server"))))
;;             (message "Using local phpactor at %s" phpactor-executable))
;;         (message "Local phpactor executable not found. Please install phpactor.")))))
;; 
;; (add-hook 'php-mode-hook 'my/eglot-php-init)
;; 
;; (with-eval-after-load 'eglot
;;   (setq remote-file-name-inhibit-locks t)
;;   (setq eglot-connect-timeout 120)
;;   (setq eglot-extend-to-xref t))

;; 日本語入力
(leaf skk
  :ensure ddskk
  :custom ((default-input-method . "japanese-skk"))
  :config
  (leaf ddskk-posframe
    :ensure t
    :global-minor-mode t))

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
;; マルチカーソル
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
(defvar remote-project-path "taro_morita@dev-tmorita:/var/www/rpst-v2/dev/"
  "リモートのプロジェクトのパス"
  )


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

(defvar local-project-path-v1 "~/dev/rpst/"
  "ローカルのプロジェクトのパス"
  )
(defvar remote-project-path-v1 "taro_morita@dev-tmorita:/var/www/precs/dev_tmorita/"
  "リモートのプロジェクトのパス"
  )

;; rpst-v1をファイル単位でデプロイする関数
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
