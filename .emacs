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

(global-hl-line-mode t) ; 行のハイライト
(set-language-environment "Japanese")

;; ファイルを自動で行を折り返す
;;(global-visual-line-mode 1)
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
;; config for shell
(setq-default shell-file-name "/bin/bash")
;; kill-ringした内容をOSのクリップボードにもコピーする
(when (eq system-type 'darwin)
  (setq ns-use-pbpaste-pasteboard t)
  )

;; space indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(setq eshell-path-env (getenv "PATH"))
; key remap
(global-set-key (kbd "C-c r") 'revert-buffer) ; ファイルをディスクの状態に戻す
; (global-set-key "\C-t" 'other-window) ; 他のウィンドウに移動する
(global-set-key "\M-*" 'pop-tag-mark)
(global-set-key (kbd "C-c w l") 'windmove-right)
(global-set-key (kbd "C-c w h") 'windmove-left)
(global-set-key (kbd "C-c w j") 'windmove-down)
(global-set-key (kbd "C-c w k") 'windmove-up)
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


(defun php-cs-fixer ()
  (interactive)
  (shell-command-to-string (format "docker compose run --rm devcontainer php-cs-fixer fix %s" (file-relative-name(buffer-file-name) (projectile-project-root)) ))
  )

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

;; (set-frame-font "Moralerspace Radon NF-13")
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
;(ido-mode 1)
;(ido-everywhere 1)
;(setq ido-show-dot-for-dired t)
;; (setq display-line-numbers-type 'relative)
(setq display-line-numbers-type t)
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


;; (leaf lsp-pyright
;;   :ensure t
;;   :custom
;;   (lsp-pyright-disable-language-service . nil)
;;   (lsp-completion-enable t)
;;   )

(leaf wgrep
  :require t
  :ensure t)

;; (leaf evil
;;   :ensure t
;;   )
;; (require 'evil)
;; (evil-mode t)

(leaf lua-mode
  :ensure t)
(leaf rbenv
  :ensure t
  )

(leaf doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

; RSSリーダー
(leaf elfeed
  :ensure t
  :bind
  ("C-x w e" . elfeed)
  :custom
  (elfeed-feeds . '(
    ("http://nullprogram.com/feed/" blog emacs)
    ("https://news.yahoo.co.jp/rss/topics/top-picks.xml" news yahoo)
    ("https://news.yahoo.co.jp/rss/topics/science.xml" news yahoo science)
    ("https://rss.arxiv.org/rss/cond-mat.supr-con" articles science superconductively)
    ("https://qiita.com/popular-items/feed.atom" tech blog)
    ("https://zenn.dev/feed" tech blog)
    ("https://planet.emacslife.com/atom.xml" tech emacs)
    ))
  )

(leaf undo-tree
    :ensure t
    :global-minor-mode global-undo-tree-mode
    :custom
    (undo-tree-auto-save-history . nil)
    )

;; (leaf puni
;;   :ensure t
;;   :require t
;;   :global-minor-mode puni-global-mode
;;   :bind (puni-mode-map
;;          ;; ("C-)" . puni-slurp-forward)
;;          ("C-)" . puni-expand-region)
;;          ("C-}" . puni-barf-forward)
;;          ("M-(" . puni-wrap-round)
;;          ("M-s" . puni-splice)
;;          ("M-z" . puni-squeeze)
;;          ("M-r" . puni-raise)
;; 	     ("C-k" . puni-kill-line)
;;          ;;("C-c v c" . puni-mark-list-around-point) ;; vtermと衝突
;;          ;;("C-c v a" . puni-mark-sexp-around-point) ;; vtermと衝突
;;          )
;;   )

(leaf ruby-mode
  :ensure t
  :hook
  (ruby-mode-hook . global-rbenv-mode)
  )

(setq ruby-program-name "ruby")
;; for dimmer
;; corfuのウィンドウが表示されたときに他のウィンドウが暗くなるのを抑制する
(defun advise-dimmer-config-change-handler ()
  "Advise to only force process if no predicate is truthy."
  (let ((ignore (cl-some (lambda (f) (and (fboundp f) (funcall f)))
                         dimmer-prevent-dimming-predicates)))
    (unless ignore
      (when (fboundp 'dimmer-process-all)
        (dimmer-process-all t)))))

;; for dimmer
;; corfuのウィンドウが表示されたときに他のウィンドウが暗くなるのを抑制する
(defun corfu-frame-p ()
  "Check if the buffer is a corfu frame buffer."
  (string-match-p "\\` \\*corfu" (buffer-name)))

;; for dimmer
;; corfuのウィンドウが表示されたときに他のウィンドウが暗くなるのを抑制する
(defun dimmer-configure-corfu ()
  "Convenience settings for corfu users."
  (add-to-list
   'dimmer-prevent-dimming-predicates
   #'corfu-frame-p))

;; 非活性のウィンドウを薄くする
(leaf dimmer
  :ensure t
  :require t
  :config
  (advice-add
   'dimmer-config-change-handler
   :override 'advise-dimmer-config-change-handler)
  (dimmer-configure-corfu)
  (dimmer-mode)
  (setq dimmer-fraction 0.5)

  )

(leaf dmacro
  :ensure t
  :custom `((dmacro-key . ,(kbd "C-S-e")))
  :global-minor-mode global-dmacro-mode
  )

;; ========================
;; FOR PHP DEVELOPMENT
;; ========================
(leaf phpactor
  :ensure t
  :require t
;  :hook
;  (php-mode-hook . phpactor-mode)
;;  :config
;;  (setq phpactor-executable "/home/taro_morita/.local/bin/phpactor")
  )

;; REPL FOR PHP
(leaf psysh
  :ensure t)

;; PHPUNIT
(leaf phpunit
  :ensure t
  :config
  (setq phpunit-executable "docker compose run --rm devcontainer phpunit")
    )

;; RUN CODE
(leaf quickrun
  :ensure t)


(leaf visual-regexp
  :ensure t
  :bind
  ("C-c r" . vr/replace)
  ("C-c q" . vr/query-replace)
  )
(setq auth-sources'("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

(leaf slack
  :ensure t
  :config
  (slack-register-team
   :name "rpst"
   :cookie (auth-source-pick-first-password :host "slack.com" :user "morita0079@gmail.com" :port "cookie" )
   :token (auth-source-pick-first-password :host "slack.com" :user "morita0079@gmail.com" :port "token")
   :default t)
  )
(leaf consult-ghq
  :ensure t
  :require t
  :bind
  ("C-c g" . consult-ghq-find)
)
(leaf vertico
  :ensure t
  :global-minor-mode t)
(leaf ace-window
    :ensure t
    :require t
    :bind
    ("M-o" . ace-window)
    :config
    (setopt aw-keys '(?j ?k ?l ?i ?o ?h ?y ?u ?p))
    :custom-face
    (aw-leading-char-face . '((t (:height 4.0 :foreground "#f1fa8c"))))
    )

(defun my-php-mode ()
  (setq show-trailing-whitespace t)
  (require 'flymake-phpstan)
  (flycheck-mode t)
  )

(leaf php-mode
  :hook
  (php-mode-hook . my-php-mode)
  (php-mode-hook . php-ts-mode)
  (php-mode-hook . lsp-mode)
  (php-mode-hook . (lambda () (flymake-phpstan-turn-on)))
  :ensure t
  :custom
  (php-manual-url . 'ja)
  (php-mode-coding-style . 'psr2)
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
  :config
  (with-eval-after-load 'lsp-mode
    (setq lsp-completion-mode :none)
    )
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
(leaf cape
  :ensure t
  :require t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  )

(leaf kind-icon
  :after corfu
  :ensure t
  :custom (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; (leaf eglot
;;   :ensure t
;;   :require t
;;   :hook
;;   (php-mode-hook . eglot-ensure)
;;   :config
;; ;;  (setq eglot-events-buffer-size 100000)  ;; for debug
;; ;;  (setq eglot-stderr-buffer-size 100000)  ;; for debug
;; ;;  (setq eglot-connect-timeout 120)	  ;; for debug
;;   (setq eglot-events-buffer-config '(:size 0))
;;   (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
;;   (define-key eglot-mode-map (kbd "M-,") 'pip-tag-mark)
;;   (add-to-list 'eglot-server-programs
;;              '(python-mode . ("pyright-langserver" "--stdio")))


;; ;;  (add-to-list'eglot-server-programs '(php-mode . ("php" "~/.composer/vendor/bin/php-language-server.php")))
;;   ;; (with-eval-after-load "eglot"
;;   ;; (add-to-list 'eglot-server-programs '(php-mode "intelephense" "--stdio" "--memory-limit=4096M")))

;;   )



;;(leaf eglot-booster
;;  :when (executable-find "emacs-lsp-booster")
;;  :vc ( :url "https://github.com/jdtsmith/eglot-booster")
;;  :global-minor-mode t)

;; (leaf eglot-booster
;;   :after eglot
;;   :config (eglot-booster-mode))

(leaf lsp-mode
    :ensure t
    :require t
    :custom
    (
     (lsp-inhibit-message . t)
     (lsp-message-project-root-warning . t)
     (create-lockfiles . nil)
     (lsp-completion-provider . :none)
     (gc-cons-threshold . 100000000)
     )
    :hook
    (prog-major-mode . lsp-prog-major-mode-enable))

(leaf lsp-ui
    :ensure t
    :require t
    :after lsp-mode
    :custom ((lsp-lens--enable . t))
    :hook (lsp-mode-hook . lsp-ui-mode)
)

(leaf typst-ts-mode
  ;;  :elpaca (:type git :host sourcehut :repo "meow_king/typst-ts-mode")
  :custom
  (typst-ts-mode-watch-options . "--open"))

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
  )

;; (leaf 'eclipse-theme
;;   :ensure t)

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
;    (add-to-list 'tramp-remote-path "/home/taro_morita/.local/bin/phpactor")
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
  :ensure t
  :mode
  ("\\.org$'" . org-mode)
  :custom
  (org-directory . "~/org-roam/")
  (org-agenda-files . '("~/org-roam/" "~/org-roam/daily/"))
  :bind
  ("C-c a" . org-agenda)
  )

(leaf org-bullets
      :ensure t
      :require t
;;      :custom (org-bullets-bullet-list '("" "" "" "" "" "" "" "" "" ""))
      :hook (org-mode-hook . org-bullets-mode))

;; disable meow for now
;; modal editing
(leaf meow
  :ensure t
  :config
  (setopt meow-use-clipboard t)
  :hook
  ((meow-insert-exit-hook . (lambda nil
                              (if skk-mode (skk-latin-mode-on))))
   (eshell-mode-hook . meow-insert)
   (after-change-major-mode-hook . (lambda nil
                                     (if (and (featurep 'magit)
                                              (magit-commit-message-buffer))
                                         (meow-insert)))))
  )

(defun meow-setup ()
      (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
;;   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . delete-region)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   ;; '("f" . meow-find)
   '("f" . avy-goto-char-timer)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   ;; '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)
   '("<backspace>" . puni-backward-delete-char)
   '("v i" . meow-inner-of-thing)
   '("v a" . meow-bounds-of-thing)
   '("v c" . puni-mark-list-around-point)
   '("v x" . puni-mark-sexp-around-point)
   '("v l" . end-of-line)
   '("v h" . beginning-of-line)
   '("v v" . puni-expand-region)
   '("v j" . end-of-buffer)
   '("v k" . beginning-of-buffer)
   '("v r" . rectangle-mark-mode)
   '(", b d" . kill-t)
   '(", w d" . delete-window)
   '(", w h" . windmove-left)
   '(", w l" . windmove-right)
   '(", w j" . windmove-down)
   '(", w k" . windmove-up)
   '(", w v" . split-window-horizontally)
   '(", w s" . split-window-vertically)
   '(": w" . save-buffer) ;; like vim save
   '(": q" . meow-quit)
   ))

(require 'meow)
(meow-setup)
(setq meow-use-clipboard t)

;;(meow-global-mode 1)

;; local LLM
(leaf ellama
  :ensure t
  :bind ("C-c e" . ellama)
  :init
  (require 'llm-ollama)
  (setopt ellama-auto-scroll t)
  (ellama-context-header-line-global-mode +1)
  (ellama-session-header-line-global-mode +1)
  (setopt ellama-language "Japanese")
  (setopt ellama-provider (make-llm-ollama
      :chat-model "codellama:7b"
      :embedding-model "codellama:7b"
    ))
  (setopt ellama-translation-provider (make-llm-ollama
    :chat-model "aya:8b"
    :embedding-model "aya:8b"
    ))
  (setopt ellama-providers
    '(("codestral" . (make-llm-ollama
        ;; :chat-model "codestral:22b-v0.1-q4_K_S"
        ;; :embedding-model "codestral:22b-v0.1-q4_K_S"))
        :chat-model "codellama:7b"
        :embedding-model "codellama:7b"))
     ("gemma2" . (make-llm-ollama
        :chat-model "gemma2:13b"
        :embedding-model "gemma2:13b"))
     ("aya" . (make-llm-ollama
        :chat-model "aya:8b"
        :embedding-model "aya:8b"))
     ("llama3.1" . (make-llm-ollama
        :chat-model "llama3.1:7b"
        :embedding-model "llama3.1:7b"))
    ("codellama" . (make-llm-ollama
        :chat-model "codellama:7b"
        :embedding-model "codellama:7b"))
   ))
  )
(leaf org-modern
  :ensure t
  :require t
  :hook
  (org-mode . org-modern-mode)
;  (org-agenda-finalize . org-modern-agenda)
  :config
  (setq
   org-pretty-entities t
   org-insert-heading-respect-content t
   org-hide-emphasis-markers t)
  )

(leaf blamer
  :ensure t
  :custom
  (blamer-idle-time . 0.3)
  (blamer-min-offset . 70)
  (blamer-type 'visual)
  (blamer-pretty-time-p . t)
  :config
  (global-blamer-mode 1)
  )

(leaf ripgrep
  :ensure t)

(leaf dockerfile-mode
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
   ("C-x b" . consult-buffer)
   ("C-x r b" . consult-bookmark)
   ("C-c c d" . consult-find)
   ("C-c c c" . consult-locate)
   ("C-c c g" . consult-grep)
   ("C-c c G" . consult-git-grep)
   ("C-c c r" . consult-ripgrep)
   ("C-c c l" . consult-line)
   ("C-c c g" . consult-grep)
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
   consult-bookmark
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
  (global-diff-hl-mode)
  :hook
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
 )


(defvar my-error-map (make-keymap))

(leaf flymake
  :doc "A universal on-the-fly syntax checker"
  :bind ((prog-mode-map
          ("M-n" . flymake-goto-next-error)
          ("M-p" . flymake-goto-prev-error))))

(leaf flymake-phpstan
  :ensure t
  :hook (php-mode-hook . (lambda () (flymake-phpstan-turn-on)))
  )

(leaf web-mode
    :ensure t
    :custom
    (web-mode-enable-auto-pairing . t)
    :mode "\\.\\(html\\|twig\\|blade\\|tpl\\)$'"

    )

(leaf typescript-mode
  :ensure t
  :mode "\\.ts$'"
  :hook (typescript-mode-hook . #'lsp-deferred)
  )

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
  ;; :config
  ;; (org-roam-db-autosync-mode)
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
(add-hook 'after-save-hook (lambda () (transport-project-file "~/ghq/rpst-v2/" "taro_morita@rpst-api:/var/lib/rpst-api-docker/rpst-v2/")))
(add-hook 'after-save-hook (lambda () (transport-project-file "~/ghq/rpst/" "taro_morita@dev-tmorita:/var/www/precs/dev_tmorita/")))

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


(global-set-key (kbd "C-c y") 'copy-file-path-and-line-to-clipboard)
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))

;; 日付を挿入する関数 主にorg-modeで使う
(defun insert-current-date (&optional diff)
  "現在年月日をカレントバッファに挿入する。引数Nを与えるとN日前の日付を挿入する。"
  (interactive "P")
  (insert
   (shell-command-to-string
    (format "echo -n $(LC_ALL=ja_JP date -v-%dd +'%%Y-%%m-%%d (%%a)' )" (or diff 0)))
   ))

;; color scheme
(load-theme 'eclipse t)

(let* ((zshpath (shell-command-to-string "/usr/bin/env zsh -c 'printenv PATH'" ))
       (pathlst (split-string zshpath ":")))
  (setq exec-path pathlst)
  (setq eshell-path-env zshpath)
  (setenv "PATH" zshpath))
(setenv "PATH" (concat "~/.rbenv/shims/:" (getenv "PATH")))

;; eshellにエイリアスを設定する
(setq eshell-command-aliases-list
      (append
       (list
	(list "ls" "ls -la")
	(list "mbv2" "~/.local/scripts/match-branch-local-and-remote.sh ~/ghq/rpst-v2/ /var/www/rpst-v2/dev/ dev-tmorita")
	(list "mbv1" "~/.local/scripts/match-branch-local-and-remote.sh ~/ghq/rpst/ /var/www/precs/dev_tmorita/ dev-tmorita")
	)
       )
      )

;; for php

;; php class generator
(defun php-class (class-name class-comment)
  (interactive "sClass name: \nsOptional comment for the class: ")
  (php-class-in-path class-name "." class-comment)
  )

(defun php-class-in-path (class-name class-file-path class-comment)
  (interactive "sClass name: \nsClass file path: \nsOptional comment for the class")
  (if (equal "" class-name)
      (message "Cannot create a class without a name.")
    (let* ((class-name (concat (capitalize (substring class-name 0 1))
			       (substring class-name 1)))
	   (class-file-name (concat class-file-path "/" class-name ".php")) ; file name
	   )
      (if (file-exists-p class-file-name)
	  (message "The file %s already exists." class-file-name)
	(progn
	  ;; generate buffer or visit
	  (switch-to-buffer (find-file-noselect class-file-name))
	  (insert "<?php" ?\n
		  ?\t "/* " class-file-name " */" ?\n
		  ?\n ?\n
		  "/**" ?\n
		  " * " class-comment ?\n
		  " * " ?\n
		  " */ " ?\n
		  "class " class-name " {" ?\n ?\n
		  "/**" ?\n
		  " * Default constructor" ?\n
		  " */" ?\n
		  "public function __construct() {" ?\n
		  "}" ?\n ?\n ?\n
		  )

	  ;; insert the end of class
	  (save-excursion
	    (insert ?\n ?\n ?\n
		    "}" ?\n
		    ?\n ?\n
		    )
	    )

	  ;; indent the xreated region
	  (c-indent-region (point-min) (point-max) t)
	  (save-buffer)
	  (message "Generation of the class %s is completed. file %s is saved !" class-name class-file-name)
	  )
	)
      )
    )
  )

;; Create a new property, that is a variable with private accessor
;; and with a couple of getter/setter methods
;; refer from https://github.com/fluca1978/fluca1978-coding-bits/blob/master/emacs-lisp/fluca1978-php.el
(defun php-prop (property-name property-comment)
  "Creates a new class property variable with an optional
   comment and with a getter and a setter method at the point where
   the cursor is"
  ;; prompt the user for the data to insert
  (interactive "sProperty name: \nsOptional comment for the property: ")
  (progn
    ;; ensure that the property name is correct, that is not an empty
    ;; string and remove any leading $sign (the user could have typed $myProperty
    ;; instead of myProperty)
    (while (string-match "[\$ \s\t]^*" property-name)
      (setq property-name (replace-match "" nil nil property-name))
      )
    (while (string-match "[ \s\t]$*" property-name)
      (setq property-name (replace-match "" nil nil property-name))
      )
    (if (equal "" property-name)
	;; the user has not specified the property name!
	(message "Cannot insert a not specified property, aborting!")
      ;; if here the user has specified the property name, so
      ;; compute the names of getter/setter
      (let ((setter-name (concat "set"
				 (capitalize (substring property-name 0 1) )
				 (substring property-name 1) )
			 ) 		; end of the setter-name variable
	    (getter-name (concat "get"
				 (capitalize (substring property-name 0 1) )
				 (substring property-name 1) )
			 ) 		; end of the getter-name variable

	    (generate-getter t) 		; do I have to generate the getter?
	    (generate-setter t) 		; do I have to generate the setter?
	    (src-point (point)) 		; where am I?
	    (property-insertion-point (point) ) ; the point where the property will be inserted
	    (property-end-point (point) )	      ; the point where the property ends
	    (generate-property t)
	    (setter-arg-name (concat "$" property-name ) )
	    (method-insertion-point (point)) ;where the methods will be inserted
	    ) 				; end of the let variable list

	;; remember the current cursor position in the buffer
	(save-excursion
	  (progn

	    ;; I need to check if the getters and setters are already there
	    (goto-char (point-min) )
	    (search-forward "class")		; move to the beginning of the class
	    (if (re-search-forward (concat "[\s \t\n]*public[\s \t\n]*function[\s \t\n]*" setter-name "[\s \t\n]*\(.*\)") nil t 1)
		(setq generate-setter nil)
	      (setq generate-setter t)
	      )
	    (goto-char (point-min) )
	    (if (re-search-forward (concat "[\s \t\n]*public[\s \t\n]*function[\s \t\n]*" getter-name "[\s \t\n]*\([\s \t\n]*\)") nil t 1)
		(setq generate-getter nil)
	      (setq generate-getter t)
	      )
	    (goto-char (point-min) )
	    (if (re-search-forward (concat "[\s \t\n]* \\(private\\|public\\)[\s \t\n]*$" property-name) nil t 1)
		(progn
		  (setq generate-property nil)
		  (setq property-insertion-point (point))
		  )
	      (setq generate-property t)
	      )

	    ;; go back to the original position
	    (goto-char src-point)

	    ;; do I have to generate the property?
	    (if generate-property
		(progn
		  (insert ?\n ?\t
			  "/**" ?\n
			  " * " property-comment ?\n
			  "*/" ?\n
			  "private $" property-name " = null;" ?\n
			  ?\n ?\n
			  )			; end of the insert for the property
		  (setq property-end-point (point) ) ; store where the property ends
		  ;; indent the property region
		  (c-indent-region property-insertion-point property-end-point t)


		  ;; go to the end of the buffer and position before the last
		  ;; curly brace that should close the class
		  (goto-char (point-max))		; goto end of buffer
		  (re-search-backward "}")		; go back to the last }
		  (setq method-insertion-point (point) ) ; store where the methods will be added

		  (if generate-getter
		      (insert ?\n ?\t
			      "/**" ?\n
			      "* Getter for the property " property-name "." ?\n
			      "* \\returns the current value of the property " property-name ?\n
			      "*" ?\n
			      "* \\author " user-full-name " - " user-mail-address ?\n
			      "*/" ?\n
			      "public function " getter-name "(){"
			      ?\n ?\t
			      "return $this->" property-name ";"
			      ?\n ?\t
			      "}"
			      ?\n ?\n ?\n
			      )		; end of the insert for the getter
		    (message "Skipping the generation of the getter method")
		    )			; end of the if for the getter method
		  (if generate-setter
		      (insert
		       ?\n ?\t
		       "/**" ?\n
		       "* Setter of the property " property-name "." ?\n
		       "* \\param " setter-arg-name " the new value for the property" ?\n
		       "*" ?\n
		       "* \\author " user-full-name " - " user-mail-address ?\n
		       "*/" ?\n
		       "public function " setter-name "( " setter-arg-name " ){"
		       ?\n ?\t
		       "$this->" property-name " = " setter-arg-name ";"
		       ?\n ?\t
		       "}"
		       ?\n ?\n ?\n
		       )			; end of the insert body for the setter method
		    (message "Skipping the generation of the setter method")
		    )				; end of the if for the setter method


		  ;; indent the region for the methods
		  (c-indent-region method-insertion-point (point) t)

		  ) 				; end of the progn on the generation of the property

	      (message "Cannot generate the property [%s], it already exists at line %d"
		       property-name
		       (count-lines 1 property-insertion-point ) )
	      )				; end of the if for the property generation


	    ) 					; end of progn within save excursion
	  )					; end of the save-escurion
	)						; end of the let main body

      )					;end of the if on the property name
    )					; end of the progn
  )


;; trail whitespaceを表示する
(setq-default show-trailing-whitespace t)

;; whitespaceを表示するモードの制御
;; whitespaceを表示しないモードのリスト
(defvar my/disable-trailing-modes
  '(commit-mode
    eshell-mode
    )
  )

(defun my/disable-trailing-mode-hook ()
  "Disable show trail whitespaces"
  (setq show-trailing-whitespace nil)
  )

(mapc
 (lambda (mode)
   (add-hook (intern (concat (symbol-name mode) "-hook"))
	     "my/disable-trailing-mode-hook"))
 my/disable-trailing-modes)

;; for typst
;; UNDER CONSTRUCTION

;; (defun org-typst-export-slide (filename)
;;   "Org mode bufferをTypstスライドに変換し、指定されたファイル名で保存します。"
;;   (interactive "F出力ファイル名: ")
;;   (let ((org-text (buffer-string))
;;         (typst-text ""))

;;     (org-map-entries
;;      (lambda ()
;;        (let ((level (org-current-level))
;;              (heading (org-get-heading t))
;;              (body (org-element-contents (org-element-at-point))))

;;          (setq typst-text
;;                (concat typst-text
;;                        (cond
;;                         ((= level 1) (format "#slide(title: \"%s\")\n%s\n\n" heading (org-element-interpret-data body)))
;;                         ((= level 2) (format "== %s\n%s\n\n" heading (org-element-interpret-data body)))
;;                         ((= level 3) (format "=== %s\n%s\n\n" heading (org-element-interpret-data body)))
;;                         (t (format "%s\n\n" (org-element-interpret-data body))))))))

;;     (with-temp-file filename
;;       (insert typst-text))

;;     (message "Typstスライドを %s に保存しました。" filename))))

;; (defun org-element-interpret-data (element)
;;   "Org elementの内容をTypst形式の文字列に変換します。"
;;   (let ((result ""))
;;     (dolist (item element)
;;       (setq result
;;             (concat result
;;                     (cond
;;                      ((eq (car item) 'paragraph) (format "%s\n" (org-element-interpret-data (cdr item))))
;;                      ((eq (car item) 'plain-list) (org-element-interpret-plain-list (cdr item)))
;;                      ((eq (car item) 'src-block) (org-element-interpret-src-block (cdr item)))
;;                      ((eq (car item) 'bold) (format "*%s*" (org-element-interpret-data (cdr item))))
;;                      ((eq (car item) 'italic) (format "_%s_" (org-element-interpret-data (cdr item))))
;;                      ((eq (car item) 'verbatim) (format "`%s`" (org-element-interpret-data (cdr item))))
;;                      ((eq (car item) 'link) (org-element-interpret-link (cdr item)))
;;                      ((stringp item) item)
;;                      (t (format "（未対応要素: %s）" (car item)))))))
;;     result))

;; (defun org-element-interpret-plain-list (element)
;;   "Org plain-list要素をTypst形式の文字列に変換します。"
;;   (let ((result ""))
;;     (dolist (item element)
;;       (let ((bullet (plist-get item :bullet))
;;             (contents (plist-get item :contents)))
;;         (setq result (concat result (format "%s %s\n" bullet (org-element-interpret-data contents))))))
;;     result))

;; (defun org-element-interpret-src-block (element)
;;   "Org src-block要素をTypst形式の文字列に変換します。"
;;   (let ((result ""))
;;     (let ((lang (plist-get (car element) :language))
;;           (code (plist-get (car element) :value)))
;;       (setq result (format "`%s\n%s\n`\n" lang code)))
;;     result))

;; (defun org-element-interpret-link (element)
;;   "Org link要素をTypst形式の文字列に変換します。"
;;   (let ((result ""))
;;     (let ((path (plist-get (car element) :path))
;;           (description (plist-get (car element) :description)))
;;       (setq result (format "[%s](%s)" (or description path) path)))
;;     result))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)))

   
