;; -*- lexical-binding: t; -*-

;;basic configuration
(scroll-bar-mode -1)
(electric-pair-mode 1)


;; spell check
(setq ispell-program-name "hunspell")
(setq ispell-dictionary "en_US")
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; window moving
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)


;; config for tramp mode
(with-eval-after-load 'tramp
 (add-to-list 'tramp-remote-path "/Users/taro_morita/.local/bin"))


;; straight setting
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)
(setq straight-repository-branch "master")
(require 'straight-x)
(straight-use-package 'use-package)

(use-package
  spacemacs-theme
  :ensure t
  :config
    (load-theme 'spacemacs-dark t)

  )


;; lsp (lsp-bridge) setting
(use-package posframe :straight t)
(use-package yasnippet :straight t :config (yas-global-mode 1))
(use-package markdown-mode :straight t)
;; ~/.emacs.d/straight/repos/lsp-bridge/python-lsp-bridgeにシンボリックリンクを貼らないと動作しない。
;; 以下のコマンドを実行する。
;; ln -s ~/.emacs.d/straight/repos/lsp-bridge/python-lsp-bridge ~/.local/bin/python-lsp-bridge
;; (use-package lsp-bridge
;;   :straight
;;   (lsp-bridge
;;    :type git
;;    :host github
;;    :repo "manateelazycat/lsp-bridge"
;;    :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;    :build (:not compile))
;;   :init
;;   (setq lsp-bridge-python-command "python3")
;;   :config
;;   (global-lsp-bridge-mode))


;; インデントにtabを利用せずスペースを利用する
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(global-display-line-numbers-mode t)
(global-auto-revert-mode 1)
(let ((mono-spaced-font "CaskaydiaCove Nerd Font")
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 140)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

(setq custom-file (locate-user-emacs-file "custome.el"))
(load custom-file :no-error-file-is-missing)


(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))


;; php-cs-fixerの設定ファイルを自動検出してオプションを返す関数
(defun my/php-mode-php-cs-fixer-config-option ()
  (let ((path (locate-dominating-file buffer-file-name ".php-cs-fixer.dist.php")))
    (if path (concat "--config=" (expand-file-name ".php-cs-fixer.dist.php" path))
      "")))

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(menu-bar-mode 1)
(scroll-bar-mode 1)
(tool-bar-mode -1)

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; (use-package moody
;;   :ensure t
;;   :config
;;   (moody-replace-mode-line-front-space)
;;   (moody-replace-mode-line-buffer-identification)
;;   (when (eq system-type 'darwin)
;;     (setq moody-slant-function 'moody-slant-apple-rgb)))

;; (use-package modus-themes
;;   :ensure t
;;   :demand t
;;   :config
;;   (load-theme 'modus-vivendi-tinted :no-confirm-loading))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-hook 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

;; (use-package expand-region
;;   :ensure t
;;   :bind ("C-=" . er/expand-region))

;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :config
;;   (progn
;;     (setq treemacs-buffer-name-function            #'treemacs-default-buffer-name
;;           treemacs-buffer-name-prefix              " *Treemacs-Buffer-"
;;           treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
;;           treemacs-deferred-git-apply-delay        0.5
;;           treemacs-directory-name-transformer      #'identity
;;           treemacs-display-in-side-window          t
;;           treemacs-eldoc-display                   'simple
;;           treemacs-file-event-delay                2000
;;           treemacs-file-extension-regex            treemacs-last-period-regex-value
;;           treemacs-file-follow-delay               0.2
;;           treemacs-file-name-transformer           #'identity
;;           treemacs-follow-after-init               t
;;           treemacs-expand-after-init               t
;;           treemacs-find-workspace-method           'find-for-file-or-pick-first
;;           treemacs-git-command-pipe                ""
;;           treemacs-goto-tag-strategy               'refetch-index
;;           treemacs-header-scroll-indicators        '(nil . "^^^^^^")
;;           treemacs-hide-dot-git-directory          t
;;           treemacs-indentation                     2
;;           treemacs-indentation-string              " "
;;           treemacs-is-never-other-window           nil
;;           treemacs-max-git-entries                 5000
;;           treemacs-missing-project-action          'ask
;;           treemacs-move-files-by-mouse-dragging    t
;;           treemacs-move-forward-on-expand          nil
;;           treemacs-no-png-images                   nil
;;           treemacs-no-delete-other-windows         t
;;           treemacs-project-follow-cleanup          nil
;;           treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;           treemacs-position                        'left
;;           treemacs-read-string-input               'from-child-frame
;;           treemacs-recenter-distance               0.1
;;           treemacs-recenter-after-file-follow      nil
;;           treemacs-recenter-after-tag-follow       nil
;;           treemacs-recenter-after-project-jump     'always
;;           treemacs-recenter-after-project-expand   'on-distance
;;           treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
;;           treemacs-project-follow-into-home        nil
;;           treemacs-show-cursor                     nil
;;           treemacs-show-hidden-files               t
;;           treemacs-silent-filewatch                nil
;;           treemacs-silent-refresh                  nil
;;           treemacs-sorting                         'alphabetic-asc
;;           treemacs-select-when-already-in-treemacs 'move-back
;;           treemacs-space-between-root-nodes        t
;;           treemacs-tag-follow-cleanup              t
;;           treemacs-tag-follow-delay                1.5
;;           treemacs-text-scale                      nil
;;           treemacs-user-mode-line-format           nil
;;           treemacs-user-header-line-format         nil
;;           treemacs-wide-toggle-width               70
;;           treemacs-width                           35
;;           treemacs-width-increment                 1
;;           treemacs-width-is-initially-locked       t
;;           treemacs-workspace-switch-cleanup        nil)

;;     ;; The default width and height of the icons is 22 pixels. If you are
;;     ;; using a Hi-DPI display, uncomment this to double the icon size.
;;     ;;(treemacs-resize-icons 44)

;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (treemacs-fringe-indicator-mode 'always)
;;     (when treemacs-python-executable
;;       (treemacs-git-commit-diff-mode t))

;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null treemacs-python-executable)))
;;       (`(t . t)
;;        (treemacs-git-mode 'deferred))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple)))

;;     (treemacs-hide-gitignored-files-mode nil))
;;   :bind
;;   (:map global-map
;;         ("M-0"       . treemacs-select-window)
;;         ("C-x t 1"   . treemacs-delete-other-windows)
;;         ("C-x t t"   . treemacs)
;;         ("C-x t d"   . treemacs-select-directory)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ("C-x t C-t" . treemacs-find-file)
;;         ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after (treemacs evil)
;;   :ensure t)

;; (use-package treemacs-projectile
;;   :after (treemacs projectile)
;;   :ensure t)

;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once)
;;   :ensure t)

;; (use-package treemacs-magit
;;   :after (treemacs magit)
;;   :ensure t)

;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

;; (use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
;;   :after (treemacs)
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Tabs))

;; (treemacs-start-on-boot)

(use-package doom-themes
  :ensure t
  :config
  ;; (load-theme 'doom-solarized-dark t)
  (doom-themes-org-config))


(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (style basic partial-completion)))))


(use-package savehist
  :ensure nil ;; because it is built-in
  :hook (after-init . savehist-mode))

(use-package cape
  :ensure t
  :after corfu
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'yasnippet-capf)
  (add-hook 'completion-at-point-functions #'eglot-completion-at-point))

(use-package corfu
  :ensure t
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match 'separator)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("SPC" . corfu-insert-separator)))
(with-eval-after-load 'lsp-bridge
  (add-hook 'lsp-bridge-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode -1))))

(use-package yasnippet-capf
  :ensure t
  :after yasnippet)

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package dired
  :ensure nil
  :straight nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))
(use-package flymake-eslint
  :ensure t
  :hook
  (js-mode . flymake-eslint-enable)
  (typescript-ts-mode . flymake-eslint-enable)
  (tsx-ts-mode . flymake-eslint-enable)
  )

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
     ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(setq corfu-auto t
    corfu-auto-delay 0.2
    corfu-auto-trigger "."
    corfu-quit-no-match 'separator)

(use-package php-mode
  :ensure t
  :mode "\\.php\\'")

(use-package eglot
  :ensure nil
  :hook
  ((python-mode . eglot-ensure)
   (ruby-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (php-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t)
  :bind (:map eglot-mode-map
          ("C-c a" . eglot-code-actions)
          ("C-c r" . eglot-rename))
  :config
  (add-to-list 'eglot-server-programs
               '((php-mode) . ("phpactor" "language-server")))
  (add-to-list 'eglot-server-programs
               '((python-mode) . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode typescript-mode) . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
           '((ruby-mode) . ("ruby-lsp"))))
(use-package eglot-booster
  :straight ( eglot-booster :type git :host nil :repo "https://github.com/jdtsmith/eglot-booster" )
  :ensure t
  :after eglot

  :config
  (eglot-booster-mode)
  :init
  (defun my/eglot-booster-maybe-enable ()
    "Enable eglot except for python"
    (unless (derived-mode-p 'python-mode)
      (eglot-booster-mode 1)))
  :hook (eglot-managed-mode . my/eglot-booster-maybe-enable)
  )

;; (use-package eglot-x
;;   :ensure t
;;   :vc (:url "https://github.com/nemethf/eglot-x" :rev :newest)

;;   :after eglot
;;   :config
;;   (eglot-x-setup))

(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

(use-package eat :ensure t)
(straight-use-package
 '(monet :type git :host github :repo "stevemolitor/monet"))

(use-package vterm :ensure t)
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
        :rev :newest
        :branch "main")
  :hook
  ((prog-mode . copilot-mode)
   (text-mode . copilot-mode))
  :bind
  (:map copilot-completion-map
    ("C-TAB" . copilot-accept-completion)
    ("C-<tab>" . copilot-accept-completion)
    ("C-c \\" . copilot-clear-overlay))
  :config
  (setq copilot-idle-delay 0.2)
  (setq copilot-max-char 10000))

(use-package git-commit :straight nil)
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
(use-package magit-prime
  :ensure t
  :config
  (add-hook 'magit-pre-refresh-hook 'magit-prime-refresh-cache))

(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
     (text-mode . diff-hl-mode)
     (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package blamer
  :ensure t
  :hook (after-init . blamer-mode)
  :config
  (global-blamer-mode 1)
  :custom
  (blamer-idle-time 0.6)
  (blamer-min-offset 20))

;; (use-package expand-region
;;   :ensure t
;;   :bind ("C-=" . er/expand-region))

(use-package expreg
  :ensure t
  :bind (
     ("C--" . expreg-contract)
     ("C-=" . expreg-expand)))
(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode +1))


(use-package web-mode
  :ensure t
  :mode ("\\.tpl\\'" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 2
    web-mode-code-indent-offset 2
    web-mode-css-indent-offset 2)
  (add-to-list 'web-mode-engine-file-regexps '("django" . "\\.tpl\\'"))
  (setq web-mode-template-delimiters-alist
    '(("django" .  (("<!--{" . "}-->") ("<!--{" . "}-->"))))))

(use-package ddskk
  :ensure t
  :bind
  (("C-x j" . skk-mode))
  :config
  (setq skk-show-mode-show t)
  (setq skk-egg-like-newline t)
  (setq skk-jisyo-code 'utf-8)
  (setq skk-large-jisyo "~/.config/SKK-JISYO.L")yy
  (setq skk-server-host "localhost")
  (setq skk-server-portnum 1178))

(use-package avy
  :ensure t
  :bind
  (("C-:" . avy-goto-char-timer)
   ("C-'" . avy-goto-word-1)))


(use-package emmet-mode
  :ensure t
  :hook
  ((html-mode . emmet-mode)
   (web-mode . emmet-mode)))

(use-package puni
  :defer t
  :init
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  :bind
  (("C-M-SPC" . puni-mark-sexp-at-point)
   ("C-M-@" . puni-mark-sexp-around-point)
   ("C-c m" . puni-mark-list-around-point)
   ("C-=" . puni-expand-region)
   ("C--" . puni-contract-region)))

(use-package gruvbox-theme
  :ensure t
  ;; :config
  ;; (load-theme 'gruvbox-dark-medium t)
  )
;; (use-package mindre-theme
;;   :ensure t
;;   :vc (:url "https://github.com/erikbackman/mindre-theme" :rev :newest)
;;   :custom
;;   (mindre-use-more-bold nil)
;;   (mindre-use-faded-lisp-parens t)
;;   :config
;;   (load-theme 'mindre t))

(use-package org-modern
  :ensure t
  :after org
  :hook
  (org-mode . org-modern-mode))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package which-key
  :diminish which-key-mode
  :ensure t
  :hook (after-init . which-key-mode))

(use-package flycheck-phpstan
  :ensure t
  :hook
  (php-mode . flymake-phpstan-turn-on))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)))

(use-package wgrep
  :ensure t)
(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures 'prog-mode
                          '("==" "===" "!=" "<=" ">=" "->" "=>" "::" "&&" "||" "++" "--"))
  (global-ligature-mode t))
(use-package consult
  :ensure t
  :bind
  ("C-c M-x" . consult-mode-command)
  ("C-c h" . consult-history)
  ("C-c k" . consult-kmacro)
  ("C-c i" . consult-info)
  ("C-x b" . consult-buffer)
  ("C-x 4 b" . consult-buffer-other-window)
  ("C-x 5 b" . consult-buffer-other-frame)
  ("C-x r b" . consult-bookmark)
  ("C-x p b" . consult-project-buffer)
  ("M-g g" . consult-goto-line)
  ("M-g M-g" . consult-goto-line)
  ("M-g f" . consult-flymake)
  ("M-g o" . consult-outline)
  ("M-g m" . consult-mark)
  ("M-s d" . consult-find)
  ("M-s c" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)



  :init
  (setq register-preview-delay 0.5)
  :hook
  (completion-list-mode consult-preview-at-point-mode)
  :config
  (consult-customize
   consult-themes :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   cosult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any)
   )
  )
(use-package reformatter
  :ensure t
  :config
  (reformatter-define php-cs-fixer-format
    :program "php-cs-fixer"
    :stdin nil
    :stdout nil
    :args `("fix", "--show-progress=none" ,(my/php-mode-php-cs-fixer-config-option) ,input-file)
    :input-file (reformatter-temp-file)
    :lighter " PHPCSFIXER"
    )
  (reformatter-define eslint-typescript-format
    :program "eslint"
    :args `(,buffer-file-name "--fix")
    ;; :stdin t
    ;; :stdout nil
    ;; :input-file (filepath)
    :lighter " ESLINT-TS")
  :hook
  (php-mode . php-cs-fixer-format-on-save-mode)
  (typescript-ts-mode . eslint-typescript-format-on-save-mode))

(put 'upcase-region 'disabled nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defgroup my-file-deploy nil "")

(defcustom my-file-deploy-local-root (expand-file-name (concat (getenv "HOME") "/ghq/rpst-v2/"))
  "ローカルのルート"
  :type 'directory
  )

(defcustom my-file-deploy-remote-root "/var/www/rpst-v2/dev/"
  "リモート側のルート"
  :type 'string
  )

(defcustom my-file-deploy-remote-host "taro_morita@dev-tmorita.precs.jp"
  "SSH接続先"
  :type 'string
  )

(defcustom my-file-deploy-rsync-args
  '("-az")
  "rsyncの引数リスト"
  :type '(repeat string))

(defun my-file-deploy--in-scope-p (file)
  (and file
       (string-prefix-p (file-name-as-directory (file-truename my-file-deploy-local-root))
                        (file-truename file))))

(defun my-file-deploy--remote-path (local-file)
  "local-fileをremote-rootに対応づけたリモートパスに変換する"
  (let* ((rel (file-relative-name (expand-file-name local-file)
                                  (file-name-as-directory my-file-deploy-local-root))))
    (concat (file-name-as-directory my-file-deploy-remote-root) rel)))

(defun my-file-deploy-after-save ()
  "保存したファイルだけを転送する"
  (interactive)
  (when (my-file-deploy--in-scope-p buffer-file-name)
    (print "is target file")
    (let* ((local (expand-file-name buffer-file-name))
           (remote (my-file-deploy--remote-path local))
           (dest (format "%s:%s" my-file-deploy-remote-host remote))
           (cmd (append (list "rsync") my-file-deploy-rsync-args (list local dest))))
      (apply #'start-process "my-file-deploy" "*my-file-deploy*" cmd))))

(add-hook 'after-save-hook #'my-file-deploy-after-save)
