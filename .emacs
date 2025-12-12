;; -*- lexical-binding: t; -*-

(let ((mono-spaced-font "Monospace")
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 140)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

(setq custom-file (locate-user-emacs-file "custome.el"))
(load custom-file :no-error-file-is-missing)

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

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

(use-package modus-themes
  :ensure t
  :demand t
  :config
  (load-theme 'modus-vivendi-tinted :no-confirm-loading))

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

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))


(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (style basic partial-completion)))))


(use-package savehist
  :ensure nil ;; because it is built-in
  :hook (after-init . savehist-mode))

(use-package corfu
  :ensure t
  :bind (:map corfu-map ("<tab>" . corfucomplete))
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

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package dired
  :ensure nil
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
   (php-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t)
  :bind (:map eglot-mode-map
	      ("C-c a" . eglot-code-actions)
	      ("C-c r" . eglot-rename))
  :config
  (add-to-list 'eglot-server-programs
	       '((php-mode) . ("intelephense" "--stdio"))))

(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

(use-package eat :ensure t)
(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

(use-package vterm :ensure t)
(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)
  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

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

(use-package git-commit)
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
	 (text-mode . diff-hl-mode)
	 (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package blamer
  :ensure t
  :hook (after-init . blamer-mode)
  :custom
  (blamer-idle-time 0.6)
  (blamer-min-offset 20))



