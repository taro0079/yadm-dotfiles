
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(setenv "LIBRARY_PATH" "/opt/homebrew/lib/gcc/14/:/opt/homebrew/lib/gcc/14/gcc/aarch64-apple-darwin23/14")
(exec-path-from-shell-initialize)
(add-to-list 'exec-path "/Users/taro_morita/.volta/bin")

; (setq custom-file "~/.emacs.custom.el")

;; key bindings
(global-set-key "\C-t" 'other-window)


(add-to-list 'default-frame-alist `(font . "Iosevka Nerd Font-18"))
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
;(ido-mode 1)
;(ido-everywhere 1)
(setq ido-show-dot-for-dired t)
(global-display-line-numbers-mode)

; (load-file custom-file)

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

(defun require-theme (theme)
  (let ((theme-package (->> theme
			    (symbol-name)
			    (funcall (-flip #'concat) "-theme")
			    (intern))))
    (rc-require theme-package)
    (load-theme theme t)))

(rc-require 'php-mode)
(require 'php-mode)
(rc-require 'leaf)

(leaf ruby-mode
      :ensure t)

(leaf eglot
  :ensure t
  :require t)

(leaf eglot-booster
  :when (executable-find "emacs-lsp-booster")
  :vc ( :url "https://github.com/jdtsmith/eglot-booster")
  :global-minor-mode t)
			 
(leaf vertico
  :ensure t
  :global-minor-mode t)

;(use-package vertico-directory
;  :ensure t
;  :after vertico
;  :bind (:map vertico-map
;	      ("C-l" . vertico-directory-up)
;	      ("\\d" . vertico-directory-delete-char)))
;  )

(leaf csv-mode
  :ensure t)

(leaf go-mode
  :ensure t)

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

(leaf ripgrep
  :ensure t)

(leaf dracula-theme
  :ensure t)
(leaf multiple-cursors
  :ensure t)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(load-theme 'dracula t)
