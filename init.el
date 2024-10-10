(require 'package)

(add-to-list 'package-archives

	         '("melpa-stable" . "http://melpa.org/packages/") t)

(package-initialize)

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(load "~/.emacs.d/custom-variables.el")
(load "~/.emacs.d/package-installations.el")
(load "~/.emacs.d/custom-functions.el")
(setq custom-file "~/.emacs.d/custom-variables.el")
(load "~/.emacs.d/keybindings.el")
(load "~/.emacs.d/packages/mebatch-mode.el")
(load "~/.emacs.d/packages/ediff-trees.el")
(setq ztree-diff-filter-list '("^\\." ".?\.pyc$" ".?pycache.?"))
(load "~/.emacs.d/packages/consult-patch.el")



(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(pixel-scroll-precision-mode)

(menu-bar-mode -1)
(setq visible-bell 1)


;; (autoload 'yaml-mode "yaml-mode" "Yaml mode")
;; (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(global-visual-line-mode t)


(add-hook 'python-mode-hook #'auto-fill-mode)
(setq-default fill-column 100)
(setq create-lockfiles nil)

(global-diff-hl-mode)


(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     ;; (yaml "https://github.com/ikatyang/tree-sitter-yaml")
	 ))

(setq major-mode-remap-alist
	  '(
		;; (yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(delete-selection-mode 1)

(put 'narrow-to-region 'disabled nil)

(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)
(setq frame-resize-pixelwise t)
(setq scroll-preserve-screen-position t)

;; Use zsh instead of bash for completions
'(explicit-shell-file-name "/bin/zsh")
'(explicit-zsh-args '("--interactive" "--login"))
;; Disable echoed commands
'(comint-process-echoes 0)

;; Put all backup files in a single directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backup_files")))

(setq magit-refresh-status-buffer nil)
(remove-hook 'server-switch-hook 'magit-commit-diff)
(remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)
(customize-set-variable
 'tramp-ssh-controlmaster-options
 (concat
  "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
  "-o ControlMaster=auto -o ControlPersist=yes"))

(load-theme 'doom-dark+)

;; Auto enable which-key
(which-key-mode)
;; Diminish which-key
(diminish 'which-key-mode)

