(use-package vertico
  :init
  (vertico-mode)
  ;;:custom
  ;;(vertico-cycle t)
  )


(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :ensure t
  :bind (
         ("C-<tab>" . consult-buffer)
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ;; C-c bindings in `mode-specific-map'
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ("M-s g" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)
         ("C-S" . consult-line)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (setq consult-preview-key 'any)

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any)
   )

  ;; Disable previews for file-related sources in consult-buffer
  (consult-customize
   consult-buffer
   consult--source-buffer
   consult--source-recent-file
   consult--source-project-recent-file
   consult--source-bookmark
   :preview-key "M-.")

  ;; Add center-on-candidate functionality to consult-line
  ;; (defun my/consult-line-center ()
  ;;   "Center the current candidate line in the buffer."
  ;;   (interactive)
  ;;   (let ((candidate (car (consult--lookup-candidate))))
  ;;     (when candidate
  ;;       (with-selected-window (consult--get-window)
  ;;         (goto-char candidate)
  ;;         (recenter)))))

  ;; (consult-customize
  ;;  consult-line
  ;;  :add-history (list #'my/consult-line-center)
  ;;  :preview-key (append consult-preview-key
  ;;                       (list (kbd "C-l") #'my/consult-line-center)))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key "e")
  )

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil) ;; No specific categories have special completion styles by default
  ;;  (completion-category-overrides '((file (styles)))) ;; Allow partial-completion for file paths
  :config
  (setq orderless-matching-styles '(orderless-literal-prefix
									orderless-literal
                                    orderless-regexp))

  )


(use-package savehist
  :init
  (savehist-mode))


;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
			  ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))


(use-package embark
  :ensure t

  :bind
  ;; (("C-." . embark-act)         ;; pick some comfortable binding
  (("C-\"" . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t)
  (sp-local-pair '(python-mode) "f\"" "\"")
  (sp-local-pair '(python-mode) "f'" "'")

  (add-hook 'python-mode-hook #'smartparens-mode)
  (add-hook 'LaTeX-mode-hook #'smartparens-mode)
  (add-hook 'plain-tex-mode #'smartparens-mode)
  )

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-move-line (global-map "C-c")
	"move-line"
	("i" move-line-up "up")
	("o" move-line-down "down"))
  
  )

(use-package multiple-cursors
  :ensure t
  :config
  (defhydra hydra-multiple-cursors (global-map "C-c )")
	"
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
 [_|_] Align with input CHAR       [Click] Cursor at point"
	("l" mc/edit-lines :exit t)
	("a" mc/mark-all-like-this :exit t)
	("n" mc/mark-next-like-this)
	("N" mc/skip-to-next-like-this)
	("M-n" mc/unmark-next-like-this)
	("p" mc/mark-previous-like-this)
	("P" mc/skip-to-previous-like-this)
	("M-p" mc/unmark-previous-like-this)
	("|" mc/vertical-align)
	("s" mc/mark-all-in-region-regexp :exit t)
	("0" mc/insert-numbers :exit t)
	("A" mc/insert-letters :exit t)
	("<mouse-1>" mc/add-cursor-on-click)
	;; Help with click recognition in this hydra
	("<down-mouse-1>" ignore)
	("<drag-mouse-1>" ignore)
	("C-n" mc/mark-next-lines)
	("C-p" mc/mark-previous-lines)
	("q" nil))
  )

(use-package ace-window
  :ensure t
  :bind (("M-<escape>" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )

(use-package undo-tree
  :ensure t
  :init
  ;; Enable undo-tree mode globally
  (global-undo-tree-mode))

(use-package avy
  :ensure t
  :bind (("<escape>" . avy-goto-char))
  :config
  (setq avy-all-windows 'all-frames)
  )
;; (use-package corfu
;;   :custom
;;   (corfu-auto t)          ;; Enable auto completion
;;   ;; (corfu-separator ?_) ;; Set to orderless separator, if not using space
;;   :bind
;;   ;; Another key binding can be used, such as S-SPC.
;;   ;; (:map corfu-map ("M-SPC" . corfu-insert-separator))
;;   :init
;;   (global-corfu-mode))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package smart-hungry-delete
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
	     ([remap delete-backward-char] . smart-hungry-delete-backward-char)
	     ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks)
  :config
  (put #'smart-hungry-delete-backward-char 'delete-selection nil)
  (put #'smart-hungry-delete-forward-char 'delete-selection nil)
  )


(use-package diminish
  :ensure t
  :config
  (diminish 'undo-tree-mode)
  (diminish 'latex-mode)
  (diminish 'tex-mode)
  (diminish 'latex-preview-pane-mode)
  (diminish 'cdlatex-mode)
  (diminish 'ivy-mode)
  (diminish 'projectile-mode)
  (diminish 'apheleia-mode)
  (diminish 'smartparens-mode)
  (diminish 'auto-fill-mode)
  (diminish 'visual-line-mode)
  )

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  )

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
			:rev :newest
			:branch "main")
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (add-to-list 'copilot-major-mode-alist '("python-ts-mode" . "python"))
  (add-to-list 'copilot-major-mode-alist '("ELisp/d" . "elisp"))
  (add-to-list 'copilot-major-mode-alist '("emacs-lisp-mode" . "elisp"))
  (add-to-list 'copilot-major-mode-alist '("lisp-interaction-mode" . "elisp"))
  (add-to-list 'copilot-indentation-alist '(prog-mode . 2))
  (add-to-list 'copilot-indentation-alist '(text-mode . 2))
  )

(use-package vterm
  :ensure t
  :config
  (add-hook 'vterm-mode-hook 'my/vterm-mode-customization)
  (with-eval-after-load 'vterm
	(define-advice vterm-copy-mode (:after (&rest r) my-customize-vterm-copy-mode-line)
      "Customize mode line when entering or exiting `vterm-copy-mode'."
      (if vterm-copy-mode
          (my/vterm-copy-mode-customization)
		(my/vterm-mode-customization))))
  )


(use-package magit
  :ensure t
  )

;; (use-package forge
;; :after magit)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-'")
  ;; Add this to completely disable LSP over TRAMP
  (setq lsp-file-watch-threshold nil)
  (setq lsp-enable-file-watchers nil)
  (defun my/should-enable-lsp ()
	"Return t if LSP should be enabled for the current buffer."
	(and (not (file-remote-p default-directory))
		 (or (derived-mode-p 'python-mode)
			 (derived-mode-p 'python-ts-mode))))
  :hook (
         ;; Replace individual mode hooks with a general programming mode hook
         (python-ts-mode . (lambda ()
							 (when (my/should-enable-lsp)
							   (lsp))
							 ;; (lsp)
							 ))
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; Add this to prevent LSP from trying to work on remote files
  (add-to-list 'lsp-disabled-clients '(python-ls . ((lambda (ws) 
                                                      (file-remote-p (buffer-file-name))))))
  ;; Bindings for LSP
  (define-key lsp-mode-map (kbd "C-' C-'") #'lsp-next-reference)
  

  )

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package consult-lsp
  :ensure t
  :bind
  (:map lsp-mode-map
		([remap xref-find-apropos] . consult-lsp-symbols)
		([remap xref-find-references] . consult-lsp-references)
		([remap xref-find-definition] . consult-lsp-definition)
		([remap xref-pop-marker-stack] . consult-lsp-history)))

(use-package pyvenv
  :ensure t
  :defer t
  :diminish
  :config
  (setenv "WORKON_HOME" "/Users/maxsobolmark/dev/emacs_pyvenvs")
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode t)
  (setq pyvenv-default-virtual-env-name "python312")
  :init
  (pyvenv-activate "/Users/maxsobolmark/dev/emacs_pyvenvs/python312")
  )

(use-package unison-sync-mode
  :vc (:url "https://github.com/jsigman/unison-sync-mode.git"
            :rev :newest
            :branch "main")
  )


(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1)
  
  ;; ;; Configure formatters for different languages
  ;; (setq apheleia-formatters
  ;;       '((python-mode . "black")
  ;;         (js-mode . "prettier")
  ;;         (css-mode . "prettier")
  ;;         (html-mode . "prettier")
  ;;         (json-mode . "prettier")
  ;;         (typescript-mode . "prettier")
  ;;         (rust-mode . "rustfmt")))

  ;; Add more language-specific configurations as needed
  )

(use-package diff-hl
  :ensure t)

(use-package async
  :ensure t)

(use-package doom-themes
  :ensure t)

(use-package swiper
  :ensure t)

(use-package copilot-chat)

(use-package ztree)

(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))

(use-package auctex
  :ensure t
  :defer t
  :config
  ;; Enable PDF mode by default
  (setq TeX-PDF-mode t)

  ;; Enable RefTeX mode for cross-referencing
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

  ;; Use modern TeX engines by default
  (setq TeX-engine 'luatex)

  ;; Automatically parse documents for preview
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  ;; Set up preview-latex for inline images
  (setq preview-scale-function 1.3)

  ;; Use a more sophisticated compilation process
  (setq TeX-command-extra-options "-shell-escape")
  (setq TeX-save-query nil)
  (setq TeX-show-compilation t))

(use-package latex-preview-pane
  :ensure t
  :config
  (latex-preview-pane-enable)
  )

(use-package yaml-mode)
