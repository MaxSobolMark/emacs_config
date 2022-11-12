;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; .emacs.d/init.el
;; Tab settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; Show line numbers
(global-linum-mode t)

;; mouse commands
(require 'mouse)
(xterm-mouse-mode t)

;; line by line scrolling with mouse and keyboard
;; Thanks to https://www.emacswiki.org/emacs/SmoothScrolling
(setq-default scroll-step 1)
(setq-default mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq-default mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq-default mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-conservatively 1000 scroll-preserve-screen-position 1) ;; extra smooth scrolling to avoid random jumps



;; ===================================

;; MELPA Package Support

;; ===================================

;; Enables basic packaging support

(require 'package)


;; Adds the Melpa archive to the list of available repositories

(add-to-list 'package-archives

	     '("melpa-stable" . "http://melpa.org/packages/") t)


;; Initializes the package infrastructure

(package-initialize)


;; If there are no archived package contents, refresh them

(when (not package-archive-contents)

  (package-refresh-contents))

  ;; Installs packages

;;
  ;; myPackages contains a list of package names

  (defvar myPackages

    '(
      doom-themes
      )

    )


  ;; Scans the list in myPackages

  ;; If the package listed is not already installed, install it

  (mapc #'(lambda (package)

	    (unless (package-installed-p package)

	      (package-install package)))

	myPackages)


  ;; ===================================

  ;; Basic Customization

  ;; ===================================


  (setq inhibit-startup-message t)    ;; Hide the startup message

  (global-linum-mode t)               ;; Enable line numbers globally


  ;; User-Defined init.el ends here
  
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine 'xetex)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(apheleia-formatters
   '((black "black" "-")
     (brittany "brittany")
     (clang-format "clang-format")
     (fish-indent "fish_indent")
     (gofmt "gofmt")
     (google-java-format "google-java-format" "-")
     (isort "isort" "-")
     (latexindent "latexindent")
     (mix-format "mix" "format" "-")
     (ocamlformat "ocamlformat" "-" "--name" filepath "--enable-outside-detected-project")
     (prettier npx "/Users/joelsobolmark/dev/node_modules/.bin/prettier" "--stdin-filepath" filepath)
     (rustfmt "rustfmt" "--quiet" "--emit" "stdout")
     (terraform "terraform" "fmt" "-")))
 '(apheleia-remote-algorithm 'local)
 '(avy-enter-times-out nil)
 '(avy-timeout-seconds 0.2)
 '(cdlatex-paired-parens "$[{(")
 '(copilot-node-executable "/Users/joelsobolmark/.nvm/versions/node/v17.9.1/bin/node")
 '(counsel-find-file-ignore-regexp "\\(?:\\‘[#.]\\)\\|\\(?:[#~]\\’\\)\\|\\(\\.~undo-tree~\\)$")
 '(custom-safe-themes
   '("1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "3d54650e34fa27561eb81fc3ceed504970cc553cfd37f46e8a80ec32254a3ec3" "6e14157d0c8857e81035e6c7131dc17e4115b3911c82a1fd32e528aec8e89eab" "f302eb9c73ead648aecdc1236952b1ceb02a3e7fcd064073fb391c840ef84bca" "2c49d6ac8c0bf19648c9d2eabec9b246d46cb94d83713eaae4f26b49a8183fc4" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "f94110b35f558e4c015b2c680f150bf8a19799d775f8352c957d9d1054b0a210" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "0a41da554c41c9169bdaba5745468608706c9046231bbbc0d155af1a12f32271" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" default))
 '(debug-on-error nil)
 '(debug-on-signal nil)
 '(ein:output-area-inlined-images t)
 '(exwm-floating-border-color "#011417")
 '(fci-rule-color "#405A61")
 '(highlight-tail-colors
   ((("#0d3630" "#0f393a" "green")
     . 0)
    (("#04363f" "#073c4a" "brightcyan")
     . 20)))
 '(ivy-initial-inputs-alist
   '((counsel-minor . "^+")
     (counsel-package . "^+")
     (counsel-org-capture . "")
     (counsel-M-x . "")
     (counsel-describe-symbol . "")
     (org-refile . "")
     (org-agenda-refile . "")
     (org-capture-refile . "")
     (Man-completion-table . "")
     (woman . "^")))
 '(jdee-db-active-breakpoint-face-colors (cons "#073642" "#268bd2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#073642" "#859900"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#073642" "#56697A"))
 '(jedi:environment-virtualenv '("~/miniconda3/bin/virtualenv"))
 '(latex-preview-pane-multifile-mode 'auctex)
 '(objed-cursor-color "#dc322f")
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-image-actual-width nil)
 '(package-selected-packages
   '(diminish slack el-get request-deferred oauth2 circe alert restart-emacs add-node-modules-path prettier-js flycheck dashboard apheleia tree-sitter jedi diff-hl counsel-tramp counsel helm-dash helm cdlatex auctex-latexmk undo-tree company-auctex latex-preview-pane bash-completion ein ztree auctex ace-window avy yasnippet-snippets company-tabnine pyenv-mode elpy company-jedi doom-themes spacemacs-theme company))
 '(pdf-latex-command "lualatex")
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(pyvenv-activate "~/dev/emacs_pyenv_3.9")
 '(pyvenv-workon nil)
 '(set-mark-command-repeat-pop t)
 '(shell-escape-mode "-shell-escape")
 '(tab-width 4)
 '(tramp-connection-timeout 25)
 '(tramp-remote-path
   '(tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin" "/sailhome/maxsobolmark/defaultenv/bin"))
 '(tramp-verbose 3)
 '(undo-tree-auto-save-history nil)
 '(warning-suppress-types '((pdf-view))))
  
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "#d4d4d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Menlo")))))

  ;; Initialize company-jedi
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)
(add-hook 'python-mode-hook (apply-partially 'company-mode -1))

;; Always start smartparens mode in python-mode
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'LaTeX-mode-hook #'smartparens-mode)
(add-hook 'plain-tex-mode #'smartparens-mode)
(smartparens-global-mode t)


  ;; Load theme
  (load-theme 'doom-dark+)

  (elpy-enable)
(require 'pyvenv)

;; (require 'company-tabnine)
;; (add-to-list 'company-backends #'company-tabnine)
  
(pyvenv-workon ".")


;; CUSTOM KEYBINDINGS:
(global-set-key (kbd "C-x #") 'comment-line)
(global-set-key (kbd "C-M-k") 'kill-whole-line)
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )
(global-set-key (kbd "C-c d") 'duplicate-line)
;; END CUSTOM KEYBINDINGS
(setq shift-selection-mode t)

;; Set shift-tab to remove 4 spaces from the front of the current line.
(global-set-key (kbd "<S-tab>") 'un-indent-by-removing-4-spaces)
(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
                (replace-match "")))))

;; Autoformat python on save
;; (add-hook 'elpy-mode-hook (lambda ()
;;                             (add-hook 'before-save-hook
;;                                       'elpy-format-code nil t)))
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
;; (global-set-key (kbd "C-c i") 'move-line-up)
;; (global-set-key (kbd "C-c o") 'move-line-down)
(defhydra hydra-move-line (global-map "C-c")
  "move-line"
  ("i" move-line-up "up")
  ("o" move-line-down "down"))


(show-paren-mode 1)

(defun do-org-show-all-inline-images ()
  (interactive)
  (org-display-inline-images t t))
(global-set-key (kbd "C-c C-x C v")
                                'do-org-show-all-inline-images)

(menu-bar-mode -1)

(require 'multiple-cursors)
;; (autoload 'multiple-cursors "multiple-cursors" "multiple-cursors")
;; (defhydra hydra-mark-like-this (global-map "C-c")
;;   "mark-like-this"
;;   (")" mc/mark-next-like-this "mark next")
;;   ("(" mc/mark-previous-like-this "mark previous")
;;   ;; ("?" mc/mark-all-like-this "mark all"))
;;   )
;; mc/num-cursors is not autoloaded
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
;; (global-set-key (kbd "C-c )") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-c (") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-0") 'mc/mark-next-lines)
;; (global-set-key (kbd "C-c C-9") 'mc/mark-previous-lines)


(defun sshcs ()
  (interactive)
  (find-file "/ssh:maxsobolmark@sc.stanford.edu:/")
  )
(defun sshcs-reconnect ()
  (interactive)
  (tramp-cleanup-all-connections)
  (find-file "/ssh:maxsobolmark@sc.stanford.edu:/")
  )
(defun sshws ()
  (interactive)
  (find-file "/ssh:maxsobolmark@iris-ws-7.stanford.edu:/"))
(defun initel ()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )

(setq visible-bell 1)

(global-set-key (kbd "M-p") 'ace-window)
(global-set-key (kbd "M-<escape>") 'ace-window)
;; (global-set-key (kbd "<f3>") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; Auctex config
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-to-list 'load-path "~/.emacs.d/packages")
(load "ediff-trees.el")

(setq ztree-diff-filter-list '("^\\." ".?\.pyc$" ".?pycache.?"))

(autoload 'yaml-mode "yaml-mode" "Yaml mode")
;; (require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(add-to-list 'load-path "~/.emacs.d/lib/desktop-environment/")
;; (require 'desktop-environment)
(autoload 'desktop-environment "desktop-environment" "Desktop environment")

(global-set-key (kbd "C-c M-c") 'copy-file)
(global-set-key (kbd "C-c M-d") 'copy-directory)
(global-set-key (kbd "C-c `") 'ediff-buffers)

(global-set-key (kbd "C-x p") 'ein:notebooklist-login)

(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)

(global-set-key (kbd "C-c /") 'comment-region)
(global-set-key (kbd "C-c ?") 'uncomment-region)

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

(autoload 'notmuch "notmuch" "Notmuch mail" t)

(setq latex-run-command "pdflatex")
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook
      (lambda()
        (local-set-key [C-tab] 'TeX-complete-symbol)))

(autoload 'org "org" "org mode")
;; (require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files '("~/org"))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/RoamNotes"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+STARTUP: latexpreview\n%^g")
      :unnarrowed t)
     ("p" "paper notes" plain (file "~/RoamNotes/org/Paper notes template.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("h" "person notes" plain (file "~/RoamNotes/org/Person notes template.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     )
   )
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         :map org-mode-map
         ("C-M-i"   . completion-at-point))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(latex-preview-pane-enable)

(ivy-mode)
(setq ivy-re-builders-alist
      '((org-roam-node-find . ivy--regex-ignore-order)
        (t . ivy--regex-plus)))

(add-to-list 'load-path "~/.emacs.d/elpa/company-auctex-20200529.1835/company-auctex.el")
(require 'company-auctex)
(company-auctex-init)
(add-hook 'LaTeX-mode-hook 'company-mode)
(add-hook 'plain-tex-mode 'company-mode)
(add-hook 'org-mode-hook 'company-mode)
(add-hook 'fundamental-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

(global-undo-tree-mode)

(global-visual-line-mode t)

(with-eval-after-load 'smartparens
  (sp-local-pair '(python-mode) "f\"" "\"")
  (sp-local-pair '(python-mode) "f'" "'"))

(global-set-key (kbd "s-<tab>") 'other-frame)
(setenv "PATH"
        (concat "/Library/TeX/texbin/" ":" (getenv "PATH")))
(setenv "PATH"
        (concat "/opt/homebrew/bin/" ":" (getenv "PATH")))
(setq exec-path (append exec-path '("/Library/TeX/texbin/")))
(setq exec-path (append exec-path '("/opt/homebrew/bin/")))

;; (setenv "WORKON_HOME" "~/dev/emacs_pyenv")
(setenv "WORKON_HOME" "~/dev/emacs_pyenv_3.9")
;; (setenv "VIRTUAL_ENV" "~/dev/emacs_pyenv")
(setenv "VIRTUAL_ENV" "~/dev/emacs_pyenv_3.9")

(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'python-mode-hook #'auto-fill-mode)
(setq-default fill-column 100)

(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-cdlatex)   ; with Emacs latex mode
(add-hook 'org-mode-hook 'turn-on-cdlatex)   ; with org mode

(add-hook 'TeX-mode-hook 'prettify-symbols-mode)
(add-hook 'org-mode-hook 'prettify-symbols-mode)


(setq org-roam-ui-sync-theme t
      org-roam-ui-follow t
      org-roam-ui-update-on-save t
      org-roam-ui-open-on-start t)
;; ; Set relative line numbers
;; (global-display-line-numbers-mode)
;; (setq display-line-numbers 'relative)
;; (setq display-line-numbers-type 'relative)
;; (global-linum-mode 0)

(defun my-insert-before-line ()
  (interactive)
  ; Bring cursor to start of line (plus indentation)
  (back-to-indentation)
  ; Copy indentation
  (set-mark (point))
  (beginning-of-line)
  (call-interactively #'kill-ring-save)
  ; Add new line and indentation
  (open-line 1)
  (yank)
  (rotate-yank-pointer 1))
(global-set-key (kbd "C-o") 'my-insert-before-line)

(defun copy-region-or-line ()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (kill-ring-save nil nil t)  ; There's something selected, so just copy
      (progn
        (back-to-indentation) (set-mark (point)) (end-of-visual-line) (kill-ring-save nil nil t))
      )
    ))
(global-set-key (kbd "M-w") 'copy-region-or-line)

(defun yank-in-place ()
  (interactive)
  (save-excursion
    (call-interactively #'avy-goto-char)
    (yank)))
(global-set-key (kbd "C-M-y") 'yank-in-place)

;; (global-unset-key (kbd "C-x C-b"))


(put 'downcase-region 'disabled nil)

(use-package lsp-mode
  :custom
  (lsp-enable-snippet t))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; (lsp-register-client
;;     (make-lsp-client :new-connection (lsp-tramp-connection (lambda ()
;;                                                              (cons "pyright-langserver"
;;                                                                    lsp-pyright-langserver-command-args)))
;;                      :major-modes '(python-mode)
;;                      :remote? t
;;                      :server-id 'pyright-remote
;;                      :initialization-options (lambda () (let* ((pyright_hash (lsp-configuration-section "pyright"))
;;                                                                (python_hash (lsp-configuration-section "python"))
;;                                                                (_ (puthash "pythonPath" (concat (replace-regexp-in-string (file-remote-p default-directory) "" "/sailhome/maxsobolmark/defaultenv/bin") "bin/python") (gethash "python" python_hash))))
;;                                                           (ht-merge pyright_hash
;;                                                                     python_hash)))
;;                      :initialized-fn (lambda (workspace)
;;                                        (with-lsp-workspace workspace
;;                                          (lsp--set-configuration
;;                                           (let* ((pyright_hash (lsp-configuration-section "pyright"))
;;                                                  (python_hash (lsp-configuration-section "python"))
;;                                                  (_ (puthash "pythonPath" (concat (replace-regexp-in-string (file-remote-p default-directory) "" "/sailhome/maxsobolmark/defaultenv/bin") "bin/python") (gethash "python" python_hash))))
;;                                             (ht-merge pyright_hash
;;                                                       python_hash)))))
;;                      ))


;; (add-hook 'lsp-mode-hook 'lsp-headerline--disable-breadcrumb)
(setq lsp-headerline-breadcrumb-enable nil)

(global-set-key (kbd "M-n") 'avy-goto-char)
(global-set-key (kbd "<escape>") 'avy-goto-char)
;; (global-set-key (kbd "M-n") 'avy-goto-char-2)

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)
(global-set-key (kbd "C-7") 'swiper-mc)
(add-to-list 'mc/cmds-to-run-once 'swiper-mc)
(setq dash-docs-enable-debugging nil)
(setq helm-dash-browser-func 'eww)
(setq dash-docs-browser-func 'eww)

(global-set-key (kbd "C-h d") 'helm-dash)
(global-set-key (kbd "M-`") 'projectile-find-file)

(autoload 'org-roam-export "org-roam-export" "org-roam-export")
;; (require 'org-roam-export)

(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
  ; take screenshot
  (if (eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" filename))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
  ; insert into file if correctly taken
  (if (file-exists-p filename)
    (insert (concat "[[file:" filename "]]"))))

(defun org-insert-clipboard-image (&optional file)
  (interactive "F")
  (shell-command (concat "pngpaste " file))
  (insert (concat "[[" file "]]"))
  (org-display-inline-images))

(setq dash-docs-common-docsets '("PyTorch" "NumPy" "PytorchLightning" "Python 3"))

(defun kill-line-backwards ()
  (interactive)
  (set-mark (point))
  (back-to-indentation)
  (kill-region nil nil t))

(global-set-key (kbd "C-S-k") 'kill-line-backwards)

(defun fast-comment ()
  (interactive)
  (if mark-active
      (comment-region (region-beginning) (region-end))
    (insert "/")
    ))
(global-set-key (kbd "/") 'fast-comment)

(defun fast-uncomment ()
  (interactive)
  (if mark-active
      (uncomment-region (region-beginning) (region-end))
    (insert "?")
    ))
(global-set-key (kbd "?") 'fast-uncomment)


(defun fast-copy ()
  (interactive)
  (if mark-active
      (copy-region-or-line)
    (insert "w")
    ))
(defun fast-kill ()
  (interactive)
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (insert "k")
    ))
(global-set-key (kbd "w") 'fast-copy)
(global-set-key (kbd "k") 'fast-kill)

(add-hook 'shell-mode-hook (lambda () (progn
                                        (local-set-key (kbd "C-M-p") 'ace-window)
                                        (local-set-key (kbd "C-M-n") 'avy-goto-char))))
(add-hook 'eaf-mode-hook (local-set-key (kbd "C-M-p") 'ace-window))

(global-set-key (kbd "C-.") (lambda () (interactive) (push-mark (point) nil nil)))
(global-set-key (kbd "C-c .") 'helm-mark-ring)

;; Counsel config
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(setq create-lockfiles nil)


(defun max-python-tab ()
  "This function is based on cdlatex-tab, intended to make cursor movements easier while writing python."
  (interactive)
  (catch 'stop
  ;; move to next possible stopping site and check out the place
    (while t
      (cond
       ;; ((= (following-char) ?\ )
       ;;  ;; stop at first space or b-o-l
       ;;  (if (not (bolp)) (forward-char 1)) (throw 'stop t))
       ;; ((= (following-char) ?\n)
       ;;  ;; stop at line end, but not after \\
       ;;  (if (and (bolp) (not (eobp)))
       ;;      (throw 'stop t)
       ;;    (if (equal "\\\\" (buffer-substring-no-properties
       ;;                       (- (point) 2) (point)))
       ;;        (forward-char 1)
       ;;      (throw 'stop t))))
       ;; (t
       ;;  ;; Stop before )}] if preceding-char is any parenthesis
       ;;  (if (or (= (char-syntax (preceding-char)) ?\()
       ;;          (= (char-syntax (preceding-char)) ?\))
       ;;          (= (preceding-char) ?-))
       ;;      (throw 'stop t)
       ;;    (forward-char 1)
       ;;    (if (looking-at "[^_\\^({\\[]")
       ;;        ;; stop after closing bracket, unless ^_[{( follow
       ;;        (throw 'stop t)))
       ;; Stop after char
       ((memq (char-after) (list ?\. ?\( ?= ?, ?% ?^ ?+ ?- ?* ?^ ?/ ?\) ?' ?\] ?\" ))
        (progn (forward-char) (throw 'stop t)))
       
       ;; ((memq (char-after) (list ?= ?- ?+ ?\.))
       ;;  (throw 'stop t))
       (t
        (forward-char 1))
       ;; ((= (preceding-char) ?\s)
       ;;  (if (bolp) (forward-char 1) (throw 'stop t)))
       )))
  ;; If the line we're at is a comment start over
  (save-excursion
    (back-to-indentation)
    (if (= (char-after) ?#)
        (setq rerun t)
      (setq rerun nil)
      )
    )
  (if rerun
      (max-python-tab))
  )
(global-set-key (kbd "TAB") 'max-python-tab)
;; (global-set-key (kbd "C-i") 'indent-for-tab-command)
(define-key input-decode-map [?\C-i] [C-i])
(global-set-key (kbd "<C-i>") 'indent-for-tab-command)
(global-set-key (kbd "<f9>") 'make-frame)
(global-diff-hl-mode)

(add-hook 'python-mode-hook 'jedi:setup)

;; Javascript stuff
(autoload 'js2-mode "js2-mode" "js2-mode")
;; (require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook 'company-mode)

(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(apheleia-global-mode +1)

(require 'dashboard)
(dashboard-setup-startup-hook)
;; Or if you use use-package
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; (use-package smart-hungry-delete
;;   :ensure t
;;   :bind (("<backspace>" . smart-hungry-delete-backward-char)
;; 		 ("C-d" . smart-hungry-delete-forward-char))
;;   :defer nil ;; dont defer so we can add our functions to hooks 
;;   :config (smart-hungry-delete-add-default-hooks)
;;   )
(use-package smart-hungry-delete
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
	     ([remap delete-backward-char] . smart-hungry-delete-backward-char)
	     ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))
(put 'upcase-region 'disabled nil)

(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 4))

(add-hook 'web-mode-hook  'web-mode-init-hook)

(setq restart-emacs-restore-frames t)

(global-set-key (kbd "C-c <backspace>") 'sp-unwrap-sexp)

(add-hook 'python-mode-hook
          (lambda () (progn
                       (local-set-key (kbd "C-M-n") #'python-nav-end-of-block)
                       (local-set-key (kbd "C-M-p") #'python-nav-beginning-of-block)
                       (local-set-key (kbd "C-t") #'python-nav-forward-defun)
                       (local-set-key (kbd "M-t") #'python-nav-backward-defun)
                       )))
(pdf-loader-install)

(delete-selection-mode 1)

(global-unset-key (kbd "ESC ESC ESC"))

(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(autoload 'eaf "eaf" "eaf")
;; (require 'eaf)
(autoload 'eaf-demo "eaf-demo" "eaf-demo")
;; (require 'eaf-demo)
(autoload 'eaf-image-viewer "eaf-image-viewer" "eaf-image-viewer")
;; (require 'eaf-image-viewer)
(autoload 'eaf-pdf-viewer "eaf-pdf-viewer" "eaf-pdf-viewer")
;; (require 'eaf-pdf-viewer)
(autoload 'eaf-browser "eaf-browser" "eaf-browser")
;; (require 'eaf-browser)
(autoload 'eaf-mindmap "eaf-mindmap" "eaf-mindmap")
;; (require 'eaf-mindmap)

;; (global-set-key (kbd "<f4>") 'eaf-open-browser-with-history)

(setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            ;; #'org-roam-unlinked-references-section
            ))


;; (el-get-bundle slack)
;; (el-get-bundle yuya373/helm-slack) ;; optional
;; (use-package helm-slack :after (slack)) ;; optional
;; (use-package slack
;;   :commands (slack-start)
;;   :init
;;   (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
;;   (setq slack-prefer-current-team t)
;;   :config
;;   (slack-register-team
;;    :name "emacs-slack"
;;    :default t
;;    :token "xoxc-736620684049-519206784643-3585091278438-d54346714a1791e693a6947e69192ca18e9d38f3bc0108f79e70a2034590600e"
;;    :cookie "xoxd-T6WI4QRxlPu8zLCVZoi0z%2BJ4%2FvGf66L6SH6EzzMANhDKvZ66Iu8bcb4VPsbuuxKdVAZo7M4rRvPhjGQQNT7T077Zjt55KgcARQAMe72uqNcAFoKmyMxFWkgyvxD9aNF1%2FJVHhpmXG0TbZ4Q8WchvJYwxUbQvBHJYpkAmK7BVXRFf1g72PRWwxcWB"
;;    :subscribed-channels '(random)
;;    :full-and-display-names t)

;;   ;; (slack-register-team
;;   ;;  :name "test"
;;   ;;  :token "xoxs-yyyyyyyyyy-zzzzzzzzzzz-hhhhhhhhhhh-llllllllll"
;;   ;;  :subscribed-channels '(hoge fuga))
;; )

;; (use-package alert
;;   :commands (alert)
;;   :init
;;   (setq alert-default-style 'notifier))

(setq avy-all-windows 'all-frames)

(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))

(require 'diminish)
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

(defun clear-kill-ring()
  (interactive)
  (setq kill-ring nil)
  (message "Cleared the kill ring.")
  )

(defun python-selection()
  (interactive)
  (if (use-region-p)
      (progn
        (setq s (region-beginning))
        (setq e (region-end))
        (goto-char (region-beginning))
        (insert "print(")
        (goto-char (region-end))
        (insert ")")
        (deactivate-mark)
        (set-mark (point))
        (backward-sexp)
        (backward-sexp)
        (setq current-prefix-arg '(4)) ; C-u
        (shell-command-on-region (region-beginning) (region-end) "python" nil t)
        (message "Done"))
    (message "Nothing selected"))
  )

(global-set-key (kbd "C-|") (lambda () (interactive) (python-selection)))

(global-set-key (kbd "<f3>") (lambda () (interactive)
                               (progn
                                 (split-window-right)
                                 (other-window 1))))

(global-set-key (kbd "<f2>") (lambda () (interactive)
                               (progn
                                 (split-window-below)
                                 (other-window 1)
                                 )))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)
(add-hook 'prog-mode-hook 'copilot-mode)
(global-set-key (kbd "<f4>") 'copilot-complete)
(global-set-key (kbd "<f5>") 'copilot-next-completion)
(global-set-key (kbd "<f6>") 'copilot-clear-overlay)
(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "<f4>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "<f4>") 'copilot-accept-completion)
