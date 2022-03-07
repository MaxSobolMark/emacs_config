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
 '(custom-safe-themes
   '("1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "3d54650e34fa27561eb81fc3ceed504970cc553cfd37f46e8a80ec32254a3ec3" "6e14157d0c8857e81035e6c7131dc17e4115b3911c82a1fd32e528aec8e89eab" "f302eb9c73ead648aecdc1236952b1ceb02a3e7fcd064073fb391c840ef84bca" "2c49d6ac8c0bf19648c9d2eabec9b246d46cb94d83713eaae4f26b49a8183fc4" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "f94110b35f558e4c015b2c680f150bf8a19799d775f8352c957d9d1054b0a210" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "0a41da554c41c9169bdaba5745468608706c9046231bbbc0d155af1a12f32271" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" default))
 '(ein:output-area-inlined-images t)
 '(exwm-floating-border-color "#011417")
 '(fci-rule-color "#405A61")
 '(highlight-tail-colors
   ((("#0d3630" "#0f393a" "green")
     . 0)
    (("#04363f" "#073c4a" "brightcyan")
     . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#073642" "#268bd2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#073642" "#859900"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#073642" "#56697A"))
 '(latex-preview-pane-multifile-mode 'auctex)
 '(objed-cursor-color "#dc322f")
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(package-selected-packages
   '(org-roam-ui cdlatex auctex-latexmk pdf-tools undo-tree company-auctex org-roam use-package latex-preview-pane hydra bash-completion ein yaml-mode ztree auctex ace-window multiple-cursors magit avy ivy projectile yasnippet-snippets company-tabnine pyenv-mode elpy smartparens company-jedi doom-themes spacemacs-theme company))
 '(pdf-latex-command "lualatex")
 '(pyvenv-workon nil)
 '(shell-escape-mode "-shell-escape"))
  
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

  ;; Initialize company-jedi
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))

  (add-hook 'python-mode-hook 'my/python-mode-hook)

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
(add-hook 'elpy-mode-hook (lambda ()
                            (add-hook 'before-save-hook
                                                                            'elpy-format-code nil t)))
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

(global-set-key (kbd "M-n") 'avy-goto-char)
;; (global-set-key (kbd "M-n") 'avy-goto-char-2)

(defun do-org-show-all-inline-images ()
  (interactive)
  (org-display-inline-images t t))
(global-set-key (kbd "C-c C-x C v")
                                'do-org-show-all-inline-images)

(menu-bar-mode -1)

(require 'multiple-cursors)
;; (defhydra hydra-mark-like-this (global-map "C-c")
;;   "mark-like-this"
;;   (")" mc/mark-next-like-this "mark next")
;;   ("(" mc/mark-previous-like-this "mark previous")
;;   ;; ("?" mc/mark-all-like-this "mark all"))
;;   )
(global-set-key (kbd "C-c )") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c (") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-0") 'mc/mark-next-lines)
(global-set-key (kbd "C-c C-9") 'mc/mark-previous-lines)


(defun sshcs ()
  (interactive)
  (find-file "/ssh:maxsobolmark@sc.stanford.edu:/")
  )
(defun initel ()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )

(setq visible-bell 1)

(global-set-key (kbd "M-p") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; Auctex config
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-to-list 'load-path "~/.emacs.d/packages")
(load "ediff-trees.el")

(setq ztree-diff-filter-list '("^\\." ".?\.pyc$" ".?pycache.?"))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

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

(require 'org)
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

(setenv "WORKON_HOME" "~/dev/emacs_pyenv")
(setenv "VIRTUAL_ENV" "~/dev/emacs_pyenv")

(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'python-mode-hook #'auto-fill-mode)
(setq-default fill-column 100)

(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-cdlatex)   ; with Emacs latex mode
(add-hook 'org-mode-hook 'turn-on-cdlatex)   ; with org mode


(setq org-roam-ui-sync-theme t
      org-roam-ui-follow t
      org-roam-ui-update-on-save t
      org-roam-ui-open-on-start t)
; Set relative line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers 'relative)
(setq display-line-numbers-type 'relative)
(global-linum-mode 0)

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
