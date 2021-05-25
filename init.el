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
 '(company-tabnine-always-trigger t)
 '(custom-safe-themes
   '("2c49d6ac8c0bf19648c9d2eabec9b246d46cb94d83713eaae4f26b49a8183fc4" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "f94110b35f558e4c015b2c680f150bf8a19799d775f8352c957d9d1054b0a210" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "0a41da554c41c9169bdaba5745468608706c9046231bbbc0d155af1a12f32271" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" default))
 '(package-selected-packages
   '(ztree auctex ace-window multiple-cursors magit avy ivy projectile yasnippet-snippets company-tabnine pyenv-mode elpy smartparens company-jedi doom-themes spacemacs-theme company)))
  
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

  ;; Load theme
  (load-theme 'doom-dark+)

  (elpy-enable)
(require 'pyvenv)

(require 'company-tabnine)
(add-to-list 'company-backends #'company-tabnine)
  
(pyvenv-workon ".")

(desktop-save-mode 1)

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
(global-set-key (kbd "C-c i") 'move-line-up)
(global-set-key (kbd "C-c o") 'move-line-down)

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
(global-set-key (kbd "C-c )") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c (") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c ?") 'mc/mark-all-like-this)


(defun sshcs ()
  (interactive)
  (find-file "/ssh:maxsobolmark@sc.stanford.edu:~/welcome.org")
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
