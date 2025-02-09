(global-set-key (kbd "C-M-k") 'kill-whole-line)
(global-set-key (kbd "C-c d") 'duplicate-line)

(global-set-key (kbd "C-c M-c") 'copy-file)
(global-set-key (kbd "C-c M-d") 'copy-directory)
(global-set-key (kbd "C-c `") 'ediff-buffers)

(global-set-key (kbd "C-c /") 'comment-region)
(global-set-key (kbd "C-c ?") 'uncomment-region)

(global-set-key (kbd "S-<tab>") 'other-window)

(global-set-key (kbd "C-o") 'my-insert-before-line)
(global-set-key (kbd "M-w") 'copy-region-or-line)


(global-set-key (kbd "M-`") 'project-find-file)
(global-set-key (kbd "C-S-k") 'kill-line-backwards)
(global-set-key (kbd "/") 'fast-comment)
(global-set-key (kbd "?") 'fast-uncomment)

(global-set-key (kbd "w") 'fast-copy)
(global-set-key (kbd "k") 'fast-kill)

(global-set-key (kbd "C-.") (lambda () (interactive) (push-mark (point) nil nil)))
;; (global-set-key (kbd "TAB") 'max-python-tab)

(global-set-key (kbd "<f9>") 'make-frame)

(global-set-key (kbd "C-c <backspace>") 'sp-unwrap-sexp)

(add-hook 'python-mode-hook
          (lambda () (progn
                       (local-set-key (kbd "C-M-n") #'python-nav-end-of-block)
                       (local-set-key (kbd "C-M-p") #'python-nav-beginning-of-block)
                       (local-set-key (kbd "C-t") #'python-nav-forward-defun)
                       (local-set-key (kbd "M-t") #'python-nav-backward-defun)
                       )))

(global-unset-key (kbd "ESC ESC ESC"))


(global-set-key (kbd "C-|") (lambda () (interactive) (python-selection)))

(global-unset-key (kbd "<f1>"))
(global-set-key (kbd "<f1>") 'delete-other-windows)
(global-set-key (kbd "<f3>") (lambda () (interactive)
                               (progn
                                 (split-window-right)
                                 (other-window 1))))

(global-set-key (kbd "<f2>") (lambda () (interactive)
                               (progn
                                 (split-window-below)
                                 (other-window 1)
                                 )))
(global-set-key (kbd "<f10>") 'delete-window)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(define-key copilot-completion-map (kbd "C-g") 'copilot-clear-overlay)
(defhydra hydra-copilot-accept (global-map "M-\\")
  "copilot-accept"
  ("\\" copilot-accept-completion "accept")
  ("c" copilot-complete "complete")
  ("f" copilot-accept-completion-by-word "accept word completion")
  ("n" copilot-accept-completion-by-line "accept line completion"))

(global-set-key (kbd "C-M-SPC") 'mark-sexp-with-prefix)

(global-set-key (kbd "M-C") 'copy-buffer-path)
(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-n") 'scroll-up-line)
(global-set-key (kbd "C-^") 'enlarge-window)
(global-set-key (kbd "C-M-^") 'shrink-window)
(global-set-key (kbd "C-x f") 'find-file)


;; Vterm
(define-key vterm-mode-map (kbd "C-g") 'vterm-copy-mode)
(define-key vterm-copy-mode-map (kbd "C-g") 'vterm-copy-mode-done)
(define-key vterm-mode-map (kbd "C-q") 'vterm-send-next-key)
;; (global-set-key (kbd "<f7>") 'vterm-shortcut)
(global-set-key (kbd "<f7>") 'vterm-to-computer)
;; Remove <f1> through <f12> and escape from vterm-mode-map
(define-key vterm-mode-map (kbd "<f1>") nil)
(define-key vterm-mode-map (kbd "<f2>") nil)
(define-key vterm-mode-map (kbd "<f3>") nil)
(define-key vterm-mode-map (kbd "<f4>") nil)
(define-key vterm-mode-map (kbd "<f5>") nil)
(define-key vterm-mode-map (kbd "<f6>") nil)
(define-key vterm-mode-map (kbd "<f7>") nil)
(define-key vterm-mode-map (kbd "<f8>") nil)
(define-key vterm-mode-map (kbd "<f9>") nil)
(define-key vterm-mode-map (kbd "<f10>") nil)
(define-key vterm-mode-map (kbd "<f11>") nil)
(define-key vterm-mode-map (kbd "<f12>") nil)
(define-key vterm-mode-map (kbd "<escape>") nil)
(define-key vterm-mode-map (kbd "M-:") nil)
;; Make C-v and M-v pass page up and page down to vterm
(define-key vterm-mode-map (kbd "C-v") 'vterm-send-key (kbd "<next>"))
(define-key vterm-mode-map (kbd "M-v") 'vterm-send-key (kbd "<prior>"))
;; Bind M-<up> to vterm-scroll-fast without prefix
(define-key vterm-mode-map (kbd "M-<up>") 'vterm-scroll-fast)
;; Bind M-<down> to vterm-scroll-fast with prefix
(define-key vterm-mode-map (kbd "M-<down>") (lambda () (interactive) (vterm-scroll-fast t)))

;; unset f on dired-mode-map
;; (define-key dired-mode-map (kbd "f") nil)

(global-set-key (kbd "C-M-s") 'swiper)
(global-set-key (kbd "C-M-r") 'swiper-backward)
(global-set-key (kbd "C-r") 'swiper-backward)
(global-set-key (kbd "C-M-S-SPC") 'select-whole-sexp)

