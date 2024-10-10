(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

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

(defun sshsirius ()
  (interactive)
  ;; Disable projectile-mode for this buffer
  (projectile-mode -1)
  (find-file "/ssh:max@172.28.76.245:~/")
  )
(defun sshcs ()
  (interactive)
  ;; Disable projectile-mode for this buffer
  (projectile-mode -1)
  (find-file "/ssh:maxsobolmark@ai.stanford.edu|ssh:maxsobolmark@sc.stanford.edu:/iris/u/maxsobolmark")
  )
(defun sshcsdt ()
  (interactive)
  ;; Disable projectile-mode for this buffer
  (projectile-mode -1)
  (find-file "/ssh:maxsobolmark@ai.stanford.edu|ssh:maxsobolmark@scdt.stanford.edu:/iris/u/maxsobolmark")
  )
(defun sshcs-reconnect ()
  (interactive)
  (tramp-cleanup-all-connections)
  ;; Disable projectile-mode for this buffer
  (projectile-mode -1)
  (find-file "/ssh:maxsobolmark@ai.stanford.edu|ssh:maxsobolmark@sc.stanford.edu:/iris/u/maxsobolmark")
  )
(defun sshws ()
  (interactive)
  (find-file "/ssh:maxsobolmark@ai.stanford.edu|ssh:maxsobolmark@iris-ws-10.stanford.edu:/"))
(defun sshws18 ()
  (interactive)
  (find-file "/ssh:maxsobolmark@ai.stanford.edu|ssh:maxsobolmark@iris-ws-18.stanford.edu:/"))
(defun sshmax ()
  (interactive)
  ;; Disable projectile-mode for this buffer
  (projectile-mode -1)
  (find-file "/ssh:max@172.28.85.119:~/"))
(defun initel ()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )
(defun scratch ()
  (interactive)
  (find-file "~/Desktop/scratch.txt")
  )
(defun mebatch-history (&optional prefix)
  (interactive "P")
  (if prefix
      (find-file "/ssh:max@172.28.85.119:~/dev/mebatch/mebatch_history.txt")
    (find-file "/ssh:maxsobolmark@ai.stanford.edu|ssh:maxsobolmark@sc.stanford.edu:/iris/u/maxsobolmark/mebatch/mebatch_history.txt")
    )
  )
(defun bashrc ()
  (interactive)
  (find-file "/ssh:maxsobolmark@ai.stanford.edu|ssh:maxsobolmark@sc.stanford.edu:~/.bashrc")
  )
(defun jingyun ()
  (interactive)
  (find-file "/ssh:maxsobolmark@ai.stanford.edu|ssh:maxsobolmark@sc.stanford.edu:/iris/u/jingyuny/projects/p_bridge/"))

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

(defun copy-region-or-line ()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (kill-ring-save nil nil t)  ; There's something selected, so just copy
      (progn
        (back-to-indentation) (set-mark (point)) (end-of-visual-line) (kill-ring-save nil nil t))
      )
    ))


(defun kill-line-backwards ()
  (interactive)
  (set-mark (point))
  (back-to-indentation)
  (kill-region nil nil t))

(defun fast-comment ()
  (interactive)
  (if mark-active
      (comment-region (region-beginning) (region-end))
    (insert "/")
    ))
(defun fast-uncomment ()
  (interactive)
  (if mark-active
      (uncomment-region (region-beginning) (region-end))
    (insert "?")
    ))
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

(defun python-selection()
  (interactive)
  (if (use-region-p)
      (progn
        (kill-region (region-beginning) (region-end))
        (setq current-prefix-arg '(4)) ; C-u
        (shell-command (format "python -c 'print(%s)'" (car kill-ring)) t)
        (end-of-visual-line)
        (delete-char 1)
        )
    (message "Nothing selected"))
  )
(defun mark-sexp-with-prefix (arg)
  "Mark the sexp at point. With prefix ARG, mark previous sexp."
  (interactive "P")
  (unless (region-active-p)
    (set-mark (point)))
  (if arg
      (backward-sexp)
    (forward-sexp))
  )

(defun copy-buffer-path (&optional prefix)
  "Copy the path of the current buffer to the kill ring.
With prefix arg, include the remote path for tramp buffers."
  (interactive "P")
  (let ((filename (buffer-file-name)))
    (if filename
        (let* ((path (if (and (not prefix) (file-remote-p filename))
                         (tramp-file-name-localname (tramp-dissect-file-name filename))
                       (file-name-directory filename))))
          (kill-new path)
          (message "Copied buffer path: %s" path))
      ;; For dired buffers
      (let ((path (if prefix
                      default-directory
                    (tramp-file-name-localname (tramp-dissect-file-name default-directory)))))
        (kill-new path)
        (message "Copied buffer path: %s" path)))))

(defun kill-all-buffers ()
  "Kill all buffers except shell buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (seq-filter (lambda (buffer)
                            (and (or (buffer-file-name buffer)
                                     (with-current-buffer buffer
                                       (eq major-mode 'dired-mode)))
                                 (not (get-buffer-process buffer))))
                          (buffer-list)))))

(defun my/vterm-mode-customization ()
  "Customize mode line for vterm-mode."
  (setq mode-name "VTerm 💻"))

(defun my/vterm-copy-mode-customization ()
  "Customize mode line for vterm-copy-mode."
  (setq mode-name "VTerm 🖊️"))

(defun local-vterm ()
  "If buffer *local-vterm* exists, switch to it. Otherwise, create a new vterm buffer from home directory."
  (interactive)
  (if (get-buffer "*local-vterm*")
      (switch-to-buffer "*local-vterm*")
    (progn
      (find-file "~")
      (vterm "~/")
      (rename-buffer "*local-vterm*")
      )
    )
  )

(defun sshcs-vterm ()
  "If buffer *ssh-vterm* exists, switch to it. Otherwise, create a new vterm buffer from sc server."
  (interactive)
  (if (get-buffer "*sshcs-vterm*")
      (switch-to-buffer "*sshcs-vterm*")
    (progn
      (sshcs)
      (vterm "/ssh:maxsobolmark@ai.stanford.edu|ssh:maxsobolmark@sc.stanford.edu:/iris/u/maxsobolmark/")
      (rename-buffer "*sshcs-vterm*")
      )
    )
  )

(defun ws18-vterm ()
  "If buffer *ws18-vterm* exists, switch to it. Otherwise, create a new vterm buffer from ws18 server."
  (interactive)
  (if (get-buffer "*ws18-vterm*")
      (switch-to-buffer "*ws18-vterm*")
    (progn
      (projectile-mode -1)
      (find-file "/ssh:maxsobolmark@ai.stanford.edu|ssh:maxsobolmark@iris-ws-18.stanford.edu:/iris/u/maxsobolmark")
      (vterm "/ssh:maxsobolmark@ai.stanford.edu|ssh:maxsobolmark@iris-ws-18.stanford.edu:/iris/u/maxsobolmark/")
      (rename-buffer "*ws18-vterm*")
      )
    )
  )

(defun vterm-shortcut (&optional prefix)
  "If no prefix, open local vterm. If prefix, open ssh vterm."
  (interactive "P")
  (if prefix
      (sshcs-vterm)
    (local-vterm))
  )

(defun vterm-scroll-fast (&optional prefix)
  "Scroll up (down) fast in vterm by sending the up (down) key 50 times."
  (interactive)
  (when (eq major-mode 'vterm-mode)
    (dotimes (_ 50) ; Repeat the action 50 times
      (vterm-send-key (if prefix "<down>" "<up>"))))
  )
