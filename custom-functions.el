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
  (find-file "/ssh:max@172.28.76.245:~/")
  )
(defun sshcs ()
  (interactive)
  (find-file "/ssh:sc.stanford.edu:/iris/u/maxsobolmark")
  )
(defun sshcsdt ()
  (interactive)
  (find-file "/ssh:scdt.stanford.edu:/iris/u/maxsobolmark")
  )
(defun sshcs-reconnect ()
  (interactive)
  (tramp-cleanup-all-connections)
  (find-file "/ssh:sc.stanford.edu:/iris/u/maxsobolmark")
  )
(defun sshws ()
  (interactive)
  (find-file "/ssh:iris-ws-10.stanford.edu:/iris/u/maxsobolmark"))
(defun sshws18 ()
  (interactive)
  (find-file "/ssh:iris-ws-18.stanford.edu:/iris/u/maxsobolmark"))
(defun sshmax ()
  (interactive)
  (find-file "/ssh:max@172.28.85.119:~/"))
(defun sshbabel ()
  (interactive)
  (find-file "/ssh:babel:~/")
  )
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
      (find-file "/ssh:sc.stanford.edu:/iris/u/maxsobolmark/mebatch/mebatch_history.txt")
    (find-file "/ssh:babel:~/mebatch/mebatch_history.txt")
    )
  )
(defun bashrc ()
  (interactive)
  (find-file "/ssh:sc.stanford.edu:~/.bashrc")
  )

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
      (vterm "/ssh:sc.stanford.edu:/iris/u/maxsobolmark/")
      (rename-buffer "*sshcs-vterm*")
      )
    )
  )

(defun sshbabel-vterm ()
  "If buffer *babel-vterm* exists, switch to it. Otherwise, create a new vterm buffer from babel server."
  (interactive)
  (if (get-buffer "*sshbabel-vterm*")
      (switch-to-buffer "*sshbabel-vterm*")
    (progn
      (sshbabel)
      (vterm "/ssh:babel:~/")
      (rename-buffer "*sshbabel-vterm*")
	  ;; Execute source ~/bashrc_max
	  (vterm-send-string "source ~/bashrc_max")
	  ;; Press enter
	  (vterm-send-return)
      )
    )
  )

(defun ws18-vterm ()
  "If buffer *ws18-vterm* exists, switch to it. Otherwise, create a new vterm buffer from ws18 server."
  (interactive)
  (if (get-buffer "*ws18-vterm*")
      (switch-to-buffer "*ws18-vterm*")
    (progn
      ;; (find-file "/ssh:iris-ws-18.stanford.edu:/iris/u/maxsobolmark")
      ;; (vterm "/ssh:iris-ws-18.stanford.edu:/iris/u/maxsobolmark/")
	  (find-file "~")
	  (vterm "~/")
      (rename-buffer "*ws18-vterm*")
	  (vterm-send-string "ssh iris-ws-18.stanford.edu")
	  (vterm-send-return)
      )
    )
  )

(defun vterm-shortcut (&optional prefix)
  "If no prefix, open local vterm. If prefix, open ssh vterm."
  (interactive "P")
  (if prefix
      (sshbabel-vterm)
    (local-vterm))
  )

(defun vterm-scroll-fast (&optional prefix)
  "Scroll up (down) fast in vterm by sending the up (down) key 50 times."
  (interactive)
  (when (eq major-mode 'vterm-mode)
    (dotimes (_ 50) ; Repeat the action 50 times
      (vterm-send-key (if prefix "<down>" "<up>"))))
  )



;; ;; Function to check if a file is remote
;; (defun file-remote-p* (filename)
;;   "Enhanced remote file check that handles more edge cases."
;;   (or (file-remote-p filename)
;;       (string-match-p "^/\\(?:ssh\\|scp\\|sftp\\|su\\|sudo\\|ftp\\):" filename)))

;; ;; Enhanced function to open files intelligently
;; (defun smart-find-file (filename &optional wildcards)
;;   "Open FILENAME intelligently - async for remote files, directly for local ones.
;; Uses standard find-file for directories or already-opened files.
;; Optional WILDCARDS argument is passed to `find-file' for local files."
;;   (interactive
;;    (find-file-read-args "Find file: "
;;                         (confirm-nonexistent-file-or-buffer)))
;;   (cond
;;    ;; If it's a directory or buffer already exists, use standard find-file
;;    ((or (file-directory-p filename)
;;         (get-file-buffer filename))
;;     (find-file filename wildcards))
;;    ;; If remote file, handle asynchronously
;;    ((file-remote-p* filename)
;;     (smart-find-file-remote filename))
;;    ;; Otherwise, regular local file
;;    (t
;;     (find-file filename wildcards))))

;; (defun smart-find-file-remote (filename)
;;   "Open remote FILENAME asynchronously using TRAMP.
;; Shows loading state in the target buffer while fetching content."
;;   (let* ((buffer-name (generate-new-buffer-name (file-name-nondirectory filename)))
;;          (target-frame (selected-frame))
;;          (target-window (selected-window))
;;          (loading-buffer (get-buffer-create buffer-name)))
;;     ;; Set up the loading buffer
;;     (with-current-buffer loading-buffer
;;       (insert (format "Loading %s...\n\nPlease wait while the remote file is being fetched..." 
;;                       filename))
;;       (read-only-mode 1))

;;     ;; Display the loading buffer in the target window
;;     (set-window-buffer target-window loading-buffer)

;;     ;; Start async process
;;     (async-start
;;      `(lambda ()
;;         (with-temp-buffer
;;           (insert-file-contents ,filename)
;;           (buffer-string)))
;;      `(lambda (content)
;;         (when (frame-live-p ',target-frame)
;;           (with-selected-frame ',target-frame
;;             (with-current-buffer ,buffer-name
;;               (read-only-mode -1)
;;               (erase-buffer)
;;               (insert content)
;;               (set-visited-file-name ,filename)
;;               (set-buffer-modified-p nil)
;;               (normal-mode)
;;               (read-only-mode -1)
;;               (goto-char (point-min))
;;               (when (window-live-p ',target-window)
;;                 (set-window-buffer ',target-window (current-buffer))))))))))

;; ;; Replace the default find-file with our smart version
;; (global-set-key [remap find-file] 'smart-find-file)

;; ;; Also replace find-file-other-window if desired
;; (defun smart-find-file-other-window (filename &optional wildcards)
;;   "Like `smart-find-file', but select file's buffer in another window."
;;   (interactive
;;    (find-file-read-args "Find file in other window: "
;;                         (confirm-nonexistent-file-or-buffer)))
;;   (let ((other-window-buffer (selected-window)))
;;     (other-window 1)
;;     (smart-find-file filename wildcards)))

;; (global-set-key [remap find-file-other-window] 'smart-find-file-other-window)

;; ;; Optionally, also handle find-file-other-frame
;; (defun smart-find-file-other-frame (filename &optional wildcards)
;;   "Like `smart-find-file', but select file's buffer in another frame."
;;   (interactive
;;    (find-file-read-args "Find file in other frame: "
;;                         (confirm-nonexistent-file-or-buffer)))
;;   (select-frame (make-frame))
;;   (smart-find-file filename wildcards))

;; (global-set-key [remap find-file-other-frame] 'smart-find-file-other-frame)

;; ;; Advice dired-find-file to use smart-find-file
;; (defun dired-smart-find-file (&rest args)
;;   "Advice for dired-find-file to use smart-find-file instead of find-file."
;;   (let ((file (dired-get-file-for-visit)))
;;     (smart-find-file file)))

;; ;; Apply the advice
;; (advice-add 'dired-find-file :override #'dired-smart-find-file)

;; ;; For dired-find-file-other-window
;; (defun dired-smart-find-file-other-window (&rest args)
;;   "Advice for dired-find-file-other-window to use smart-find-file."
;;   (let ((file (dired-get-file-for-visit)))
;;     (other-window 1)
;;     (smart-find-file file)))

;; (advice-add 'dired-find-file-other-window :override #'dired-smart-find-file-other-window)


(defun sshtpu ()
  "SSH into a selected Google Cloud TPU instance."
  (interactive)
  (let* ((v4-tpus (mapcar (lambda (n) (format "v4-tpu-%d-z" n)) (number-sequence 0 7)))
         (v5e-tpus (mapcar (lambda (n) (format "v5e-tpu-16-%d" n)) (number-sequence 0 7)))
         (tpu-options (append v4-tpus v5e-tpus))
         (selected-tpu (consult--read
                        tpu-options
                        :prompt "Select TPU: "
                        :category 'tpu))
         (ssh-method (if (string-prefix-p "v5e" selected-tpu) "gcsshv5" "gcssh"))
         (file-path (concat "/" ssh-method ":" selected-tpu ":/home/maxsobolmark/")))
    (find-file file-path)))

(defun tpu-vterm ()
  "Open a vterm in a selected Google Cloud TPU instance."
  (interactive)
  (let* ((v4-tpus (mapcar (lambda (n) (format "v4-tpu-%d-z" n)) (number-sequence 0 7)))
         (v5e-tpus (mapcar (lambda (n) (format "v5e-tpu-16-%d" n)) (number-sequence 0 7)))
         (tpu-options (append v4-tpus v5e-tpus))
         (selected-tpu (consult--read
                        tpu-options
                        :prompt "Select TPU: "
                        :category 'tpu))
         (ssh-method (if (string-prefix-p "v5e" selected-tpu) "gcsshv5" "gcssh"))
         (file-path (concat "/" ssh-method ":" selected-tpu ":/home/maxsobolmark/"))
         (buffer-name (concat "*vterm-" selected-tpu "*")))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (find-file "~")
      (vterm "~/")
      (rename-buffer buffer-name)
      ;; Extract TPU index from selected-tpu
      (let* ((cleaned-tpu (replace-regexp-in-string "-z$" "" selected-tpu))
             (tpu-index (string-to-number (car (last (split-string cleaned-tpu "-"))))))
        (message "Connecting to TPU %d" tpu-index)
        (vterm-send-string (if (string-prefix-p "v4" cleaned-tpu)
                               (format "tpuconnectv4 %d" tpu-index)
                             (format "tpuconnectv5 %d" tpu-index)))
        (vterm-send-return)))))

(defun lsp-next-reference (&optional prefix)
  "Go to the next (previous) reference in the current buffer using lsp-ui-find-next-reference."
  (interactive "P")
  (if prefix
	  (lsp-ui-find-prev-reference)
	(lsp-ui-find-next-reference))
  )

(defun select-whole-sexp ()
  "Select the entire S-expression at or before point."
  (interactive)
  (beginning-of-thing 'sexp)
  (set-mark (point))
  (end-of-thing 'sexp)
  )

