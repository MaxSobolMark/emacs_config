(require 'consult)
(require 'vterm)

(defvar my-computers-config-file "~/.emacs.d/ssh-hosts.el"
  "Path to the configuration file containing computer details.")

(defun my-load-computers ()
  "Load computer configurations from `my-computers-config-file`."
  (when (file-exists-p my-computers-config-file)
    (with-temp-buffer
      (insert-file-contents my-computers-config-file)
      (let ((data (read (buffer-string))))
        data))))

;; (defun vterm-to-computer (&optional prefix)
;;   "Display a list of computers to connect to and handle selection.
;; If called with PREFIX, defaults to the first computer in the list."
;;   (interactive "P")
;;   (let* ((computers (my-load-computers))
;;          ;; Add a "local" option to the list
;;          (choices (cons '("Local" . nil)
;;                         (mapcar (lambda (entry)
;;                                   (let ((display-name (plist-get entry :display_name))
;;                                         (ssh-name (plist-get entry :ssh_name)))
;;                                     (cons display-name ssh-name)))
;;                                 computers)))
;;          (default-choice (caar choices)) ; First option (display name of "Local")
;;          (selected (if prefix
;;                        default-choice
;;                      (consult--read
;;                       choices
;;                       :prompt "Select a computer: "
;;                       :category 'computer
;;                       :sort nil)))
;;          (ssh-name (cdr (assoc selected choices))) ; Retrieve ssh-name from selection
;;          (buffer-name (format "*vterm-%s*" selected)))
;;     (if (get-buffer buffer-name)
;;         (switch-to-buffer buffer-name)
;;       (vterm buffer-name)
;;       (when ssh-name
;;         (vterm-send-string (format "ssh %s" ssh-name))
;;         (vterm-send-return)))))

;; Example of the configuration file (~/.computers-config.el):
;; '((:display_name "Server 1" :ssh_name "user@server1.com")
;;   (:display_name "Server 2" :ssh_name "user@server2.com"))

(defun ssh-to-computer (&optional prefix)
  "Open a remote directory on a selected computer using Tramp.
If called with PREFIX, defaults to the first computer in the list."
  (interactive "P")
  (let* ((computers (my-load-computers))
         ;; Add a "local" option to the list
         (choices (cons '("Local" . nil)
                        (mapcar (lambda (entry)
                                  (let ((display-name (plist-get entry :display_name))
                                        (ssh-name (plist-get entry :ssh_name)))
                                    (cons display-name ssh-name)))
                                computers)))
         (default-choice (caar choices)) ; First option (display name of "Local")
         (selected (if prefix
                       default-choice
                     (consult--read
                      choices
                      :prompt "Select a computer: "
                      :category 'computer
                      :sort nil)))
         (ssh-name (cdr (assoc selected choices))))
    (if (not ssh-name)
        (message "Local option selected. No remote directory opened.")
      (find-file (format "/ssh:%s:~/" ssh-name)))))

(defun my-computers-with-open-buffers (computers)
  "Return a list of computers with their buffer status.
Computers with open vterm buffers are marked with a special property."
  (mapcar (lambda (entry)
            (let* ((display-name (plist-get entry :display_name))
                   (ssh-name (plist-get entry :ssh_name))
                   (buffer-name (format "*vterm-%s*" display-name))
                   (has-buffer (get-buffer buffer-name)))
              (list :display_name display-name
                    :ssh_name ssh-name
                    :buffer-name buffer-name
                    :has_buffer has-buffer)))
          computers))

(defun vterm-to-computer (&optional prefix)
  "Display a list of computers to connect to and handle selection.
Computers with open vterm buffers are prioritized and highlighted.
If called with PREFIX, defaults to the first computer in the list."
  (interactive "P")
  (let* ((computers (my-computers-with-open-buffers (my-load-computers)))
         ;; Create the "Local" entry
         (local-entry (list :display_name "Local"
                            :ssh_name nil
                            :buffer-name "*vterm-Local*"
                            :has_buffer (get-buffer "*vterm-Local*")))
         ;; Include "Local" in the computers list
         (all-computers (cons local-entry computers))
         ;; Get the list of buffer names in the order of recent usage
         (recent-buffers (mapcar #'buffer-name (buffer-list)))
         ;; Sort open connections by recent usage (lower index = more recent)
         (with-buffers (sort (seq-filter (lambda (entry) (plist-get entry :has_buffer)) all-computers)
                             (lambda (a b)
                               (< (or (seq-position recent-buffers (plist-get a :buffer-name)) most-positive-fixnum)
                                  (or (seq-position recent-buffers (plist-get b :buffer-name)) most-positive-fixnum)))))
         ;; Computers without open buffers
         (without-buffers (seq-filter (lambda (entry) (not (plist-get entry :has_buffer))) all-computers))
         ;; Final list of choices
         (choices (append with-buffers without-buffers))
         ;; Create formatted choices with colors applied to open buffers
         (formatted-choices (mapcar (lambda (entry)
                                      (let* ((display-name (plist-get entry :display_name))
                                             (ssh-name (plist-get entry :ssh_name))
                                             (has-buffer (plist-get entry :has_buffer))
                                             (colored-display (if has-buffer
                                                                  (propertize display-name 'face '(:foreground "green"))
                                                                display-name)))
                                        (cons colored-display ssh-name)))
                                    choices))
         (default-choice (caar formatted-choices)) ; Default to the first option in the reordered list
         (selected (if prefix
                       default-choice
                     (consult--read
                      formatted-choices
                      :prompt "Select a computer: "
                      :category 'computer
                      :sort nil)))
         ;; Retrieve ssh-name by matching against the display name
         (ssh-name (cdr (assoc selected formatted-choices)))
         (buffer-name (format "*vterm-%s*" (substring-no-properties selected))))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      ;; Ensure vterm is called locally
      (let ((default-directory "~/"))  ;; Set the default directory to home
        (vterm buffer-name))
      (when ssh-name
        (vterm-send-string (format "ssh %s" ssh-name))
        (vterm-send-return)))))

(provide 'ssh-computer-manager)
