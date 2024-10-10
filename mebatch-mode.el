;; (defvar mebatch-mode-font-lock-keywords
;;   (let* ((param-regex "\\*\\*\\*\\([^*]+\\)\\*\\*\\*")
;;          (param-faces '(font-lock-variable-name-face
;;                         font-lock-function-name-face
;;                         font-lock-constant-face
;;                         font-lock-keyword-face
;;                         font-lock-type-face
;;                         font-lock-builtin-face))
;;          (param-list '())
;;          (face-index 0))
;;     (list
;;      ;; Highlight mebatch command
;;      '("\\<mebatch\\>" . font-lock-preprocessor-face)
;;      ;; Highlight parameters
;;      `(,param-regex
;;        (1 (let* ((param (match-string-no-properties 1))
;;                  (existing-face (cdr (assoc param param-list))))
;;             (if existing-face
;;                 existing-face
;;               (let ((new-face (nth face-index param-faces)))
;;                 (setq param-list (cons (cons param new-face) param-list))
;;                 (setq face-index (mod (1+ face-index) (length param-faces)))
;;                 new-face))))))))

;; (define-derived-mode mebatch-mode fundamental-mode "MeBatch"
;;   "Major mode for editing MeBatch command lines."
;;   (setq font-lock-defaults '(mebatch-mode-font-lock-keywords)))

;; (add-to-list 'auto-mode-alist '("\\.mebatch\\'" . mebatch-mode))

;; (provide 'mebatch-mode)


(defvar mebatch-param-list '())
(defvar mebatch-face-index 0)

(defvar mebatch-mode-font-lock-keywords
  (let ((param-regex "\\*\\*\\*\\([^*]+\\)\\*\\*\\*")
        (param-faces '(font-lock-variable-name-face
                       font-lock-function-name-face
                       font-lock-constant-face
                       font-lock-keyword-face
                       font-lock-type-face
                       font-lock-builtin-face)))
    (list
     ;; Highlight mebatch command
     '("\\<mebatch\\>" . font-lock-preprocessor-face)
     ;; Highlight parameters
     `(,param-regex
       (1 (let* ((param (match-string-no-properties 1))
                 (existing-face (cdr (assoc param mebatch-param-list))))
            (if existing-face
                existing-face
              (let ((new-face (nth mebatch-face-index param-faces)))
                (setq mebatch-param-list (cons (cons param new-face) mebatch-param-list))
                (setq mebatch-face-index (mod (1+ mebatch-face-index) (length param-faces)))
                new-face))))))))

(define-derived-mode mebatch-mode fundamental-mode "MeBatch"
  "Major mode for editing MeBatch command lines."
  (setq font-lock-defaults '(mebatch-mode-font-lock-keywords))
  (setq-local mebatch-param-list '())
  (setq-local mebatch-face-index 0))

(add-to-list 'auto-mode-alist '("\\.mebatch\\'" . mebatch-mode))


(defun mebatch-instantiate ()
  "Transform a mebatch command in the selected region into a specific command,
picking the first option for each variable."
  (interactive)
  (when (use-region-p)
    (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
           (command-parts (split-string region-text "\""))
           (mebatch-part (car command-parts))
           (command-part (cadr command-parts)))
      (when (and mebatch-part command-part)
        (let ((instantiated-command command-part))
          ;; Replace variables in the command part
          (while (string-match "\\*\\*\\*\\([^*]+\\)\\*\\*\\*" instantiated-command)
            (let* ((var-name (match-string 1 instantiated-command))
                   (var-pattern (concat "\\*\\*\\*" var-name ":\\([^,*]+\\)"))
                   (var-value (if (string-match var-pattern mebatch-part)
                                  (match-string 1 mebatch-part)
                                ""))
                   (replacement-pattern (concat "\\*\\*\\*" var-name "\\*\\*\\*")))
              (setq instantiated-command 
                    (replace-regexp-in-string replacement-pattern var-value instantiated-command t t))))
          ;; Replace the region with the instantiated command
          (delete-region (region-beginning) (region-end))
          (insert instantiated-command))))))

(provide 'mebatch-mode)
