;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)
(add-to-list 'default-frame-alist '(undecorated . t))
(setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/14:/opt/homebrew/opt/libgccjit/lib/gcc/14:/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin23/14")
;; (add-to-list 'default-frame-alist '(undecorated-round . t))
