(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(apheleia-mode-alist
   '((php-mode . phpcs) (json-mode . prettier-json) (json-ts-mode . prettier-json) (asm-mode . asmfmt)
	 (awk-mode . gawk) (bash-ts-mode . shfmt) (bazel-mode . buildifier)
	 (beancount-mode . bean-format) (c++-ts-mode . clang-format) (caddyfile-mode . caddyfmt)
	 (cc-mode . clang-format) (c-mode . clang-format) (c-ts-mode . clang-format)
	 (c++-mode . clang-format) (caml-mode . ocamlformat) (cmake-mode . cmake-format)
	 (cmake-ts-mode . cmake-format) (common-lisp-mode . lisp-indent)
	 (crystal-mode . crystal-tool-format) (css-mode . prettier-css) (css-ts-mode . prettier-css)
	 (dart-mode . dart-format) (dart-ts-mode . dart-format) (elixir-mode . mix-format)
	 (elixir-ts-mode . mix-format) (elm-mode . elm-format) (fish-mode . fish-indent)
	 (go-mode . gofmt) (go-mod-ts-mode . gofmt) (go-ts-mode . gofmt)
	 (graphql-mode . prettier-graphql) (haskell-mode . brittany) (html-mode . prettier-html)
	 (html-ts-mode . prettier-html) (java-mode . google-java-format)
	 (java-ts-mode . google-java-format) (js3-mode . prettier-javascript)
	 (js-mode . prettier-javascript) (js-ts-mode . prettier-javascript) (kotlin-mode . ktlint)
	 (latex-mode . latexindent) (LaTeX-mode . latexindent) (lua-mode . stylua)
	 (lisp-mode . lisp-indent) (nasm-mode . asmfmt) (nix-mode . nixfmt) (perl-mode . perltidy)
	 (purescript-mode . purs-tidy) (python-mode . black) (python-ts-mode . black)
	 (ruby-mode . prettier-ruby) (ruby-ts-mode . prettier-ruby) (rustic-mode . rustfmt)
	 (rust-mode . rustfmt) (rust-ts-mode . rustfmt) (scss-mode . prettier-scss)
	 (svelte-mode . prettier-svelte) (terraform-mode . terraform) (TeX-latex-mode . latexindent)
	 (TeX-mode . latexindent) (tsx-ts-mode . prettier-typescript) (tuareg-mode . ocamlformat)
	 (typescript-mode . prettier-typescript) (typescript-ts-mode . prettier-typescript)
	 (web-mode . prettier) (yaml-mode . prettier-yaml) (yaml-ts-mode . prettier-yaml)
	 (emacs-lisp-mode . lisp-indent)))
 '(apheleia-remote-algorithm 'local)
 '(avy-enter-times-out nil)
 '(avy-timeout-seconds 0.2)
 '(cdlatex-paired-parens "$[{(")
 '(copilot-node-executable "/opt/homebrew/bin/node")
 '(custom-safe-themes
   '("aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
	 "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf"
	 "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae"
	 "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53"
	 "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13"
	 "3d54650e34fa27561eb81fc3ceed504970cc553cfd37f46e8a80ec32254a3ec3"
	 "6e14157d0c8857e81035e6c7131dc17e4115b3911c82a1fd32e528aec8e89eab"
	 "f302eb9c73ead648aecdc1236952b1ceb02a3e7fcd064073fb391c840ef84bca"
	 "2c49d6ac8c0bf19648c9d2eabec9b246d46cb94d83713eaae4f26b49a8183fc4"
	 "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6"
	 "f94110b35f558e4c015b2c680f150bf8a19799d775f8352c957d9d1054b0a210"
	 "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a"
	 "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11"
	 "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd"
	 "0a41da554c41c9169bdaba5745468608706c9046231bbbc0d155af1a12f32271"
	 "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0"
	 "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" default))
 '(debug-on-error nil)
 '(debug-on-quit nil)
 '(debug-on-signal nil)
 '(diff-hl-disable-on-remote t)
 '(dired-listing-switches "-alh")
 '(elfeed-feeds
   '("https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml"))
 '(explicit-shell-file-name "/bin/zsh")
 '(global-display-line-numbers-mode t)
 '(highlight-tail-colors
   ((("#0d3630" "#0f393a" "green") . 0) (("#04363f" "#073c4a" "brightcyan") . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#073642" "#268bd2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#073642" "#859900"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#073642" "#56697A"))
 '(magit-diff-highlight-trailing nil)
 '(make-backup-files nil)
 '(ns-right-alternate-modifier 'control)
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background
				 "Transparent" :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-image-actual-width nil)
 '(package-selected-packages
   '(add-node-modules-path apheleia auctex-latexmk bash-completion cdlatex company-auctex company-jedi
						   consult-lsp copilot corfu counsel-tramp dap-mode dashboard diff-hl
						   diminish doom-themes ein el-get elpy embark-consult exec-path-from-shell
						   flycheck forge gcmh helm-dash jedi latex-preview-pane lsp-ui marginalia
						   multiple-cursors orderless org-roam pdf-tools prettier-js projectile
						   request-deferred restart-emacs slack smart-hungry-delete smartparens
						   tree-sitter-langs undo-tree unfill unison-sync-mode vertico vterm
						   yaml-mode ztree))
 '(package-vc-selected-packages
   '((unison-sync-mode :url "https://github.com/jsigman/unison-sync-mode.git" :branch "main")
	 (copilot :url "https://github.com/copilot-emacs/copilot.el" :branch "main")))
 '(pdf-latex-command "lualatex")
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(project-vc-merge-submodules nil)
 '(remote-file-name-inhibit-cache nil)
 '(remote-file-name-inhibit-locks t)
 '(scroll-conservatively 101)
 '(set-mark-command-repeat-pop t)
 '(shell-escape-mode "-shell-escape")
 '(shell-file-name "/bin/zsh")
 '(tab-width 4)
 '(tramp-auto-save-directory "/Users/joelsobolmark/.emacs.d/backup_files/")
 '(tramp-connection-timeout 25)
 '(tramp-remote-path
   '(tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin"
							   "/local/bin" "/local/freeware/bin" "/local/gnu/bin"
							   "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin"
							   "/opt/sbin" "/opt/local/bin" "/sailhome/maxsobolmark/defaultenv/bin"))
 '(tramp-verbose 2)
 '(undo-tree-auto-save-history nil)
 '(vc-handled-backends '(Git))
 '(vterm-kill-buffer-on-exit nil)
 '(warning-suppress-types '((pdf-view))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "#d4d4d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Menlo")))))
