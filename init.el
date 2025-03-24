
;;; Code:
(set-face-attribute 'default nil
	:font (if (string-equal system-type 'windows-nt)
							  "Iosevka NF"
							"Iosevka Nerd Font")
	:height (if (string-equal (system-name) "aldrinslaptop")
								120
							 160))

;; PACKAGES

(require 'package)
(add-to-list 'package-archives
				 '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defvar package-list)
(setq package-list
  '(
	ace-window
	atom-one-dark-theme
	clang-format
	cmake-mode
	company
	doom-modeline
	emms
	exec-path-from-shell
	flycheck
	fountain-mode
	go-mode
	ido-completing-read+
	js2-mode
	kotlin-mode
	lsp-ui
	lsp-ivy
	lsp-treemacs
	lsp-mode
	lsp-tailwindcss
	magit
	nerd-icons
	php-mode
	prettier
	projectile
	shell-pop
	toml-mode
	treemacs
	treemacs-magit
	treemacs-nerd-icons
	tree-sitter
	tree-sitter-hl
	tree-sitter-langs
	tree-sitter-query
	wakatime-mode
	which-key
	yasnippet
  )
)

(dolist (package package-list)
  (when (and (not (package-installed-p package)) (assoc package package-archive-contents))
    (package-install package)))

;; LOADS

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/emms/lisp")


;; CONFIGURATIONS

(setq explicit-shell-file-name "/bin/zsh")
(setq explicit-zsh-args '("--interactive" "--login"))
(setq shell-pop-term-shell "/bin/zsh")
(setq comint-process-echoes 0)

(require 'whitespace)
(setq-default whitespace-style '(face tabs tab-mark))
(setq-default whitespace-display-mappings '((tab-mark 9 [32 8674 9] [92 9])))
(custom-set-faces '(whitespace-tab ((t (:foreground "#504D5A" :weight bold)))))
(global-whitespace-mode 1)


;; gray25 & light gray

(set-face-attribute 'mode-line-active nil
			:background "slate blue"
			:foreground "white"
			:box nil)

(setq-default tab-width (if (string-equal (system-name) "aldrinslaptop")
								3
							 2))
(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq line-number-mode t)
(setq column-number-mode t)
(global-display-line-numbers-mode)
(setq show-paren-mode t)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-context-when-offscreen t)
(electric-pair-mode)
(setq scroll-bar-mode nil)

(require 'ido-completing-read+)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-height 43)

(require 'lsp-mode)
(global-set-key [f2] 'lsp-rename)

(require 'projectile)
(projectile-mode)
(setq projectile-indexing-method 'alien)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(defun cpp-format-function ()
  "Format cc, cpp, cxx, hpp, c and h files."
  (when (buffer-file-name)
	 (let ((file-ext (file-name-extension buffer-file-name)))
		(if (or (string-equal file-ext "cc")
				  (string-equal file-ext "cpp")
				  (string-equal file-ext "cxx")
				  (string-equal file-ext "hpp")
				  (string-equal file-ext "c")
				  (string-equal file-ext "h"))
			 (clang-format-buffer)))))

(require 'clang-format)
(setq clang-format-style "LLVM")
(add-hook 'c++-mode-hook (add-hook 'before-save-hook 'cpp-format-function))

(exec-path-from-shell-initialize)

(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)

(require 'treemacs)
(setq aw-ignored-buffers (delq 'treemacs-mode aw-ignored-buffers))
(treemacs)
(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  (define-key treemacs-mode-map (kbd "C-c C-s") #'treemacs-switch-workspace))

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)
(setq git-commit-cd-to-toplevel t)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(global-set-key (kbd "C-h C-r") 'split-window-right)

(require 'magit)
(global-set-key (kbd "M-g") 'magit)

(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tree-sitter-query)
(require 'fountain-mode)
(require 'cmake-mode)

(require 'shell-pop)
(global-set-key (kbd "C-<return>") 'shell-pop)

;; Mind Palace
(defvar mind-palace-dir)
(setq mind-palace-dir "/mnt/main/docs/AldrinsMindPalace")
(defun insert-time-comment ()
  "Insert the current time as a comment to a markdown document."
  (interactive)
  (insert (concat "\n[comment]: # " (format-time-string "%Y/%m/%d %H:%M:%S") "\n")))
(defun mind-palace-fn ()
  "Function to run if files in the mind palace are being edited."
   (when (string-equal (file-name-extension buffer-file-name) "md")
 	  (when (file-in-directory-p buffer-file-name mind-palace-dir)
 		 (buffer-face-set '(:family "Inter" :height 130))
 		 (local-set-key (kbd "C-c C-t") 'insert-time-comment))))
(add-hook 'find-file-hook 'mind-palace-fn)

(require 'wakatime-mode)
(global-wakatime-mode)

(require 'flycheck)
(global-flycheck-mode)
(global-set-key (kbd "M-p") 'flycheck-previous-error)
(global-set-key (kbd "M-n") 'flycheck-next-error)

(require 'emms-setup)
(require 'emms-player-vlc)
(emms-all)
(emms-default-players)
(setq emms-player-vlc-command-name "cvlc")
(global-set-key [f7] 'emms-start)
(global-set-key [f5] 'emms-stop)
(global-set-key [f6] 'emms-previous)
(global-set-key [f8] 'emms-next)
(if (file-directory-p "/mnt/main/songs/lofi")
    (emms-add-directory "/mnt/main/songs/lofi"))
(emms-mode-line-disable)
(setq emms-playing-time-display-mode t)

(global-set-key (kbd "C-x t") 'toggle-truncate-lines)

(run-at-time (current-time) 300 'recentf-save-list)

(set-terminal-coding-system 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Treesitter Modes

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.py\\'" . python-ts-mode)
         ("\\.cmake\\'" . cmake-ts-mode)
         ("\\.go\\'" . go-ts-mode)
         ("\\.js\\'" . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" . json-ts-mode)
         ("\\.yaml\\'" . yaml-ts-mode)
         ("\\.css\\'" . css-ts-mode)
         ("\\.yml\\'" . yaml-ts-mode)
         ("\\.php\\'" . php-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.mm\\'" . objc-mode)
         ("\\.mdx\\'" . markdown-mode))
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (objc "https://github.com/tree-sitter-grammars/tree-sitter-objc")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (php "https://github.com/tree-sitter/tree-sitter-php" "v0.22.8" "php/src" )
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             ; (c-mode . c-ts-mode)
             ; (c++-mode . c++-ts-mode)
             ; (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))

;; HOOKS

;; Shell mode
(add-hook 'shell-mode-hook (lambda ()
			     (local-set-key (kbd "M-l") 'erase-buffer)))
;; Startup
(setq initial-buffer-choice 'recentf-open-files)

;; TypeScript & Javascript
(require 'prettier)
(defun ts-format-function()
  "Format js, ts, tsx and jsx files."
  (when (buffer-file-name)
	 (let ((file-ext (file-name-extension buffer-file-name)))
		(if (or (string-equal file-ext "ts")
				  (string-equal file-ext "tsx")
				  (string-equal file-ext "js")
				  (string-equal file-ext "jsx")
				  (string-equal file-ext "json"))
			 (prettier-prettify)))))
(global-prettier-mode)

;; Company
(require 'company)
(add-hook 'lsp-mode-hook 'company-mode)
(add-hook 'lsp-mode-hook 'lsp-diagnostics-mode)
;; C++
(add-hook 'c++-mode-hook 'lsp-mode)
(add-hook 'c++-mode-hook 'lsp-ui-mode)
(add-hook 'c++-mode-hook 'tree-sitter-hl-mode)
;; C
(add-hook 'c-mode-hook 'lsp-mode)
(add-hook 'c-mode-hook 'lsp-ui-mode)
(add-hook 'c-mode-hook 'tree-sitter-hl-mode)
;; Go
(add-hook 'go-mode-hook #'lsp-deferred)
(defun lsp-go-install-save-hooks ()
  "Lambda to format and organize imports in go files before saving."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(define-derived-mode complete-tsx-mode jtsx-tsx-mode "Complete TSX mode"
  "Major mode for editing TSX files with LSP support."
  (lsp))

(define-derived-mode complete-typescript-mode jtsx-typescript-mode "Complete Typescript Mode"
  "Major mode for editing Typescript files with LSP support."
  (lsp))

(define-derived-mode complete-jsx-mode jtsx-jsx-mode "Complete JS mode"
  "Major mode for editing JS files with LSP support."
  (lsp))

;(use-package jtsx
;  :ensure t
;  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
;			("\\.tsx\\'"  . jtsx-tsx-mode)
;			("\\.ts\\'"   . jtsx-typescript-mode)))

;; Javascript
;(add-hook 'js-mode-hook 'lsp-mode)
;(add-hook 'js-mode-hook 'lsp-ui-mode)
;(add-hook 'js-mode-hook 'jtsx-jsx-mode)
;; TypeScript
;(add-hook 'ts-mode-hook 'lsp-mode)
;(add-hook 'ts-mode-hook 'lsp-ui-mode)
;(add-hook 'ts-mode-hook 'jtsx-typescript-mode)
; (add-hook 'ts-mode-hook 'typescript-ts-mode)
;; TSX
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . complete-tsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . complete-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"  . complete-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'"  . complete-typescript-mode))

(use-package lsp-tailwindcss
  ;:straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (setq lsp-tailwindcss-major-modes '(jtsx-tsx-mode css-ts-mode)
        lsp-tailwindcss-experimental-class-regex
        [":class\\s+\"([^\"]*)\""
         ":[\\w-.#>]+\\.([\\w-]*)"
         "tw|yourModule\\(([^)]*)\\)"
         "[\"'`]([^\"'`]*).*?[\"'`]"
         ]
        lsp-tailwindcss-class-attributes ["class" "className" "ngClass" ":class"]))

;; JS, TS, TSX, JSX
(add-hook 'jtsx-jsx-mode-hook (add-hook 'before-save-hook 'ts-format-function))
(add-hook 'jtsx-typescript-mode-hook (add-hook 'before-save-hook 'ts-format-function))
(add-hook 'jtsx-tsx-mode-hook (add-hook 'before-save-hook 'ts-format-function))

;; Dart
(add-hook 'dart-mode-hook 'lsp-mode)
(add-hook 'dart-mode-hook 'lsp-ui-mode)
(add-hook 'dart-mode-hook 'tree-sitter-hl-mode)
;; Fountain
;;(setq whitespace-space font-lock-comment-face)
(add-hook 'fountain-mode-hook (lambda ()
				(setq-local face-remapping-alist '((default (:height (if (string-equal (system-name) "aldrinslaptop")
																							170
																						 200)) default)))
				(setq-local whitespace-style
					    '(face tabs tab-mark))
				(whitespace-mode)
				))
(defun my-wrap-lines ()
  "Disable `truncate-lines' in the current buffer."
  (setq truncate-lines nil))
(add-hook 'magit-diff-mode-hook 'my-wrap-lines)


;; THEMES

(load-theme 'atom-one-dark t)
  ;; (load-theme 'atom-dark t)
  ;; (load-theme 'kanagawa t)
  ;; (load-theme 'timu-spacegrey t)
  ;; (load-theme 'everforest-hard-dark t)
  ;; (load-theme 'almost-mono-black t)
  ;; (load-theme 'poet-dark-monochrome t)
  ;; (load-theme 'turbonight t)
