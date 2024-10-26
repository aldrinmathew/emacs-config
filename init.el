
;;; Code:
(set-face-attribute 'default nil
	:font (if (eq 'system-type 'windows-nt)
							  "Iosevka NF"
							"Iosevka Nerd Font")
	:height (if (string-equal (system-name) "AldrinsLaptop")
								130
							 170))

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
     magit
     nerd-icons
     php-mode
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

;; gray25 & light gray

(set-face-attribute 'mode-line-active nil
		    :background "slate blue"
		    :foreground "white"
		    :box nil)

(setq-default tab-width 3)
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
  (message buffer-file-name)
  (when (buffer-file-name)
	 (let ((file-ext (file-name-extension buffer-file-name)))
		(message file-ext)
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

(require 'magit)
(global-set-key (kbd "M-g") 'magit)

(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tree-sitter-query)
(require 'fountain-mode)
(require 'cmake-mode)
(require 'shell-pop)

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

;; HOOKS

;; Shell mode
(add-hook 'shell-mode-hook (lambda ()
			     (local-set-key (kbd "M-l") 'erase-buffer)))
;; Startup
(setq initial-buffer-choice 'recentf-open-files)

;; Company
(require 'company)
(add-hook 'lsp-mode-hook 'company-mode)
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
;; Javascript
(add-hook 'js-mode-hook 'lsp-mode)
(add-hook 'js-mode-hook 'lsp-ui-mode)
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js-mode-hook 'tree-sitter-hl-mode)
;; TypeScript
(add-hook 'ts-mode-hook 'lsp-mode)
(add-hook 'ts-mode-hook 'lsp-ui-mode)
(add-hook 'ts-mode-hook 'tree-sitter-hl-mode)
;; Rust
(add-hook 'rust-mode-hook 'lsp-mode)
(add-hook 'rust-mode-hook 'lsp-ui-mode)
(add-hook 'rust-mode-hook 'tree-sitter-hl-mode)
;; Dart
(add-hook 'dart-mode-hook 'lsp-mode)
(add-hook 'dart-mode-hook 'lsp-ui-mode)
(add-hook 'dart-mode-hook 'tree-sitter-hl-mode)
;; Fountain
;;(setq whitespace-space font-lock-comment-face)
(add-hook 'fountain-mode-hook (lambda ()
				(setq-local face-remapping-alist '((default (:height 200) default)))
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
