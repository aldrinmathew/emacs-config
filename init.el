;; My emacs configuration
;; Author: Aldrin Mathew

;; FONT STYLING

(set-face-attribute 'default nil :font (if (eq 'system-type 'windows-nt)
					   "Iosevka NF"
					 "Iosevka Nerd Font"))
(set-face-attribute 'default nil :height 170)

;; PACKAGES

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq package-list
  '(
     ace-window
     atom-one-dark-theme
     clang-format
     exec-path-from-shell
     fountain-mode
     go-mode
     js2-mode
     lsp-ui
     lsp-ivy
     lsp-treemacs
     lsp-mode
     magit
     projectile
     shell-pop
     treemacs
     treemacs-magit
     tree-sitter
     tree-sitter-hl
     tree-sitter-langs
     tree-sitter-query
     wakatime-mode
     which-key
  )
)

(dolist (package package-list)
  (when (and (not (package-installed-p package)) (assoc package package-archive-contents))
    (package-install package)))

;; LOADS

(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")


;; CONFIGURATIONS

(server-start)

(setq line-number-mode t)
(setq column-number-mode t)
(global-display-line-numbers-mode)

(require 'projectile)
(setq projectile-indexing-method 'alien)

(require 'clang-format)
(setq clang-format-style "LLVM")
(add-hook 'c++-mode-hook (lambda () (add-hook 'before-save-hook 'clang-format-buffer)))

(exec-path-from-shell-initialize)

(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)

(require 'treemacs)
(setq aw-ignored-buffers (delq 'treemacs-mode aw-ignored-buffers))
(add-hook 'emacs-startup-hook 'treemacs)
(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

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

(require 'shell-pop)
(custom-set-variables
 '(shell-pop-term-shell "/bin/bash")
 '(shell-pop-window-position "bottom")
 '(shell-pop-universal-key "C-<return>")
 '(shell-pop-autocd-to-working-dir t)
 '(shell-pop-restore-window-configuration t)
 '(shell-pop-cleanup-buffer-at-process-exit t))

(require 'wakatime-mode)
(global-wakatime-mode)


;; HOOKS

;; Shell mode
(add-hook 'shell-mode-hook (lambda ()
			     (local-set-key (kbd "M-l") 'erase-buffer)))
;; Startup
(custom-set-variables
 '(initial-buffer-choice 'recentf-open-files))
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
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
;;(add-hook 'go-mode-hook 'tree-sitter-hl-mode)
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

;; THEMES

(load-theme 'atom-one-dark t)
  ;; (load-theme 'atom-dark t)
  ;; (load-theme 'kanagawa t)
  ;; (load-theme 'timu-spacegrey t)
  ;; (load-theme 'everforest-hard-dark t)
  ;; (load-theme 'almost-mono-black t)
  ;; (load-theme 'poet-dark-monochrome t)
  ;; (load-theme 'turbonight t)
