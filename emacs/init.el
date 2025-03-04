(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(which-key magit highlight-indent-guides editorconfig popwin neotree company evil))
 '(tool-bar-mode nil)
 '(treesit-font-lock-level 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Input Mono" :foundry "FBI " :slant normal :weight regular :height 120 :width normal)))))


;; custom
;; disable toolbars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; enable recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; make emacs create all backup files in a very particular directory,
;; and make sure that backups are created as copies of the original file
(setq backup-directory-alist `(("." . "~/.emacs-backups")))
(setq backup-by-copying t)
;; change the number of held backups
(setq delete-old-versions t
      kept-new-version 6
      kept-old-versions 2
      version-control t)

;; define func that creates emacs ctags files in a specified directory
(setq path-to-ctags "/usr/bin/ctags")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name)))
)

;; change default indentation options
(setq-default indent-tabs-mode nil) ;; indent with spaces
(setq-default tab-width 4) ;; and set a default indentation width
(setq-default c-basic-offset 4)
(setq-default c-indent-level 4)

;; display line numbers by default and disable line wrapping
(global-display-line-numbers-mode)
(setq-default truncate-lines t)
;; and enable a line ruler by default
(setq-default display-fill-column-indicator-column 80)
(global-display-fill-column-indicator-mode)

;; enable fido mode with vertical completions
(fido-mode t)
(icomplete-vertical-mode t)

;; custom functions
(defun connect-or-disconnect-lsp ()
  "Enable or disable lsp-mode in the current buffer."
  (interactive)
  (if lsp-mode
      (lsp-disconnect)
      (lsp))
)

;; remap major modes for treesittre
(setq major-mode-remap-alist
 '((sh-mode . sh-ts-mode)
   (bash-mode . bash-ts-mode)
   (js-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (c-mode . c-ts-mode)
   (cpp-mode . cpp-ts-mode)
   (java-mode . java-ts-mode)
   (python-mode . python-ts-mode)))

;; set up melpa packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

;; enable orderless for fido
(unless (package-installed-p 'orderless)
  (package-install 'catppuccin-theme))
(use-package orderless
  :ensure t
  :config
  (fido-mode)
  :custom
  (completion-styles '(orderless)))

(defun my-icomplete-styles ()
  (setq-local completion-styles '(orderless)))
(add-hook 'icomplete-minibuffer-setup-hook 'my-icomplete-styles)

;; enable our theme of choice
(unless (package-installed-p 'catppuccin-theme)
  (package-install 'catppuccin-theme))
(load-theme 'catppuccin t)

;; download and enable lsp-mode
(unless (package-installed-p 'lsp-mode)
  (package-install 'lsp-mode))
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")) ;; or 'C-l', 's-l'
  ;; :config
  ;; (lsp-enable-which-key-integration t))
;; setup mode hooks
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c-ts-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c++-ts-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'python-ts-mode-hook #'lsp)
(add-hook 'js-mode-hook #'lsp)
(add-hook 'js2-mode-hook #'lsp)
(add-hook 'js-ts-mode-hook #'lsp)
(add-hook 'sh-mode-hook #'lsp)
;; enable in most programming modes anyway
;; (add-hook 'prog-mode-hook #'lsp)

;; download and enable company
(unless (package-installed-p 'company)
  (package-install 'company))
(company-mode 1)

;; download and enable evil
(unless (package-installed-p 'evil)
  (package-install 'evil))
(setq evil-want-C-u-scroll t) ;; enable C-u to scroll instead of repeat
(setq evil-want-fine-undo 'fine) ;; enable undo like vim
(require 'evil)
(evil-mode 1)
;; setup undo system
(evil-set-undo-system 'undo-redo)
;; setup custom evil keybinds
(evil-define-key 'normal 'global "gcc" 'comment-line)
(evil-define-key 'visual 'global "gc" 'comment-or-uncomment-region)
;; toggle lsp
(evil-define-key 'normal 'global (kbd "<SPC> a") 'connect-or-disconnect-lsp)

;; setup extra evil keybinds 
(evil-define-key 'normal 'global (kbd "<SPC> r f") 'recentf)
(evil-define-key 'normal 'global (kbd "<SPC> o f") 'find-file)
(evil-define-key 'normal 'global (kbd "<SPC> ,") 'switch-to-buffer)
(evil-define-key 'normal 'global (kbd "<SPC> g") 'rgrep)
(evil-define-key 'normal 'global (kbd "<SPC> l g") 'lgrep)
(evil-define-key 'normal 'global (kbd "<SPC> m") 'evil-show-marks)
(evil-define-key 'normal 'global (kbd "<SPC> t") 'tags-search)
(evil-define-key 'normal 'global (kbd "gtd") 'lsp-goto-type-definition)
(evil-define-key 'normal 'global (kbd "gk") 'lsp-describe-thing-at-point)
(evil-define-key 'normal 'global (kbd "gd") 'lsp-find-definition)
(evil-define-key 'normal 'global (kbd "gr") 'lsp-find-references)
(evil-define-key 'normal 'global (kbd "gi") 'lsp-find-implementation)
(evil-define-key 'normal 'global (kbd "<SPC> r n") 'lsp-rename)
(evil-define-key 'normal 'global (kbd "gh") 'flymake-show-diagnostic)
(evil-define-key 'normal 'global (kbd "gbh") 'flymake-show-buffer-diagnostics)
(evil-define-key 'normal 'global (kbd "gBh") 'flymake-show-project-diagnostics)

;; download and enable emacs-neotree
(unless (package-installed-p 'neotree)
  (package-install 'neotree))
(use-package neotree)
(evil-define-key 'normal 'global (kbd "<SPC> e") 'neotree-toggle)

;; download and enable editorconfig
(unless (package-installed-p 'editorconfig)
  (package-install 'editorconfig))
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; download and enable hightlight-indent-guides
(unless (package-installed-p 'highlight-indent-guides)
  (package-install 'highlight-indent-guides))
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; setup tree-sitter grammar alist
(setq treesit-language-source-alist
   '((cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (bash "https://github.com/tree-sitter/tree-sitter-bash")
     (java "https://github.com/tree-sitter/tree-sitter-java")))
;; and install the grammars
(dolist (lang treesit-language-source-alist)
  (unless (treesit-language-available-p (car lang))
    (treesit-install-language-grammar (car lang))))

;; let's also install and setup magit
(unless (package-installed-p 'magit)
  (package-install 'magit))

;; and which-key for ez completions and reminders
(unless (package-installed-p 'which-key)
  (package-install 'which-key))
(require 'which-key)
(which-key-mode)
