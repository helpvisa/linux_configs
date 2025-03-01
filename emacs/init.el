(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(package-selected-packages
   '(highlight-indent-guides editorconfig popwin neotree company evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; custom
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


;; custom functions
(defun connect-or-disconnect-lsp ()
  "Enable or disable lsp-mode in the current buffer."
  (interactive)
  (if lsp-mode
      (lsp-disconnect)
      (lsp))
)


;; set up melpa packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

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
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'js-mode-hook #'lsp)
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

;; download and enable fzf
(unless (package-installed-p 'fzf)
  (package-install 'fzf))
(use-package fzf
  ;; :bind
  ;; set emacs keybinds here
  :config
  (setq fzf/args "-x --color bw --print-query --style=minimal --margin=1,0 --no-hscroll"
	fzf/executable "fzf"
	fzf/git-grep-args "-i --line-number %s"
	;; command used for 'fzf-grep-*' functions
	;; example usage for ripgrep:
	;; fzf/grep-command "rg --no-heading -nH"
	fzf/grep-command "grep -nrH"
	;; if nil, the fzf buffer appears at top of window
	fzf/position-bottom t
	fzf/window-height 15))
;; setup evil keybinds for fzf
(evil-define-key 'normal 'global (kbd "<SPC> o f") 'fzf-find-file)
(evil-define-key 'normal 'global (kbd "<SPC> o F") 'fzf-find-file-in-dir)
(evil-define-key 'normal 'global (kbd "<SPC> ,") 'fzf-switch-buffer)
(evil-define-key 'normal 'global (kbd "<SPC> g") 'fzf-grep)
(evil-define-key 'normal 'global (kbd "<SPC> G") 'fzf-grep-with-narrowing)

;; download and enable emacs-neotree
(unless (package-installed-p 'neotree)
  (package-install 'neotree))
(use-package neotree)
(evil-define-key 'normal 'global (kbd "<SPC> e") 'neotree-toggle)

;; download and enable popup windows with popwin.el
(unless (package-installed-p 'popwin)
  (package-install 'popwin))
(use-package popwin)
(popwin-mode 1)

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
