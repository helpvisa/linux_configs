(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(multiple-cursors evil-collection vdiff vterm writeroom-mode flycheck which-key magit highlight-indent-guides editorconfig popwin neotree company evil))
 '(tool-bar-mode nil)
 '(treesit-font-lock-level 4))


;; custom
;; setup some custom keybindings
;; scroll by a half-view's length even with default emacs binds
(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")
(define-key global-map (kbd "C-v") 'View-scroll-half-page-forward)
(define-key global-map (kbd "M-v") 'View-scroll-half-page-backward)
(define-key global-map (kbd "C-c /") 'comment-line)
(define-key global-map (kbd "C-c C-/") 'comment-or-uncomment-region)
(define-key global-map (kbd "C-c r") 'recentf)
(define-key global-map (kbd "C-c b") 'switch-to-buffer)
(define-key global-map (kbd "C-c g") 'rgrep)

;; update load path
(add-to-list 'load-path "~/.config/emacs/custom-elisp")
;; uncomment if you want emacs GTK windows to have no titlebar
;; (setq default-frame-alist '((undecorated . t)))

;; set default size for new frames
(setq default-frame-alist '((width . 90)
                            (height . 36)))

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
(setq-default c-ts-mode-indent-offset 4)
(setq-default c-indent-level 4)

;; display line numbers by default and disable line wrapping
(global-display-line-numbers-mode)
(setq-default truncate-lines t)
;; and enable a line ruler by default
(setq-default fill-column 79)
(setq-default display-fill-column-indicator-column 80)
(global-display-fill-column-indicator-mode)

;; enable fido mode with vertical completions
(fido-mode t)
(icomplete-vertical-mode t)

;; enable flyspell, auto-fill, and writeroom if being used as email writer
(add-to-list 'auto-mode-alist '("/tmp/mutt*" . mail-mode))
(add-to-list 'auto-mode-alist '("/tmp/neomutt*" . mail-mode))
(add-hook 'mail-mode-hook (lambda ()
                            (auto-fill-mode)
                            (setq fill-column 72)
                            (flyspell-mode)
                            (display-line-numbers-mode 0)
                            (message "Activating mail-mode hooks.")))

;; custom functions
;; toggle lsp
(defun connect-or-disconnect-lsp ()
  "Enable or disable lsp-mode in the current buffer."
  (interactive)
  (if lsp-mode
      (lsp-disconnect)
      (lsp))
)
;; backspace to previous tabstop and replace backspace with it ...
(defvar my-offset 4 "My indentation offset.")
(defun backspace-whitespace-to-tab-stop ()
  "Delete whitespace to the next tab-stop, otherwise delete one character."
  (interactive)
  (if (or indent-tabs-mode
          (region-active-p)
          (save-excursion
            (> (point) (progn (back-to-indentation)
                              (point)))))
      (call-interactively 'backward-delete-char-untabify)
    (let ((movement (% (current-column) my-offset))
          (p (point)))
      (when (= movement 0) (setq movement my-offset))
      ;; Account for edge case near beginning of buffer
      (setq movement (min (- p 1) movement))
      (save-match-data
        (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char (- (match-end 1) (match-beginning 1)))
          (call-interactively 'backward-delete-char))))))
;; rebind backspace to this func
(global-set-key (kbd "<DEL>") 'backspace-whitespace-to-tab-stop)

;; remap major modes for treesitter
(setq major-mode-remap-alist
 '((bash-mode . bash-ts-mode)
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

;; make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

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
  (setq lsp-keymap-prefix "C-c l") ;; or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))
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
(add-hook 'bash-mode-hook #'lsp)
(add-hook 'bash-ts-mode-hook #'lsp)
;; enable in most programming modes anyway
;; (add-hook 'prog-mode-hook #'lsp)
(define-key global-map (kbd "C-c f d") 'lsp-find-definition)
(define-key global-map (kbd "C-c f r") 'lsp-find-references)
(define-key global-map (kbd "C-c f R") 'lsp-rename)
(define-key global-map (kbd "C-c f h") 'display-local-help)
(define-key global-map (kbd "C-c f b h") 'flymake-show-buffer-diagnostics)
(define-key global-map (kbd "C-c f p h") 'flymake-show-project-diagnostics)

;; download and enable company
(unless (package-installed-p 'company)
  (package-install 'company))
(company-mode 1)

;; download and enable pycheck for diagnostics under cursor
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(global-flycheck-mode +1)

;; download and enable emacs-neotree
(unless (package-installed-p 'neotree)
  (package-install 'neotree))
(use-package neotree)

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

;; use popwin to free us of annoying splits for *help* and such
(unless (package-installed-p 'popwin)
  (package-install 'popwin))
(require 'popwin)
(popwin-mode 1)
;; and setup some custom modes for our annoying *lsp-help* buffers
(push "*lsp-help*" popwin:special-display-config)

;; writeroom for distraction-free writing
(unless (package-installed-p 'writeroom-mode)
  (package-install 'writeroom-mode))

;; enable libvterm
(use-package vterm
  :ensure t)

;; enable multicursor support
(unless (package-installed-p 'multiple-cursor)
  (package-install 'multiple-cursors))
(require 'multiple-cursors)
;; and setup some keybinds for em
(define-key global-map (kbd "C-c c c") 'mc/edit-lines)
(define-key global-map (kbd "C-c c n") 'mc/mark-next-like-this)
(define-key global-map (kbd "C-c c p") 'mc/mark-previous-like-this)
(define-key global-map (kbd "C-c c a") 'mc/mark-all-like-this)


(provide 'init)
;;; init.el ends here
