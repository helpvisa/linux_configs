(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-diff-options "-w")
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(global-display-line-numbers-mode t)
 '(treesit-font-lock-level 4))


;; custom
;; update load path
(add-to-list 'load-path "~/.config/emacs/custom-elisp")
;; require custom elisp here
;; (require 'simpc-mode)
;; (setq major-mode-remap-alist
;;  '((c-mode . simpc-mode)))
;; uncomment if you want emacs GTK windows to have no titlebar
;; (setq default-frame-alist '((undecorated . t)))

;; set default size for new frames
(setq default-frame-alist '((width . 90)
                            (height . 36)))
;; and set a default PGTK delay
(setq-default pgtk-wait-for-event-timeout 0)

;; disable toolbars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; and enable xterm-mouse-mode for terminal mouse interaction
(xterm-mouse-mode 1)
;; enable global line wrapping too
(global-visual-line-mode 1)

;; enable recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; enable repeatable mark hopping, and decrease # of marks for more usability
(setq-default set-mark-command-repeat-pop t)
(setq mark-ring-max 6)
(setq global-mark-ring-max 8)

;; make emacs create all backup files in a very particular directory,
;; and make sure that backups are created as copies of the original file
(setq backup-directory-alist `(("." . "~/.emacs-backups/")))
(setq backup-by-copying t)
;; change the number of held backups
(setq delete-old-versions t
      kept-new-version 6
      kept-old-versions 3
      version-control t)
;; also change autosave directory
(setq auto-save-file-name-transforms `((".*" "~/.emacs-autosaves/\1" t)))
;; make the directories too in case they don't exist
(make-directory "~/.emacs-backups/" t)
(make-directory "~/.emacs-autosaves/" t)

;; change the default view up / view down keys to be half-view
(require 'view)
(global-set-key "\C-v" 'View-scroll-half-page-forward)
(global-set-key "\M-v" 'View-scroll-half-page-backward)
(global-set-key [?\C-\S-v] 'View-scroll-line-forward)
(global-set-key "\M-\S-v" 'View-scroll-line-backward)

;; define func that creates emacs ctags files in a specified directory
(setq path-to-ctags "/usr/bin/ctags")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name)))
)

;; parameters functions to save and restore window splits
(defvar th-frame-config-register ?°
  "The register which is used for storing and restoring frame
configurations by `th-save-frame-configuration' and
`th-jump-to-register'.")

(defun th-save-frame-configuration (arg)
  "Stores the current frame configuration in register
`th-frame-config-register'. If a prefix argument is given, you
can choose which register to use."
  (interactive "P")
  (let ((register (if arg
                      (read-char "Which register? ")
                    th-frame-config-register)))
    (frame-configuration-to-register register)
    (message "Frame configuration saved in register '%c'."
             register)))

(defun th-jump-to-register (arg)
  "Jumps to register `th-frame-config-register'. If a prefix
argument is given, you can choose which register to jump to."
  (interactive "P")
  (let ((register (if arg
                      (read-char "Which register? ")
                    th-frame-config-register)))
    (jump-to-register register)
    (message "Jumped to register '%c'."
             register)))

(global-set-key (kbd "<f5>")
                'th-save-frame-configuration)
(global-set-key (kbd "<f9>")
                'th-jump-to-register)

;; change default indentation options
(setq-default tab-width 8) ;; and set a default indentation width
(setq-default indent-tabs-mode nil) ;; indent with spaces
(setq-default standard-indent 4)
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
;; ;; also add completion-preview-mode into most buffers
;; ;; code buffers
;; (add-hook 'prog-mode-hook #'completion-preview-mode)
;; ;; text buffers
;; (add-hook 'text-mode-hook #'completion-preview-mode)
;; ;; and in shell
;; (with-eval-after-load 'comint
;;   (add-hook 'comint-mode-hook #'completion-preview-mode))
;; ;; change some settings for completion-preview-mode
;; (with-eval-after-load 'completion-preview
;;   ;; show after two chars
;;   (setq completion-preview-minimum-symbol-length 2)
;;   ;; non-standard commands that should show preview
;;   (push 'org-self-insert-command completion-preview-commands)
;;   (push 'paredit-backward-delete completion-preview-commands)
;;   ;; some custom bindings
;;   (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
;;   (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
;;   ;; and select after cycling
;;   (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert))

;; enable flyspell, auto-fill, and writeroom if being used as email writer
(add-to-list 'auto-mode-alist '("/tmp/mutt*" . mail-mode))
(add-to-list 'auto-mode-alist '("/tmp/neomutt*" . mail-mode))
(add-hook 'mail-mode-hook (lambda ()
                            (auto-fill-mode)
                            (setq fill-column 72)
                            (flyspell-mode)
                            (display-line-numbers-mode 0)
                            (message "Activating mail-mode hooks.")))
;; do the same for markdown mode
(add-hook 'markdown-mode-hook (lambda ()
                                (setq indent-tabs-mode nil)
                                (setq tab-width 4)
                                (setq indent-line-function (quote insert-tab))
                                (flyspell-mode)))
(add-hook 'markdown-ts-mode-hook (lambda ()
                                   (setq indent-tabs-mode nil)
                                   (setq tab-width 4)
                                   (setq indent-line-function (quote insert-tab))
                                   (flyspell-mode)))
;; install adaptive wrapping
(unless (package-installed-p 'adaptive-wrap)
  (package-install 'adaptive-wrap))
;; and enable adaptive line wrapping in *all* modes
(add-hook 'prog-mode-hook #'adaptive-wrap-prefix-mode)
(add-hook 'text-mode-hook #'adaptive-wrap-prefix-mode)
(with-eval-after-load 'comint
  (add-hook 'comint-mode-hook #'adaptive-wrap-prefix-mode))

;; custom functions
;; call a shell command on the current file
(defun shell-command-on-current-file (command)
  "run a command using the current file as input"
  (interactive "sUse file in command: ")
  (let ((new-command (concat command " " (file-name-nondirectory buffer-file-name))))
    (shell-command new-command)))

;; remove last character from a given line
(defun remove-last-character-from-line (line)
  "Remove the final character from a given line."
  (substring line 0 (string-width line)))

;; get contents of a given line from a given buffer
(defun get-line-at-point-from-given-buffer (buffer line)
  "Given a buffer and line position, return a preview of the line."
  (let (preview))
  (save-excursion
    (set-buffer buffer)
    (goto-line line)
    (setq preview (thing-at-point 'line)))
  (remove-last-character-from-line preview))

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
(require 'cc-mode)
(eval-after-load 'cc-mode
  '(define-key c-mode-map (kbd "<DEL>") 'backspace-whitespace-to-tab-stop))


;; remap major modes for treesitter
; (setq major-mode-remap-alist
;  '((bash-mode . bash-ts-mode)
;    (js-mode . js-ts-mode)
;    (typescript-mode . typescript-ts-mode)
;    (json-mode . json-ts-mode)
;    (css-mode . css-ts-mode)
;    (java-mode . java-ts-mode)
;    (python-mode . python-ts-mode)))

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
  (package-install 'orderless))
(use-package orderless
  :ensure t
  :config
  (fido-mode)
  :custom
  (completion-styles '(orderless)))

(defun my-icomplete-styles ()
  (setq-local completion-styles '(orderless)))
(add-hook 'icomplete-minibuffer-setup-hook 'my-icomplete-styles)

;; enable our themes of choice
(unless (package-installed-p 'catppuccin-theme)
  (package-install 'catppuccin-theme))
;; (load-theme 'catppuccin t)
(unless (package-installed-p 'kuronami-theme)
  (package-install 'kuronami-theme))

;; acquire lua-mode
(unless (package-installed-p 'lua-mode)
  (package-install 'lua-mode))

;; download and enable lsp-mode
;; (unless (package-installed-p 'lsp-mode)
;;   (package-install 'lsp-mode))
;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")) ;; or 'C-l', 's-l'
;;   ;; :config
;;   ;; (lsp-enable-which-key-integration t))
;; ;; do NOT warn me when lsp-mode does not exist for a given mode
;; (setq lsp-warn-no-matched-clients nil)
;; ;; add modes to language list
;; ;; (add-to-list 'lsp-language-id-configuration '(simpc-mode . "c"))
;; ;; setup mode hooks
;; (add-hook 'simpc-mode-hook #'lsp)
;; (add-hook 'c-mode-hook #'lsp)
;; (add-hook 'c-ts-mode-hook #'lsp)
;; (add-hook 'c++-mode-hook #'lsp)
;; (add-hook 'c++-ts-mode-hook #'lsp)
;; (add-hook 'python-mode-hook #'lsp)
;; (add-hook 'python-ts-mode-hook #'lsp)
;; (add-hook 'js-mode-hook #'lsp)
;; (add-hook 'js2-mode-hook #'lsp)
;; (add-hook 'js-ts-mode-hook #'lsp)
;; (add-hook 'sh-mode-hook #'lsp)
;; (add-hook 'lua-mode-hook #'lsp)
;; ;; enable in most programming modes anyway
;; ;; (add-hook 'prog-mode-hook #'lsp)
;; ;; also download lsp-mode for java
;; (unless (package-installed-p 'lsp-java)
;;   (package-install 'lsp-java))
;; (require 'lsp-java)
;; (add-hook 'java-mode-hook #'lsp)
;; (add-hook 'java-ts-mode-hook #'lsp)
;; ;; write a function to toggle lsp-mode
;; (defun connect-or-disconnect-lsp ()
;;   "Enable or disable lsp-mode in the current buffer."
;;   (interactive)
;;   (if lsp-mode
;;       (lsp-disconnect)
;;       (lsp))
;; )

;; download and enable company
(unless (package-installed-p 'company)
  (package-install 'company))
(require 'company)
;; make company use tab-n-go mode
(add-hook 'after-init-hook 'company-tng-mode)
;; do not automatically complete to suggestion on pressing RET
(dolist (key '("<return>" "RET"))
  (define-key company-active-map (kbd key)
              `(menu-item nil company-complete
                          :filter ,(lambda (cmd)
                                     (when (company-explicit-action-p)
                                       cmd)))))
;; use tab to finish autocomplete
;; (define-key company-active-map (kbd "TAB") #'company-complete-selection)
;; (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
;; prevent space and other keys from doing anything unpredicatble in company
(define-key company-active-map (kbd "SPC") nil)
(define-key company-active-map (kbd "t") nil)
(define-key company-active-map (kbd "C-n") nil)
(define-key company-active-map (kbd "C-p") nil)
(define-key company-active-map (kbd "C-f") nil)
(define-key company-active-map (kbd "C-b") nil)
(define-key company-active-map (kbd "C-v") nil)
(define-key company-active-map (kbd "M-v") nil)
;; company can overrides above keymaps based on company-auto-complete-chars
;; turn this off to prevent that behaviour
(setq company-auto-complete-chars nil)
;; disable delay for showing suggestions
(setq company-idle-delay 0)
;; show suggestions after entering only 1 character
(setq company-minimum-prefix-length 1)
;; let suggestions list wrap back around to top
(setq company-selection-wrap-around 1)
;; enable company globally
(company-mode 1)
(add-hook 'after-init-hook 'global-company-mode)

;; download and enable evil
(unless (package-installed-p 'evil)
  (package-install 'evil))
(setq evil-want-C-u-scroll t) ;; enable C-u to scroll instead of repeat
(setq evil-want-fine-undo 'fine) ;; enable undo like vim
(setq evil-want-keybinding 'nil)
(require 'evil)
;; (evil-mode 1)
;; setup undo system
(evil-set-undo-system 'undo-redo)
;; load from evil-collection (mode-by-mode)
(unless (package-installed-p 'evil-collection)
  (package-install 'evil-collection))
;; (evil-collection-init 'magit)
;; (evil-collection-init 'ediff)
;; (evil-collection-init 'dired)
;; activate any evil-collection keymap interactively
(defun evil-collection-init-custom (keymap)
  "Initialize any evil-collection keymap from the interactive minibuffer."
  (interactive "sEnable keymap: ")
  (evil-collection-init (intern keymap)))
;; custom evil keybinds
;; split evil-show-marks into components
(evil-define-command evil-gather-marks (mrks)
  "Gather marks from evil-markers-alist. Call in other funcs.
If MRKS is non-nil it should be a string and only registers
corresponding to the characters of this string are shown."
  :repeat nil
  (let ((all-markers
        (append (cl-remove-if (lambda (m)
                                (or (evil-global-marker-p (car m))
                                    (not (markerp (cdr m)))))
                                evil-markers-alist)
                (cl-remove-if (lambda (m)
                                (or (not (evil-global-marker-p (car m)))
                                    (not (markerp (cdr m)))))
                                (default-value 'evil-markers-alist)))))
    (when mrks
        (setq mrks (string-to-list mrks))
        (setq all-markers (cl-delete-if (lambda (m)
                                        (not (member (car m) mrks)))
                                        all-markers)))
    ;; map marks to list of 4-tuples (char row col file)
    (setq all-markers
            (mapcar (lambda (m)
                    (with-current-buffer (marker-buffer (cdr m))
                        (save-excursion
                        (goto-char (cdr m))
                        (list (car m)
                                (line-number-at-pos (point))
                                (current-column)
                                (buffer-name)))))
                    all-markers))))

(evil-define-command evil-show-marks-with-preview (mrks)
  "Show all marks with line previews."
  :repeat nil
  (interactive "<a>")
  (let (all-markers))
  (setq all-markers (evil-gather-marks mrks))
    (evil-with-view-list
        :name "evil-marks"
        :mode-name "Evil Marks"
        :format [("Mark" 6 nil)
                ("Line" 6 nil)
                ("Col" 6 nil)
                ("Buffer" 24 nil)
                ("Preview" 1000 nil)]
        :entries (cl-loop for m in (sort all-markers (lambda (a b) (< (car a) (car b))))
                        collect `(nil [,(char-to-string (nth 0 m))
                                        ,(number-to-string (nth 1 m))
                                        ,(number-to-string (nth 2 m))
                                        (,(nth 3 m))
                                        ,(get-line-at-point-from-given-buffer
                                        (nth 3 m) (nth 1 m))]))
    :select-action #'evil--show-marks-select-action))

(evil-define-command evil-format-marks (mrks)
  "Format the marks into a string that can be used in other prompts / displays."
  (let (all-markers))
  (setq all-markers (evil-gather-marks mrks))
    (setq final-list (cl-loop for m in (sort all-markers (lambda (a b) (< (car a) (car b))))
                              collect (concat
                                       (byte-to-string (nth 0 m))
                                     " in "
                                     (nth 3 m)
                                     ":"
                                     (number-to-string (nth 1 m))
                                     " -> "
                                     (string-clean-whitespace
                                       (get-line-at-point-from-given-buffer
                                        (nth 3 m) (nth 1 m)))))))

(evil-define-command evil-select-mark-from-list (mrks)
  "Show all marks in a minibuffer and select from them."
  (interactive "<a>")
  (let (selection))
  (setq selection (completing-read "Select mark: " (evil-format-marks nil)))
  (evil-goto-mark (string-to-char (substring selection 0 1))))

;; other custom keymaps
;; use a minor-mode to prevent stupid major-modes from rebinding C-c commands
;; add to `emulation-mode-map-alists` so it also takes precedence over other
;; miscreant minor-modes
(defvar my/keys-keymap (make-keymap)
  "Keymap for my/keys-mode.")
(define-minor-mode my/keys-mode
  "Minor mode to prevent major-modes from overwriting custom keybinds."
  :init-value t
  :global t
  :keymap my/keys-keymap)
(add-to-list 'emulation-mode-map-alists
             `((my/keys-mode . ,my/keys-keymap)))
(my/keys-mode)
;; pane travels for standard emacs bindings
(define-key my/keys-keymap (kbd "C-c C-w h") 'evil-window-left)
(define-key my/keys-keymap (kbd "C-c C-w l") 'evil-window-right)
(define-key my/keys-keymap (kbd "C-c C-w k") 'evil-window-up)
(define-key my/keys-keymap (kbd "C-c C-w j") 'evil-window-down)
;; toggle lsp and flycheck
(define-key my/keys-keymap (kbd "C-c C-a a") 'flycheck-mode)
;; (define-key my/keys-keymap (kbd "C-c C-a A") 'connect-or-disconnect-lsp)
;; (evil-define-key 'normal 'global (kbd "<SPC> A") 'connect-or-disconnect-lsp)
(evil-define-key 'normal 'global (kbd "<SPC> a") 'flycheck-mode)
;; setup extra evil keybinds 
(define-key my/keys-keymap (kbd "C-c C-g") 'comment-line)
(evil-define-key 'normal 'global "gcc" 'comment-line)
(evil-define-key 'visual 'global "gc" 'comment-or-uncomment-region)
(define-key my/keys-keymap (kbd "C-c C-r") 'recentf)
(evil-define-key 'normal 'global (kbd "<SPC> r f") 'recentf)
(evil-define-key 'normal 'global (kbd "<SPC> o f") 'find-file)
(evil-define-key 'normal 'global (kbd "<SPC> ,") 'switch-to-buffer)
(define-key my/keys-keymap (kbd "C-c C-s") 'lgrep)
(evil-define-key 'normal 'global (kbd "<SPC> g") 'rgrep)
(evil-define-key 'normal 'global (kbd "<SPC> l g") 'lgrep)
(evil-define-key 'normal 'global (kbd "<SPC> m") 'evil-select-mark-from-list)
(evil-define-key 'normal 'global (kbd "<SPC> M") 'evil-show-marks-with-preview)
(evil-define-key 'normal 'global (kbd "<SPC> t") 'tags-search)
(define-key my/keys-keymap (kbd "C-c C-a d") 'eglot-find-typeDefinition)
(evil-define-key 'normal 'global (kbd "gtd") 'eglot-find-typeDefinition)
;; (define-key my/keys-keymap (kbd "C-c C-a K") 'lsp-describe-thing-at-point)
;; (evil-define-key 'normal 'global (kbd "gk") 'lsp-describe-thing-at-point)
(define-key my/keys-keymap (kbd "C-c C-a D") 'eglot-find-definition)
(evil-define-key 'normal 'global (kbd "gd") 'eglot-find-definition)
;; (define-key my/keys-keymap (kbd "C-c C-a r") 'lsp-find-references)
;; (evil-define-key 'normal 'global (kbd "gr") 'lsp-find-references)
(define-key my/keys-keymap (kbd "C-c C-a i") 'eglot-find-implementation)
(evil-define-key 'normal 'global (kbd "gi") 'eglot-find-implementation)
(define-key my/keys-keymap (kbd "C-c C-a n") 'eglot-rename)
(evil-define-key 'normal 'global (kbd "<SPC> r n") 'eglot-rename)
(define-key my/keys-keymap (kbd "C-c C-a h") 'display-local-help)
(evil-define-key 'normal 'global (kbd "gh") 'display-local-help)
(evil-define-key 'normal 'global (kbd "gbh") 'flymake-show-buffer-diagnostics)
(evil-define-key 'normal 'global (kbd "gBh") 'flymake-show-project-diagnostics)
(define-key my/keys-keymap (kbd "M-C-1") 'shell-command-on-current-file)
(evil-define-key 'normal 'global (kbd "M-C-1") 'shell-command-on-current-file)
(define-key my/keys-keymap (kbd "C-c C-c C-c") 'term-interrupt-subjob)

;; download and enable flycheck for diagnostics under cursor
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(global-flycheck-mode +1)

;; download and enable emacs-neotree
(unless (package-installed-p 'neotree)
  (package-install 'neotree))
(use-package neotree)
(evil-define-key 'normal 'global (kbd "<SPC> e") 'neotree-toggle)

(eval-after-load 'neotree
  '(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter))

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
;; add markdown-ts-mode
(use-package markdown-ts-mode
  ;; :mode ("\\.md\\'" . markdown-ts-mode)
  :mode ("\\.mtsd\\'" . markdown-ts-mode)
  :defer 't)
;; and install the grammars
(dolist (lang treesit-language-source-alist)
  (unless (treesit-language-available-p (car lang))
    (treesit-install-language-grammar (car lang))))

;; let's also install and setup magit
(unless (package-installed-p 'magit)
  (package-install 'magit))
;; and git-gutter
(unless (package-installed-p 'git-gutter)
  (package-install 'git-gutter))
(global-git-gutter-mode +1)

;; and which-key for ez completions and reminders
(unless (package-installed-p 'which-key)
  (package-install 'which-key))
(require 'which-key)
(which-key-mode)

;; use popwin to free us of annoying splits for *help* and such
;; (unless (package-installed-p 'popwin)
;;   (package-install 'popwin))
;; (require 'popwin)
;; (popwin-mode 1)
;; and setup some custom modes for our annoying *lsp-help* buffers
;; (push "*lsp-help*" popwin:special-display-config)

;; writeroom for distraction-free writing
(unless (package-installed-p 'writeroom-mode)
  (package-install 'writeroom-mode))

;; enable libvterm
(use-package vterm
  :ensure t)
(evil-define-key 'normal 'global (kbd "<SPC> o t") 'vterm)

;; enable multicursor support
(unless (package-installed-p 'multiple-cursors)
  (package-install 'multiple-cursors))
(require 'multiple-cursors)
;; and setup some keybinds for em
(define-key my/keys-keymap (kbd "C-c C-c c") 'mc/edit-lines)
(define-key my/keys-keymap (kbd "C-c C-c n") 'mc/mark-next-like-this)
(define-key my/keys-keymap (kbd "C-c C-c p") 'mc/mark-previous-like-this)
(define-key my/keys-keymap (kbd "C-c C-c a") 'mc/mark-all-like-this)
(define-key my/keys-keymap (kbd "C-c C-c m") 'mc/mark-pop)
;; evil
(evil-define-key 'visual 'global (kbd "C-c c") 'mc/edit-lines)
(evil-define-key 'normal 'global (kbd "C-c n") 'mc/mark-next-like-this)
(evil-define-key 'normal 'global (kbd "C-c p") 'mc/mark-previous-like-this)
(evil-define-key 'normal 'global (kbd "C-c a") 'mc/mark-all-like-this)
(evil-define-key 'normal 'global (kbd "C-c m") 'mc/mark-pop)

;; download and enable slime
(unless (package-installed-p 'slime)
  (package-install 'slime))
(require 'slime)
(slime-setup '(slime-fancy slime-quicklisp slime-asdf slime-mrepl))
;; make sure inferior-lisp-program is correct
;; emacs looks for "lisp" for some reason
(setq inferior-lisp-program "/usr/bin/sbcl")

;; download and enable simple-httpd
(unless (package-installed-p 'simple-httpd)
  (package-install 'simple-httpd))
(require 'simple-httpd)

;; allow changing the cursor appearance in the terminal
(unless (package-installed-p 'evil-terminal-cursor-changer)
  (package-install 'evil-terminal-cursor-changer))
(require 'evil-terminal-cursor-changer)
(etcc-on)

;; download and enable evil-rsi-mode
(define-minor-mode evil-rsi-mode
  "Rsi mode."
  :lighter " rsi"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (evil-define-key 'insert map "\C-o" #'evil-execute-in-normal-state)
            (evil-define-key 'insert map "\C-r" #'evil-paste-from-register)
            (evil-define-key 'insert map "\C-v" #'quoted-insert)
            (evil-define-key 'insert map (kbd "C-S-k") #'evil-insert-digraph)
            (evil-define-key 'insert map "\C-e" #'end-of-line)
            (when evil-want-C-w-delete
              (evil-define-key 'insert map "\C-w" #'evil-delete-backward-word))
            map)
  (if evil-rsi-mode
      (progn
        (evil-update-insert-state-bindings nil t)
        (when evil-want-C-w-delete
          (define-key minibuffer-local-map [remap kill-region] #'evil-delete-backward-word))
        (define-key evil-ex-completion-map [remap evil-insert-digraph] #'kill-line)
        (define-key evil-ex-completion-map "\C-S-k" #'evil-insert-digraph)
        (define-key evil-ex-completion-map "\C-a" #'beginning-of-line))
    (evil-update-insert-state-bindings)
    (define-key minibuffer-local-map [remap kill-region] nil)
    (define-key evil-ex-completion-map [remap evil-insert-digraph] nil)
    (define-key evil-ex-completion-map "\C-a" #'evil-ex-completion)))
;; now enable the minor mode
(evil-rsi-mode)


(provide 'init)
;;; init.el ends here
