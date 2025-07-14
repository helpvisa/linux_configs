(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-diff-options "-w")
 '(ediff-window-setup-function 'ediff-setup-windows-plain))
(custom-set-faces
 '(default ((t (:family "IosevkaTerm Nerd Font" :foundry "UKWN" :slant normal :weight regular :height 120 :width normal)))))

;; update load path
(add-to-list 'load-path "~/.config/emacs/custom-elisp")
;; require custom elisp here

;; set default size for new frames
(setq default-frame-alist '((width . 90)
                            (height . 36)))
;; and set a default PGTK delay
(setq-default pgtk-wait-for-event-timeout 0)
;; and enable hover-focus
(setq mouse-autoselect-window t)

;; disable splash
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; disable toolbars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; and enable xterm-mouse-mode for terminal mouse interaction
(xterm-mouse-mode 1)
;; enable global word wrapping too
(setq-default word-wrap t)

;; enable recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; enable repeatable mark hopping, and decrease # of marks for more usability
(setq-default set-mark-command-repeat-pop t)
(setq mark-ring-max 6)
(setq global-mark-ring-max 8)

;; delete the currently selected text when typing
(delete-selection-mode 1)

;; disable audible bell, because it is very annoying
(setq visible-bell 1)

;; make emacs create all backup files in a very particular directory,
;; and make sure that backups are created as copies of the original file
;; first, make the directories in case they don't exist
(make-directory "~/.config/emacs/backups/" t)
(make-directory "~/.config/emacs/autosaves/" t)
(make-directory "~/.config/emacs/locks/" t)
(setq backup-directory-alist `(("." . "~/.config/emacs/backups/")))
(setq backup-by-copying t)
;; change the number of held backups
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 3
      version-control t)
;; also change autosave directory
(setq auto-save-file-name-transforms `((".*" "~/.config/emacs/autosaves/\1" t)))
;; and make lockfiles go somewhere nice
(setq lock-file-name-transforms '((".*" "~/.config/emacs/locks/", t)))

;; change the default view up / view down keys to be half-view
(require 'view)
(global-set-key "\C-v" 'View-scroll-half-page-forward)
(global-set-key "\M-v" 'View-scroll-half-page-backward)
(global-set-key [?\C-\S-v] 'View-scroll-line-forward)
(global-set-key "\M-\S-v" 'View-scroll-line-backward)

;; change default indentation options
(setq-default tab-width 8) ;; and set a default indentation width
(setq-default indent-tabs-mode nil) ;; indent with spaces
(setq-default standard-indent 4)
(setq-default c-basic-offset 4)
(setq-default c-ts-mode-indent-offset 4)
(setq-default c-indent-level 4)

;; display line numbers by default
(setq-default display-line-numbers 'visual)
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
                            (flyspell-mode 1)
                            (display-line-numbers-mode 0)
                            (message "Activating mail-mode hooks.")))
;; do the same for markdown mode
(add-hook 'markdown-mode-hook (lambda ()
                                (setq indent-tabs-mode nil)
                                (setq tab-width 4)
                                (setq indent-line-function (quote insert-tab))
                                (adaptive-wrap-prefix-mode 1)
                                (flyspell-mode 1)))

;; set up melpa packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)  ;; not needed in Emacs >= 27
(when (not package-archive-contents)
  (package-refresh-contents))

;; make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; install adaptive wrapping
(unless (package-installed-p 'adaptive-wrap)
  (package-install 'adaptive-wrap))
;; and enable adaptive line wrapping in *all* modes
(add-hook 'prog-mode-hook #'adaptive-wrap-prefix-mode)
(add-hook 'text-mode-hook #'adaptive-wrap-prefix-mode)
(with-eval-after-load 'comint
  (add-hook 'comint-mode-hook #'adaptive-wrap-prefix-mode))

;; also enable eglot in all programming modes while we're here
(add-hook 'prog-mode-hook 'eglot-ensure)

;; custom functions
;; restore cursor type based on mode
(defun restore-cursor-type ()
  (if overwrite-mode
      (setq cursor-type 'hbar)
    (setq cursor-type 'box)))

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

;; setup cursor changes based on mode
(add-hook 'overwrite-mode-hook
          (lambda ()
            (if overwrite-mode
                (setq cursor-type 'hbar)
              (setq cursor-type 'box))))
(add-hook 'activate-mark-hook
          (lambda ()
            (if (not evil-mode)
                (setq cursor-type 'bar))))
(add-hook 'deactivate-mark-hook
          (lambda ()
            (restore-cursor-type)))
(add-hook 'isearch-mode-hook
          (lambda ()
            (setq cursor-type 'bar)))
(add-hook 'isearch-mode-end-hook
          (lambda ()
            (restore-cursor-type)))

;; preview regular expressions for search-and-replace
(unless (package-installed-p 'visual-regexp)
  (package-install 'visual-regexp))
(require 'visual-regexp)

;; install whole-line-or-region
(unless (package-installed-p 'whole-line-or-region)
  (package-install 'whole-line-or-region))
(require 'whole-line-or-region)
(whole-line-or-region-global-mode 1)

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

;; install whichever theme set most intrigues you
; (unless (package-installed-p 'ef-themes)
;   (package-install 'ef-themes))
; (unless (package-installed-p 'stimmung-themes)
;   (package-install 'stimmung-themes))
; (unless (package-installed-p 'kuronami-theme)
;   (package-install 'kuronami-theme))
; (load-theme 'your-theme-of-choice t)
;; we'll just load a built-in default that's less ugly in the meantime
(load-theme 'tango-dark)

;; acquire lua-mode
(unless (package-installed-p 'lua-mode)
  (package-install 'lua-mode))

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
;; optionally enable below for less intrusive behaviour
; (setq company-frontends '(company-tng-frontend
;                           company-echo-strip-common-frontend))
;; enable company globally
(company-mode 1)
(add-hook 'after-init-hook 'global-company-mode)

;; download evil, as a toggleable option
(unless (package-installed-p 'evil)
  (package-install 'evil))
(setq evil-want-C-u-scroll t) ;; enable C-u to scroll instead of repeat
(setq evil-want-fine-undo 'fine) ;; enable undo like vim
(setq evil-want-keybinding 'nil)
(require 'evil)
;; setup undo system
(evil-set-undo-system 'undo-redo)
;; load from evil-collection (mode-by-mode)
(unless (package-installed-p 'evil-collection)
  (package-install 'evil-collection))
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
(evil-define-key 'normal 'global (kbd "<SPC> a") 'flycheck-mode)
;; setup extra keybinds 
(define-key my/keys-keymap (kbd "C-c C-g") 'comment-line)
(evil-define-key 'normal 'global "gcc" 'comment-line)
(evil-define-key 'visual 'global "gc" 'comment-or-uncomment-region)
(define-key my/keys-keymap (kbd "C-c C-r") 'recentf)
(evil-define-key 'normal 'global (kbd "<SPC> r f") 'recentf)
(evil-define-key 'normal 'global (kbd "<SPC> o f") 'find-file)
(evil-define-key 'normal 'global (kbd "<SPC> ,") 'switch-to-buffer)
(define-key my/keys-keymap (kbd "C-c C-s") 'rgrep)
(evil-define-key 'normal 'global (kbd "<SPC> g") 'rgrep)
(evil-define-key 'normal 'global (kbd "<SPC> m") 'evil-select-mark-from-list)
(evil-define-key 'normal 'global (kbd "<SPC> M") 'evil-show-marks-with-preview)
(evil-define-key 'normal 'global (kbd "<SPC> t") 'tags-search)
(define-key my/keys-keymap (kbd "C-c C-a d") 'eglot-find-typeDefinition)
(evil-define-key 'normal 'global (kbd "gtd") 'eglot-find-typeDefinition)
(define-key my/keys-keymap (kbd "C-c C-a D") 'eglot-find-definition)
(evil-define-key 'normal 'global (kbd "gd") 'eglot-find-definition)
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
(define-key my/keys-keymap (kbd "C-c C-c") 'term-interrupt-subjob)
(define-key my/keys-keymap (kbd "C-c *") 'highlight-symbol-at-point)
(define-key my/keys-keymap (kbd "C-c &") 'unhighlight-regexp)
;; visual-regexp binds
(define-key my/keys-keymap (kbd "C-c r") 'vr/replace)
(define-key my/keys-keymap (kbd "C-c q") 'vr/query-replace)
;; increase text scale
(define-key my/keys-keymap (kbd "C-=") 'text-scale-increase)
(define-key my/keys-keymap (kbd "C--") 'text-scale-decrease)
;; search for files with dired
(define-key my/keys-keymap (kbd "C-c f") 'find-dired)

;; download and enable flycheck for diagnostics under cursor
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(global-flycheck-mode +1)

;; download and enable editorconfig
(unless (package-installed-p 'editorconfig)
  (package-install 'editorconfig))
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

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

;; enable multicursor support
(unless (package-installed-p 'multiple-cursors)
  (package-install 'multiple-cursors))
(require 'multiple-cursors)
;; and setup some keybinds for em
(define-key my/keys-keymap (kbd "C-c c c") 'mc/edit-lines)
(define-key my/keys-keymap (kbd "C-c c n") 'mc/mark-next-like-this)
(define-key my/keys-keymap (kbd "C-c c p") 'mc/mark-previous-like-this)
(define-key my/keys-keymap (kbd "C-c c a") 'mc/mark-all-like-this)
(define-key my/keys-keymap (kbd "C-c c m") 'mc/mark-pop)
;; evil
(evil-define-key 'visual 'global (kbd "C-c c") 'mc/edit-lines)
(evil-define-key 'normal 'global (kbd "C-c n") 'mc/mark-next-like-this)
(evil-define-key 'normal 'global (kbd "C-c p") 'mc/mark-previous-like-this)
(evil-define-key 'normal 'global (kbd "C-c a") 'mc/mark-all-like-this)
(evil-define-key 'normal 'global (kbd "C-c m") 'mc/mark-pop)

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
