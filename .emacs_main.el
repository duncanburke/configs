;; -*- mode: Emacs-Lisp; -*-

(defun loade ()
  "Reload .emacs. This is defined at the beginning so that the file can be easily reloaded even if errors have occured."
  (interactive)
  (load-file "~/.emacs"))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not. Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed
 'ghc
 'company-ghc
 'haskell-mode
 'magit
 'markdown-mode
 'smart-tabs-mode
 'subatomic256-theme
 'gitconfig-mode
 'gitignore-mode
 'flx-ido)

;; Alternative: "DejaVu Sans Mono:style=Book:size=12"
(add-to-list 'default-frame-alist '(font . "Terminus:style=Regular:size=10"))

(load-theme 'subatomic256 t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(fringe-mode 0)
(scroll-bar-mode -1)

(setq-default
 indent-tabs-mode nil
 require-final-newline 'ask
 default-major-mode 'text-mode
 even-window-heights nil
 resize-mini-windows nil
 default-tab-width 4
 user-mail-address "duncankburke@gmail.com"
 diff-switches "-u"
 truncate-lines t
 mouse-yank-at-point t
 mouse-hilight 1)

(global-linum-mode t)

;; gc tuning
(setq gc-cons-threshold 20000000)

;; Show column and line numbers on status bar
(column-number-mode)

;; Get rid of the annoying spash screen
(setq inhibit-splash-screen t)

;; Don't follow symlinks to version-controlled files
(setq vc-follow-symlinks nil)

;; Stop pop-up windows
(setq pop-up-windows nil)

;; I rarely don't want to see trailing whitespace. Also, it's not very intrusive in windowed mode.
(setq show-trailing-whitespace t)

(mapc (lambda (sym) (put sym 'disabled nil))
      '(downcase-region
        upcase-region))

;; Make windows split vertically like C-x 3 for things like help, grep, compile, gdb etc.
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Answer y or n instead of yes or no at minibar prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Often things will open new windows for some reason. These can be useful, but one can be
;; left with a multitude of useless buffers lying around. Rather than just switching away from the offending
;; buffer, use C-z z. C-z x is somewhat unpredictable, as one isn't sure exactly which other buffer it's going
;; to close; so it's best used when there are two panes.
(defun close-and-kill-next-pane ()
  "Close the other pane and kill the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-buffer)
  (delete-window))

(defun close-and-kill-this-pane ()
  "Close this pane and kill the buffer in it also."
  (interactive)
  (kill-buffer)
  (delete-window))

(defun backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0))

(defun diff-region ()
  "Select a region to compare"
  (interactive)
  (when (use-region-p) ; there is a region
    (let (buf)
      (setq buf (get-buffer-create "*Diff-regionA*"))
      (save-current-buffer
        (set-buffer buf)
        (erase-buffer))
      (append-to-buffer buf (region-beginning) (region-end)))
    )
  (message "Now select other region to compare and run `diff-region-now`"))

(defun diff-region-now ()
  "Compare current region with region already selected by `diff-region`"
  (interactive)
  (when (use-region-p)
    (let (bufa bufb)
      (setq bufa (get-buffer-create "*Diff-regionA*"))
      (setq bufb (get-buffer-create "*Diff-regionB*"))
      (save-current-buffer
        (set-buffer bufb)
        (erase-buffer))
      (append-to-buffer bufb (region-beginning) (region-end))
      (ediff-buffers bufa bufb))))

;; Disable C-z. Normally, this would cause it to be minimised in a graphical environment, but it gets
;; confused with xmonad
(global-unset-key (kbd "C-z"))

;; Mirror of C-d
(global-set-key (kbd "C-D") 'backward-delete-char)

;; Bindings for window movement
(global-set-key (kbd "C-M-h") 'windmove-left)
(global-set-key (kbd "C-M-t") 'windmove-down)
(global-set-key (kbd "C-M-n") 'windmove-up)
(global-set-key (kbd "C-M-s") 'windmove-right)
(global-unset-key (kbd "C-x o"))

;; Alternate binding for M-x
(global-set-key (kbd "C-x RET") 'execute-extended-command)
(global-unset-key (kbd "M-x"))

(global-set-key (kbd "C-z x") 'close-and-kill-next-pane)
(global-set-key (kbd "C-z z") 'close-and-kill-this-pane)

;; Keybindings to the X clipboard
(global-set-key (kbd "s-u") 'clipboard-yank)
(global-set-key (kbd "s-e") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-c v") 'clipboard-yank)
(global-set-key (kbd "C-c c") 'clipboard-kill-ring-save)

;; Delete trailing whitespace
(global-set-key (kbd "C-x t") 'delete-trailing-whitespace)

;; Line wrap at right edge of screen
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)

;; Line numbers at left edge of screen
(global-set-key (kbd "C-c l") 'linum-mode)

;; Backwards kill line
(global-set-key (kbd "C-l") 'backward-kill-line)

;; replace buff-menu with bs-show
(global-set-key (kbd "C-x C-b") 'bs-show)

;; Show-hide menu
(global-set-key (kbd "C-x y") 'menu-bar-mode)

;; Show whitespace
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; Jump to the specified line number
(global-set-key (kbd "C-c a") 'goto-line)
(global-set-key (kbd "M-g") 'goto-line)

;; replace C-<arrow> bindings
(global-set-key (kbd "M-N") 'forward-paragraph)
(global-set-key (kbd "M-P") 'backward-paragraph)
(global-set-key (kbd "M-F") 'right-word)
(global-set-key (kbd "M-B") 'left-word)
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))

;; replace C-<backspace> binding
(global-set-key (kbd "M-D") 'backward-kill-word)
(global-unset-key (kbd "<C-backspace>"))

(autoload 'comment-region "newcomment" "")
(global-set-key (kbd "M-c") 'comment-region)
(autoload 'uncomment-region "newcomment" "")
(global-set-key (kbd "M-C") 'uncomment-region)

(global-set-key (kbd "C-c x") 'text-mode)

(global-set-key (kbd "C-c g") 'magit-status)

;; Enable the wip save minor mode for magit. wip-save still needs to be enabled on a
;; per-repository basis
(autoload 'global-magit-wip-save-mode "magit-wip" "")
(global-magit-wip-save-mode 1)

;; Keybinding to insert a fucking tab, rather than doing crazy indent
(defun command-insert-tab ()
  "Insert a tab character"
  (interactive)
  (insert "\t"))
(global-set-key (kbd "<C-tab>") 'command-insert-tab)

;; Make emacs stop asking silly questions about changed files.
;; Somewhat unsafe, and a matter of taste.
(defun ask-user-about-supersession-threat (fn)
  "blatantly ignore files that changed on disk")
(defun ask-user-about-lock (file opponent)
  "always grab lock" t)

(autoload 'smart-tabs-mode "smart-tabs-mode")
(autoload 'company-mode "company")
(autoload 'markdown-mode "markdown-mode")
(autoload 'haskell-mode "haskell")
(autoload 'haskell-cabal-mode "haskell-cabal")
(autoload 'haskell-doc-mode "haskell-doc")
(autoload 'haskell-c-mode "haskell-c")
(autoload 'ido-mode "ido")
(autoload 'ido-everywhere "ido")
(autoload 'flx-ido-mode "flx-ido")
(autoload 'gitconfig-mode "gitconfig-mode")
(autoload 'gitignore-mode "gitignore-mode")

(defun mode-extension (mode extension)
  (add-to-list 'auto-mode-alist `(,(concat "\\" extension "$") . ,mode)))

(mapc (lambda (a)
        (mode-extension (car a) (cdr a)))
      '((haskell-mode . ".hs")
        (haskell-mode . ".hs-boot")
        (haskell-cabal-mode . ".cabal")
        (markdown-mode . ".md")
        (markdown-mode . ".page")))

(defun tabstop-hook ()
  (define-key (current-local-map) (kbd "TAB") 'tab-to-tab-stop)
  (setq tab-width 4
        indent-tabs-mode t
        tab-stop-list (number-sequence 4 200 4)))

(add-hook 'fundamental-mode-hook 'tabstop-hook)
(add-hook 'text-mode-hook 'tabstop-hook)
(add-hook 'conf-mode-hook 'tabstop-hook)
(add-hook 'markdown-mode-hook 'tabstop-hook)

(add-hook
 'dired-mode-hook
 '(lambda ()
    ;; Allow dired to recursive delete without confirmation
    (setq dired-recursive-deletes 'always)
    ;; Stop dired from spamming windows as you navigate
    (put 'dired-find-alternate-file 'disabled nil)))

(add-hook
 'c-mode-hook
 '(lambda ()
    (setq c-basic-offset 2
          tab-width 2
          show-trailing-whitespace t
          indent-tabs-mode t)
    (smart-tabs-mode t)
    (c-set-offset 'case-label '+)
    (c-toggle-auto-newline nil)
    (c-toggle-hungry-state t)
    (c-toggle-electric-state t)
    (c-toggle-syntactic-indentation t)
    (local-set-key (kbd "RET") 'newline-and-indent)
    (subword-mode t)))

(add-hook
 'python-mode-hook
 '(lambda ()
    (setq python-check-command "pychecker --stdlib -# 0 -xXT"
          tab-width 4
          python-indent 4)
    (global-set-key (kbd "C-.") 'python-shift-right)
    (global-set-key (kbd "C-,") 'python-shift-left)
    (local-set-key (kbd "RET") 'newline-and-indent)))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(add-hook
 'company-mode-hook
 '(lambda ()
    (add-to-list 'company-backends 'company-ghc)))

(add-hook
 'haskell-mode-hook
 '(lambda ()
    (setq indent-tabs-mode nil
          tab-width 2
          haskell-indentation-cycle-warn nil
          ghc-hlint-options '("--ignore=Use camelCase")
          show-trailing-whitespace t
          ghc-display-error 'minibuffer
          ghc-display-hole 'other-buffer)
    (ghc-init)
    (flyspell-prog-mode)
    (company-mode)
    (auto-fill-mode 1)
    (turn-on-haskell-indentation)
    (set (make-local-variable
          'fill-nobreak-predicate)
         (lambda ()
           (not (eq (get-text-property (point) 'face)
                    'font-lock-comment-face))))
    (define-key haskell-mode-map (kbd "M-s") 'ghc-display-errors)))

(add-hook
 'haskell-cabal-mode-hook
 '(lambda ()
    (setq indent-tabs-mode nil)))

(add-hook
 'org-mode-hook
 '(lambda ()
    (setq org-hide-leading-stars t
          org-startup-indented nil
          org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE")))))

(add-hook
 'lisp-mode-hook
 '(lambda ()
    (setq indent-tabs-mode nil
          lisp-indent-offset 4
          tab-width 4)
    (show-paren-mode t)
    (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook
 'markdown-mode-hook
 '(lambda ()
    (flyspell-mode)
    (visual-line-mode t)))

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(add-hook
 'ido-setup-hook
 '(lambda ()
    (setq ido-enable-flex-matching t
          ido-use-filename-at-point 'guess
          ido-create-new-buffer 'always
          ido-default-file-method 'selected-window
          ido-default-buffer-method 'selected-window
          ido-enable-flex-matching t
          ido-use-faces nil)))

;; monkey-patch magit to show patch on commit buffer
(advice-add #'magit-key-mode-popup-committing :after
            (lambda ()
              (magit-key-mode-toggle-option (quote committing) "--verbose")))
