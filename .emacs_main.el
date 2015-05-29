;; -*- mode: Emacs-Lisp; -*-

(defun loade ()
  "Reload .emacs. This is defined at the beginning so that the file can be easily reloaded even if errors have occured."
  (interactive)
  (load-file "~/.emacs"))

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages")
                          ("gnu" . "http://elpa.gnu.org/packages/")))
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

(defvar my-offset 4 "My indentation offset. ")
(defun backspace-whitespace-to-tab-stop ()
  "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
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

(defconst original-global-map (copy-keymap global-map))

(defvar my-key-bindings nil)
(defvar my-key-remaps nil)

(defvar my-keys-minor-mode-map (make-keymap))

(defun my-map-set-key (key command)
  (define-key my-keys-minor-mode-map key command))

;; (defun my-map-bind-key (key command)
;;   (setq my-key-bindings ((key . command) my-key-bindings)))
;; (defun my-map-remap-key (old new)
;;   (setq my-key-remaps ((old . new) my-key-remaps))

;; Disable C-z. Normally, this would cause it to be minimised in a graphical environment, but it gets
;; confused with xmonad
(global-unset-key (kbd "C-z"))

;; Rebind key prefixes we're going to use
(my-map-set-key (kbd "C-.") (lookup-key global-map (kbd "C-h")))
(my-map-set-key (kbd "C-p") (lookup-key global-map (kbd "C-u")))
(my-map-set-key (kbd "C-u") (lookup-key global-map (kbd "C-c")))
(my-map-set-key (kbd "C-e") (lookup-key global-map (kbd "C-q")))
(my-map-set-key (kbd "C-,") (lookup-key global-map (kbd "C-g")))
(my-map-set-key (kbd "C-b") (lookup-key global-map (kbd "C-z")))
(my-map-set-key (kbd "M-'") (lookup-key global-map (kbd "M-o")))

(global-unset-key (kbd "C-n"))
(global-unset-key (kbd "C-p"))
(global-unset-key (kbd "C-f"))
(global-unset-key (kbd "C-b"))
(global-unset-key (kbd "C-a"))
(global-unset-key (kbd "C-e"))

(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<prior>"))
(global-unset-key (kbd "<next>"))

(global-unset-key (kbd "M-n"))
(global-unset-key (kbd "M-p"))
(global-unset-key (kbd "M-f"))
(global-unset-key (kbd "M-b"))
(global-unset-key (kbd "M-a"))
(global-unset-key (kbd "M-e"))

(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))

(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))

(global-unset-key (kbd "<backspace>"))
(global-unset-key (kbd "DEL"))
(global-unset-key (kbd "<deletechar>"))
(global-unset-key (kbd "<C-backspace>"))
(global-unset-key (kbd "<M-backspace>"))

;; Cursor Movement Bindings
(my-map-set-key (kbd "C-h") 'backward-char)
(my-map-set-key (kbd "C-t") 'previous-line)
(my-map-set-key (kbd "C-n") 'next-line)
(my-map-set-key (kbd "C-s") 'forward-char)

(my-map-set-key (kbd "M-h") 'left-word)
(my-map-set-key (kbd "M-t") 'backward-paragraph)
(my-map-set-key (kbd "M-n") 'forward-paragraph)
(my-map-set-key (kbd "M-s") 'right-word)

(my-map-set-key (kbd "M-H") 'beginning-of-line)
(my-map-set-key (kbd "M-T") 'scroll-down-command)
(my-map-set-key (kbd "M-N") 'scroll-up-command)
(my-map-set-key (kbd "M-S") 'end-of-line)

;; (my-map-set-key (kbd "C-v") 'beginning-of-line)
;; (my-map-set-key (kbd "C-z") 'end-of-line)

;; Kill Bindings
(my-map-set-key (kbd "C-g") 'backward-delete-char)
(my-map-set-key (kbd "C-c") 'nil)
(my-map-set-key (kbd "C-r") 'nil)
(my-map-set-key (kbd "C-l") 'delete-char)

(my-map-set-key (kbd "M-g") 'backward-kill-word)
(my-map-set-key (kbd "M-c") 'backward-kill-paragraph)
(my-map-set-key (kbd "M-r") 'kill-paragraph)
(my-map-set-key (kbd "M-l") 'kill-word)

(my-map-set-key (kbd "M-G") 'backward-kill-line)
(my-map-set-key (kbd "M-C") 'nil)
(my-map-set-key (kbd "M-R") 'nil)
(my-map-set-key (kbd "M-L") 'kill-line)

;; Bindings for window movement
(my-map-set-key (kbd "C-M-h") 'windmove-left)
(my-map-set-key (kbd "C-M-t") 'windmove-up)
(my-map-set-key (kbd "C-M-n") 'windmove-down)
(my-map-set-key (kbd "C-M-s") 'windmove-right)
(global-unset-key (kbd "C-x o"))

(my-map-set-key (kbd "C-a") 'isearch-backward)
(my-map-set-key (kbd "C-o") 'isearch-forward)
(my-map-set-key (kbd "M-a") 'isearch-backward-regexp)
(my-map-set-key (kbd "M-o") 'isearch-forward-regexp)
(my-map-set-key (kbd "C-;") 'query-replace-string)
(my-map-set-key (kbd "C-q") 'query-replace-regexp)
(my-map-set-key (kbd "M-;") 'replace-string)
(my-map-set-key (kbd "M-q") 'replace-regexp)

(define-key isearch-mode-map (kbd "C-a") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-o") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-a") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "M-o") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-,") 'isearch-abort)
(define-key isearch-mode-map (kbd "C-e") 'isearch-quote-char)
(define-key isearch-mode-map (kbd "M-p") 'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "M-t") 'isearch-ring-advance)


;; Alternate binding for M-x
(my-map-set-key (kbd "C-x RET") 'execute-extended-command)
(global-unset-key (kbd "M-x"))

(my-map-set-key (kbd "C-b x") 'close-and-kill-next-pane)
(my-map-set-key (kbd "C-b z") 'close-and-kill-this-pane)
t
;; Keybindings to the X clipboard
(my-map-set-key (kbd "s-u") 'clipboard-yank)
(my-map-set-key (kbd "s-e") 'clipboard-kill-ring-save)
(my-map-set-key (kbd "C-u v") 'clipboard-yank)
(my-map-set-key (kbd "C-u c") 'clipboard-kill-ring-save)

;; Delete trailing whitespace
(my-map-set-key (kbd "C-x t") 'delete-trailing-whitespace)

;; Line wrap at right edge of screen
(my-map-set-key (kbd "C-u t") 'toggle-truncate-lines)

;; Line numbers at left edge of screen
(my-map-set-key (kbd "C-u l") 'linum-mode)

;; replace buff-menu with bs-show
(my-map-set-key (kbd "C-x C-b") 'bs-show)

;; Show-hide menu
(my-map-set-key (kbd "C-x y") 'menu-bar-mode)

;; Show whitespace
(my-map-set-key (kbd "C-u w") 'whitespace-mode)

;; Jump to the specified line number
(my-map-set-key (kbd "C-u a") 'goto-line)

(autoload 'comment-region "newcomment" "")
(my-map-set-key (kbd "C-'") 'comment-region)
(autoload 'uncomment-region "newcomment" "")
(my-map-set-key (kbd "M-'") 'uncomment-region)

(my-map-set-key (kbd "C-u x") 'text-mode)

(my-map-set-key (kbd "C-u g") 'magit-status)

;; Enable the wip save minor mode for magit. wip-save still needs to be enabled on a
;; per-repository basis
(autoload 'global-magit-wip-save-mode "magit-wip" "")
(global-magit-wip-save-mode 1)

;; Keybinding to insert a fucking tab, rather than doing crazy indent
(defun command-insert-tab ()
  "Insert a tab character"
  (interactive)
  (insert "\t"))
(my-map-set-key (kbd "<C-tab>") 'command-insert-tab)

(define-minor-mode my-keys-minor-mode
  "A minor mode for providing global keybindings with precedence over
any other loaded keymap."
  t " my-keys" 'my-keys-minor-mode-map)

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
      (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

(my-keys-minor-mode 1)

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
    (local-set-key (kbd "RET") 'newline-and-indent)))
    ;; (local-set-key (kbd "C-.") 'python-shift-right)
    ;; (local-set-key (kbd "C-,") 'python-shift-left)

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
          ido-use-filename-at-point nil
          ido-create-new-buffer 'always
          ido-default-file-method 'selected-window
          ido-default-buffer-method 'selected-window
          ido-enable-flex-matching t
          ido-use-faces nil)))

;; monkey-patch magit to show patch on commit buffer
(advice-add #'magit-key-mode-popup-committing :after
            (lambda ()
              (magit-key-mode-toggle-option (quote committing) "--verbose")))
