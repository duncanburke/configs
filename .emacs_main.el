;; -*- mode: Emacs-Lisp; -*-

(defun loade ()
  "Reload .emacs. This is defined at the beginning so that the file can be easily reloaded even if errors have occured."
  (interactive)
  (load-file "~/.emacs"))

(defun try-load (feature)
  (require feature nil 'noerror))

(defun mode-extension (mode extension)
  (add-to-list 'auto-mode-alist `(,(concat "\\" extension "$") . ,mode)))

;; Initialise packaging
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Alternative: "DejaVu Sans Mono:style=Book:size=12"
(add-to-list 'default-frame-alist '(font . "Terminus:style=Regular:size=10"))

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(if window-system
    (progn
      ;; Disable icons on the menu
      (tool-bar-mode -1)
      (fringe-mode 0)
      ;; Remove scroll bars
      (scroll-bar-mode -1)

      (require 'color-theme)
      (if (try-load 'color-theme-solarized)
          (color-theme-solarized-dark)))
  (progn
    (load-theme 'subatomic256 t)
    (menu-bar-mode -1)))


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

;; Show column and line numbers on status bar
(column-number-mode)

;; Stop dired from spamming windows as you navigate
(put 'dired-find-alternate-file 'disabled nil)

;; Allow dired to recursive delete without confirmation
(setq dired-recursive-deletes 'always)

;; Get rid of the annoying spash screen
(setq inhibit-splash-screen t)

;; Don't follow symlinks to version-controlled files
(setq vc-follow-symlinks nil)

(mapc (lambda (sym) (put sym 'disabled nil))
      '(downcase-region
        upcase-region))

;; Make windows split vertically like C-x 3 for things like help, grep, compile, gdb etc.
(setq split-height-threshold nil)
(setq split-width-threshold 0)

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

;; Disable C-z. Normally, this would cause it to be minimised in a graphical environment, but it gets
;; confused with xmonad
(define-key (current-global-map) (kbd "C-z") 'nil)

;; Alternate binding for M-x
(define-key (current-global-map) (kbd "C-x RET") 'execute-extended-command)

(define-key (current-global-map) (kbd "C-z x") 'close-and-kill-next-pane)
(define-key (current-global-map) (kbd "C-z z") 'close-and-kill-this-pane)

;; Keybindings to the X clipboard
(define-key (current-global-map) (kbd "s-v") 'clipboard-yank)
(define-key (current-global-map) (kbd "s-c") 'clipboard-kill-ring-save)
(define-key (current-global-map) (kbd "C-c v") 'clipboard-yank)
(define-key (current-global-map) (kbd "C-c c") 'clipboard-kill-ring-save)

;; Delete trailing whitespace
(define-key (current-global-map) (kbd "C-x t") 'delete-trailing-whitespace)

;; Line wrap at right edge of screen
(define-key (current-global-map) (kbd "C-c t") 'toggle-truncate-lines)

;; Line numbers at left edge of screen
(define-key (current-global-map) (kbd "C-c l") 'linum-mode)

;; Show-hide menu
(define-key (current-global-map) (kbd "C-x y") 'menu-bar-mode)

;; Show whitespace
(define-key (current-global-map) (kbd "C-c w") 'whitespace-mode)

;; Jump to the specified line number
(define-key (current-global-map) (kbd "C-c a") 'goto-line)

;; What is says on the box
(define-key (current-global-map) (kbd "C-c 1") 'compile)

;; Alternate undo which doesn't undo undos like normal undo
(define-key (current-global-map) (kbd "C-?") 'undo-only)

;; Trying to get this one to work
(define-key (current-global-map) (kbd "C-M-x") '(switch-to-buffer "*scratch*"))

;; The various modes
(define-key (current-global-map) (kbd "C-c p") 'c++-mode)
(define-key (current-global-map) (kbd "C-c h") 'haskell-mode)
(define-key (current-global-map) (kbd "C-c i") 'haskell-indentation-mode)
(define-key (current-global-map) (kbd "C-c m") 'matlab-mode)
(define-key (current-global-map) (kbd "C-c y") 'python-mode)
(define-key (current-global-map) (kbd "C-c r") 'picture-mode)
(define-key (current-global-map) (kbd "C-c x") 'text-mode)
(define-key (current-global-map) (kbd "C-c o") 'org-mode)

(define-key (current-global-map) (kbd "C-c g") 'magit-status)

;; Global initialisation

(global-magit-wip-save-mode 1)

;; Keybinding to insert a fucking tab, rather than doing crazy indent
(defun command-insert-tab ()
  "Insert a tab character"
  (interactive)
  (insert "\t")
  )
(define-key (current-global-map) (kbd "<C-tab>") 'command-insert-tab)

;; Make backspace delete whitespace in increments of tabstop
;; This is definitely a matter of taste and some improvements need to be made
(defun backward-delete-char-tabstop ()
  (interactive)
  (cond
   ((looking-back " ")
    (let ((i tab-width))
      (while (progn (setq i (- i 1)) (and (>= i 0) (looking-back " ")))
        (delete-backward-char 1))))
   (t  (delete-backward-char 1))))

;; (define-key (current-global-map) (kbd "<backspace>") 'backward-delete-char-tabstop)
;; (define-key isearch-mode-map [backspace] 'isearch-delete-char)

;; Make emacs stop asking silly questions about changed files.
;; Somewhat unsafe, and a matter of taste.
(defun ask-user-about-supersession-threat (fn)
  "blatantly ignore files that changed on disk"
  )
(defun ask-user-about-lock (file opponent)
  "always grab lock"
  t)

(require 'tramp)
;; enable these for tramp debugging
;; (setq tramp-verbose 10)
;; (setq tramp-debug-buffer t)

(mapc 'try-load '(tramp
                  haskell-mode
                  haskell-cabal
                  haskell-doc
                  haskell-c
                  company-mode
                  company
                  markdown-mode))

(mapc (lambda (a) (mode-extension (car a) (cdr a))) '((haskell-mode . ".hs")
                                                      (haskell-mode . ".hs-boot")
                                                      (c-mode . ".cs")
                                                      (octave-mode . ".m")
                                                      (haskell-cabal-mode . ".cabal")
                                                      (markdown-mode . ".md")
                                                      (markdown-mode . ".page")))

(ignore-errors (progn (load-file "~/.emacs.d/irc.el")
				   (define-key (current-global-map) (kbd "C-c f") 'freenode)))
(ignore-errors
  (progn
    (require 'smarttabs)
    (autoload 'smart-tabs-mode "smart-tabs-mode"
      "Intelligently indent with tabs, align with spaces!")
    (autoload 'smart-tabs-mode-enable "smart-tabs-mode")
    (autoload 'smart-tabs-advice "smart-tabs-mode")))

;; This is for kernel work - currently not used
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defun tabstop-hook ()
	(define-key (current-local-map) (kbd "TAB") 'tab-to-tab-stop)
	(setq tab-width 4
		indent-tabs-mode t
		tab-stop-list (number-sequence 4 200 4))
	)
(add-hook 'fundamental-mode-hook 'tabstop-hook)
(add-hook 'text-mode-hook 'tabstop-hook)
(add-hook 'conf-mode-hook 'tabstop-hook)

(defun my-c-mode-hook ()
  (setq c-basic-offset 2
        tab-width 2)
  (smart-tabs-mode-enable)
  (smart-tabs-advice c-indent-line c-basic-offset)
  (smart-tabs-advice c-indent-region c-basic-offset)
  (c-set-offset 'case-label '+)
  (setq indent-tabs-mode t)
  (c-toggle-auto-newline nil)
  (c-toggle-hungry-state t)
  (c-toggle-electric-state t)
  (c-toggle-syntactic-indentation t)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (subword-mode t))

(setq c-mode-hook nil)
(add-hook 'c-mode-hook 'my-c-mode-hook)

(if (try-load 'rust-mode)
    (progn
;      (mode-extension 'rust-mode ".rs")
      (add-hook 'rust-mode-hook
                (lambda ()
                  (if nil
                      (progn
                        (require 'smarttabs)
                        (setq indent-tabs-mode t)
                        (smart-tabs-mode-enable)
                        (smart-tabs-advice rust-indent rust-indent-unit)))))
      (add-hook 'compilation-mode-hook
                (lambda ()
                  (add-to-list 'compilation-error-regexp-alist 'rust)
                  (add-to-list 'compilation-error-regexp-alist-alist
                               '(rust "^\\([.a-zA-Z0-9]+\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3))))))

(defun my-python-mode-hook ()
  (setq python-check-command "pychecker --stdlib -# 0 -xXT")
  (define-key (current-global-map) (kbd "C-.") 'python-shift-right)
  (define-key (current-global-map) (kbd "C-,") 'python-shift-left)
  (setq tab-width 4
        python-indent 4)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (if nil (progn
            (setq indent-tabs-mode t)
            (smart-tabs-mode-enable)
            (smart-tabs-advice python-indent-line python-indent))
    nil))

(setq python-mode-hook nil)
(add-hook 'python-mode-hook 'my-python-mode-hook)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(add-to-list 'company-backends 'company-ghc)

(defun my-haskell-mode-hook ()
	(setq indent-tabs-mode nil
          tab-width 2)
    (turn-on-haskell-indentation)

    (setq ghc-hlint-options '("--ignore=Use camelCase"))
    (ghc-init)
    (define-key haskell-mode-map (kbd "M-s") 'ghc-display-errors)

    (company-mode))

(setq haskell-mode-hook nil)
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(defun my-haskell-cabal-mode-hook ()
	(setq indent-tabs-mode nil))

(add-hook 'haskell-cabal-mode-hook 'my-haskell-cabal-mode-hook)

(defun my-matlab-mode-hook ()
  (auto-fill-mode))
(add-hook 'matlab-mode-hook 'my-matlab-mode-hook)

(defun my-lisp-hook ()
  (setq indent-tabs-mode nil
        lisp-indent-offset 4
        tab-width 4)
  (local-set-key (kbd "RET") 'newline-and-indent)
  )
(setq lisp-mode-hook nil)
(add-hook 'lisp-mode-hook 'my-lisp-hook)

(add-hook 'sql-interactive-mode-hook
	(function (lambda () (setq tab-width 4))))

(setq gdb-non-stop-setting nil)

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
  (message "Now select other region to compare and run `diff-region-now`")
  )

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
      (ediff-buffers bufa bufb))
    )
  )

; run perl on the current region, updating the region
(defun perl-replace-region (start end)
  "Apply perl command to region"
  (interactive "r")
  (shell-command-on-region start end
                           (read-from-minibuffer "Replace region command: " '("perl -pel \'s///g\'" . 14 ))
                           t t )
  (exchange-point-and-mark))

; run perl on the current buffer, updating the buffer
(defun perl-replace-buffer ()
  "Apply perl command to buffer"
  (interactive)
  (let ((ptline (count-lines (point-min) (point)))
        (ptcol (current-column))
        (markline 0)
        (markcol  0)
        (command (read-from-minibuffer "Replace buffer command: " '("perl -pel \'s///g\'" . 14 ))))
    (exchange-point-and-mark)
    (setq markline (count-lines (point-min) (point)))
    (setq markcol  (current-column))
    (mark-whole-buffer)
    (let ((new-start (region-beginning))
          (new-end   (region-end)))
      (shell-command-on-region  new-start new-end command t t))
    (goto-line markline)
    (move-to-column markcol)
    (exchange-point-and-mark)
    (goto-line ptline)
    (move-to-column ptcol)
    )
  )

(define-key (current-global-map) (kbd "C-M-^") 'perl-replace-region)
(define-key (current-global-map) (kbd "C-M-&") 'perl-replace-buffer)
