(defun loade ()
	"Reload .emacs. This is defined at the beginning so that the file can be easily reloaded even if errors have occured."
	(interactive)
	(load-file "~/.emacs"))

;; Alternative: "DejaVu Sans Mono:style=Book:size=12"
(add-to-list 'default-frame-alist '(font . "Terminus:style=Regular:size=10"))

(add-to-list 'load-path '("/usr/share/emacs/site-lisp" "~/.emacs.d"))

;;(require 'color-theme)
;; I've looked through all the themes in color-theme. Most of these are bad. Believe me, the rest are worse.
;; nw: taming-mr-arneson, clarity, renegade, midnight, dark-laptop, jsc-ligh2, ld-dark, clarity, renegade
;; promising: montz, tty-dark, gray30, lethe
(require 'color-theme-solarized)
(eval-after-load "color-theme"
	'(progn
		 (color-theme-initialize)
		 (if (window-system)
			 (color-theme-solarized-dark)
			 (color-theme-clarity))))

;;(require 'color-theme-solarized)
;;(color-theme-solarized-dark)

;; Show column and line numbers on status bar
(column-number-mode)

;; Disable icons on the menu
(tool-bar-mode -1)

;; Remove scroll bars
(toggle-scroll-bar nil)

;; Use unified diff
(setq diff-switches "-u")

;; When in terminal, disable the menu bar entirely
;;(when (not (window-system))
;;	(menu-bar-mode -1))
(menu-bar-mode -1)

;; Stop dired from spamming windows as you navigate
(put 'dired-find-alternate-file 'disabled nil)

;; Get rid of the annoying spash screen
(setq inhibit-splash-screen t)

;; Follow symlinks to version-controlled files
(setq vc-follow-symlinks t)

;; By default these are disabled
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Don't wrap lines by default
(set-default 'truncate-lines t)

;; I'm not sure what this does. I must have had a reason for it, though
(setq resize-mini-windows t)

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

(define-key (current-global-map) (kbd "C-z x") 'close-and-kill-next-pane)
(define-key (current-global-map) (kbd "C-z z") 'close-and-kill-this-pane)

;; Keybindings to the X clipboard
(define-key (current-global-map) (kbd "s-v") 'clipboard-yank)
(define-key (current-global-map) (kbd "s-c") 'clipboard-kill-ring-save)
(define-key (current-global-map) (kbd "C-c v") 'clipboard-yank)
(define-key (current-global-map) (kbd "C-c c") 'clipboard-kill-ring-save)

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


;; Keybinding to insert a fucking tab, rather than doing crazy indent
(defun command-insert-tab ()
	"Insert a tab character"
	(interactive)
	(insert "\t")
	)
(define-key (current-global-map) (kbd "<C-tab>") 'command-insert-tab)


;; Make backspace delete whitespace in increments of tabstop.
;; This is definitely a matter of taste and some improvements need to be made to make it fully pimped
(defun backward-delete-char-tabstop ()
	(interactive)
	(cond
		((looking-back " ")
			(let ((i tab-width))
				(while (progn (setq i (- i 1)) (and (>= i 0) (looking-back " ")))
					(delete-backward-char 1))))
		(t  (delete-backward-char 1))))

(define-key (current-global-map) (kbd "<backspace>") 'backward-delete-char-tabstop)
(define-key isearch-mode-map [backspace] 'isearch-delete-char)

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


(require 'haskell-mode)

(require 'cython-mode)

(require 'smarttabs)
(autoload 'smart-tabs-mode "smart-tabs-mode"
	"Intelligently indent with tabs, align with spaces!")
(autoload 'smart-tabs-mode-enable "smart-tabs-mode")
(autoload 'smart-tabs-advice "smart-tabs-mode")

;; Yeah, this doesn't work
;; (setq gnus-select-method '(nnml ""))
;; (setq mail-sources
;;       '((pop :server "pop.gmail.com"
;;              :port 995
;;              :user "duncan.burke@orionvm.com.au"
;;              :password ""
;;              :stream ssl)))

;; This is for kernel work - currently not used
(defun c-lineup-arglist-tabs-only (ignored)
	"Line up argument lists by tabs, not spaces"
	(let* ((anchor (c-langelem-pos c-syntactic-element))
		      (column (c-langelem-2nd-pos c-syntactic-element))
		      (offset (- (1+ column) anchor))
		      (steps (floor offset c-basic-offset)))
		(* (max steps 1)
			c-basic-offset)))

(defun my-c-mode-hook ()
	(setq c-basic-offset 4
		tab-width 4)
	(smart-tabs-mode-enable)
	(smart-tabs-advice c-indent-line c-basic-offset)
	(smart-tabs-advice c-indent-region c-basic-offset)
	(setq indent-tabs-mode 4)
	(c-toggle-auto-newline nil)
	(c-toggle-hungry-state t)
	(c-toggle-electric-state t)
	(c-toggle-syntactic-indentation t)
	(local-set-key (kbd "RET") 'newline-and-indent)
	(subword-mode t))

(setq c-mode-hook nil)
(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-python-mode-hook ()
	(setq python-check-command "pychecker --stdlib -# 0 -xXT")
	(define-key (current-global-map) (kbd "C-.") 'python-shift-right)
	(define-key (current-global-map) (kbd "C-,") 'python-shift-left)

	(setq tab-width 4
		python-indent 4
		indent-tabs-mode t)

	(local-set-key (kbd "RET") 'newline-and-indent)
	(smart-tabs-mode-enable)
	(smart-tabs-advice python-indent-line python-indent))

(setq python-mode-hook nil)
(add-hook 'python-mode-hook 'my-python-mode-hook)

(defun my-matlab-mode-hook ()
	(auto-fill-mode))
(add-hook 'matlab-mode-hook 'my-matlab-mode-hook)

(defun my-lisp-hook ()
	(setq indent-tabs-mode t
		lisp-indent-offset 4
		tab-width 4)
	(local-set-key (kbd "RET") 'newline-and-indent)
	(smart-tabs-mode-enable)
	(smart-tabs-advice lisp-indent-line lisp-indent-offset))

(setq lisp-mode-hook nil)
(add-hook 'lisp-mode-hook 'my-lisp-hook)


(add-hook 'sql-interactive-mode-hook
	(function (lambda () ((setq tab-width 4)))))
