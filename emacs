(defun loade ()
  (interactive)
  (load-file "~/.emacs"))

(column-number-mode)
(add-to-list 'default-frame-alist '(font . "Terminus:style=Regular:size=10"))
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

;;/ssh:root@localhost#9004:/var/lib/postgresql/
;;(set-default-font "DejaVu Sans Mono:style=Book:size=12")

(set-default-font "Terminus:style=Regular:size=10")
(tool-bar-mode -1)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq inhibit-splash-screen t)
(put 'upcase-region 'disabled nil)
(setq resize-mini-windows t)
;;(setq tramp-syntax 'url)
(require 'tramp)
(setq tramp-verbose 10)
(setq tramp-debug-buffer t)
;;(setq tramp-default-method "ssh")

(toggle-truncate-lines nil)
;;(setq tab-always-indent nil)
;;(setq indent-tabs-mode nil)
;;(setq tab-width 4)

(setq split-height-threshold nil)
(setq split-width-threshold 0)

(defun close-and-kill-next-pane ()
  "Close the other pane and kill the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-buffer)
  (delete-window)
  )

(defun close-and-kill-this-pane ()
  "Close this pane and kill the buffer in it also."
  (interactive)
  (kill-buffer)
  (delete-window)
  )

(define-key (current-global-map) (kbd "s-v") 'clipboard-yank)
(define-key (current-global-map) (kbd "s-c") 'clipboard-kill-ring-save)
(define-key (current-global-map) (kbd "C-c v") 'clipboard-yank)
(define-key (current-global-map) (kbd "C-c c") 'clipboard-kill-ring-save)
(define-key (current-global-map) (kbd "C-c t") 'toggle-truncate-lines)
(define-key (current-global-map) (kbd "C-c l") 'linum-mode)
(define-key (current-global-map) (kbd "C-c p") 'c++-mode)
(define-key (current-global-map) (kbd "C-c h") 'haskell-mode)
(define-key (current-global-map) (kbd "C-c i") 'haskell-indentation-mode)
(define-key (current-global-map) (kbd "C-c m") 'matlab-mode)
(define-key (current-global-map) (kbd "C-c y") 'python-mode)
(define-key (current-global-map) (kbd "C-c 1") 'compile)
(define-key (current-global-map) (kbd "C-c w") 'whitespace-mode)
(define-key (current-global-map) (kbd "C-c r") 'picture-mode)
(define-key (current-global-map) (kbd "C-c x") 'text-mode)
(define-key (current-global-map) (kbd "C-c a") 'goto-line)
(define-key (current-global-map) (kbd "C-z") 'nil)
(define-key (current-global-map) (kbd "C-z x") 'close-and-kill-next-pane)
(define-key (current-global-map) (kbd "C-z z") 'close-and-kill-this-pane)
(define-key (current-global-map) (kbd "C-c o") 'org-mode)
(define-key (current-global-map) (kbd "C-?") 'undo-only)
(define-key (current-global-map) (kbd "M-%") 'replace-regexp)
;;(define-key (current-global-map) (kbd "C-M-x") '(switch-to-buffer "*scratch*"))


(defun command-insert-tab ()
  "Insert a tab character"
  (interactive)
  (insert "\t")
  )

(define-key (current-global-map) (kbd "<C-tab>") 'command-insert-tab)

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

(add-to-list 'load-path "/usr/share/emacs/site-lisp/color-theme.el")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/haskell-mode/haskell-mode.el")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/cython-mode.el")

(when (not (window-system))
  (menu-bar-mode -1))

(defun ask-user-about-supersession-threat (fn)
  "blatantly ignore files that changed on disk"
  )
(defun ask-user-about-lock (file opponent)
  "always grab lock"
  t)


(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (if (window-system)
         (color-theme-charcoal-black)
       (color-theme-clarity))
     ;; normal: taylor,
     ;; nw: taming-mr-arneson, clarity, renegade, midnight, dark-laptop, jsc-ligh2, ld-dark, clarity, renegade
     ;; promising: montz, tty-dark, gray30, lethe
     ;;(if (display-graphic-p) (color-theme-charcoal-black) (color-theme-lawrence))
     ))

(require 'haskell-mode)

(require 'cython-mode)

(require 'smarttabs)
(autoload 'smart-tabs-mode "smart-tabs-mode"
  "Intelligently indent with tabs, align with spaces!")
(autoload 'smart-tabs-mode-enable "smart-tabs-mode")
(autoload 'smart-tabs-advice "smart-tabs-mode")


(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defun my-c-mode-hook ()
  (setq indent-tabs-mode t
        c-basic-offset 4
        tab-width 4)
  (smart-tabs-mode-enable)
  (smart-tabs-advice c-indent-line c-basic-offset)
  (smart-tabs-advice c-indent-region c-basic-offset))

(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-matlab-mode-hook ()
  (auto-fill-mode)
  )
(add-hook 'matlab-mode-hook 'my-matlab-mode-hook)


(defun my-python-hook ()
  (setq python-check-command "pychecker --stdlib -# 0 -xXT")
  (define-key (current-global-map) (kbd "C-.") 'python-shift-right)
  (define-key (current-global-map) (kbd "C-,") 'python-shift-left)

  (if t (function (lambda ()
                    (setq indent-tabs-mode t
                          python-indent 4
                          tab-width 4)))
        (function (lambda ()
                    (setq indent-tabs-mode nil
                          tab-width 2
                          python-indent 2)))))
(add-hook 'python-mode-hook 'my-python-hook)

(defun my-lisp-hook ()
  (setq indent-tabs-mode nil
        tab-width 4))
(add-hook 'lisp-mode-hook 'my-lisp-hook)


(add-hook 'sql-interactive-mode-hook
          (function (lambda () ((setq tab-width 4)))))

