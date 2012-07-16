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
(setq tab-always-indent nil)
(setq indent-tabs-mode nil)
(setq tab-width 4)

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

;(defadvice indent-to (before indent-to-spaces)
;  "force indent-to to only insert spaces"
;  (ad-set-arg 1 (- (point) (ad-get-arg 0))))
;(ad-activate 'indent-to)

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





;; (setq gnus-select-method '(nnml ""))

;; (setq mail-sources
;;       '((pop :server "pop.gmail.com"
;;              :port 995
;;              :user "duncan.burke@orionvm.com.au"
;;              :password ""
;;              :stream ssl)))


(eval-when-compile (require 'cc-defs))

;; Wrapper function needed for Emacs 21 and XEmacs (Emacs 22 offers the more
;; elegant solution of composing a list of lineup functions or quantities with
;; operators such as "add")
(defun google-c-lineup-expression-plus-4 (langelem)
  "Indents to the beginning of the current C expression plus 4 spaces.

This implements title \"Function Declarations and Definitions\" of the Google
C++ Style Guide for the case where the previous line ends with an open
parenthese.

\"Current C expression\", as per the Google Style Guide and as clarified by
subsequent discussions,
means the whole expression regardless of the number of nested parentheses, but
excluding non-expression material such as \"if(\" and \"for(\" control
structures.

Suitable for inclusion in `c-offsets-alist'."
  (save-excursion
    (back-to-indentation)
    ;; Go to beginning of *previous* line:
    (c-backward-syntactic-ws)
    (back-to-indentation)
    ;; We are making a reasonable assumption that if there is a control
    ;; structure to indent past, it has to be at the beginning of the line.
    (if (looking-at "\\(\\(if\\|for\\|while\\)\()\s *(\\)")
        (goto-char (match-end 1)))
    (vector (+ 4 (current-column)))))

(defconst google-c-style
  `((c-recognize-knr-p . nil)
    (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
    (c-basic-offset . 2)
    (indent-tabs-mode . nil)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((defun-open after)
                               (defun-close before after)
                               (class-open after)
                               (class-close before after)
                               (namespace-open after)
                               (inline-open after)
                               (inline-close before after)
                               (block-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (extern-lang-close after)
                               (statement-case-open after)
                               (substatement-open after)))
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p . nil)
    (comment-column . 40)
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-offsets-alist . ((arglist-intro google-c-lineup-expression-plus-4)
                        (func-decl-cont . ++)
                        (member-init-intro . ++)
                        (inher-intro . ++)
                        (comment-intro . 0)
                        (arglist-close . c-lineup-arglist)
                        (topmost-intro . 0)
                        (block-open . 0)
                        (inline-open . 0)
                        (substatement-open . 0)
                        (statement-cont
                         .
                         (,(when (fboundp 'c-no-indent-after-java-annotations)
                             'c-no-indent-after-java-annotations)
                          ,(when (fboundp 'c-lineup-assignments)
                             'c-lineup-assignments)
                          ++))
                        (label . /)
                        (case-label . +)
                        (statement-case-open . +)
                        (statement-case-intro . +) ; case w/o {
                        (access-label . /)
                        (innamespace . 0))))
  "Google C/C++ Programming Style")

(defun google-set-c-style ()
  "Set the current buffer's c-style to Google C/C++ Programming
  Style. Meant to be added to `c-mode-common-hook'."
  (interactive)
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent t)
  (c-add-style "Google" google-c-style t))

(defun google-make-newline-indent ()
  "Sets up preferred newline behavior. Not set by default. Meant
  to be added to `c-mode-common-hook'."
  (interactive)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map [ret] 'newline-and-indent))

(provide 'google-c-style)

(defun my-c-mode-common-hook ()
  (google-set-c-style)
  (google-make-newline-indent)
					;(setq tab-width 4 indent-tabs-mode 1)
  (c-toggle-auto-newline 1)
  (c-toggle-electric-state 1)
  (c-toggle-hungry-state 1)
  (toggle-truncate-lines nil)
  (linum-mode t)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-matlab-mode-hook ()
  (auto-fill-mode)
  )
(add-hook 'matlab-mode-hook 'my-matlab-mode-hook)


;; Python Hook
(add-hook 'python-mode-hook
          (if t
	      (function (lambda ()
			  (setq indent-tabs-mode t
				python-indent 4
				tab-width 4)))
	    (function (lambda ()
			(setq indent-tabs-mode nil
			      tab-width 2
			      python-indent 2)))))



(add-hook 'sql-interactive-mode-hook 
	  (function (lambda () ((setq tab-width 4)))))