(require 'utils)
;; Alternative: "DejaVu Sans Mono:style=Book:size=12"
;; (add-to-list 'default-frame-alist '(font . "Terminus:style=Regular:size=12"))
(add-to-list 'default-frame-alist '(font . "-*-terminus-medium-*-*-*-12-*-*-*-*-*-*-*"))

(setq
 ;; gc tuning
 gc-cons-threshold 20000000
 inhibit-splash-screen t
 vc-follow-symlinks t
 pop-up-windows nil
 split-height-threshold nil
 ;; Make windows split vertically like C-x 3 for things like help, grep, compile, gdb etc.
 split-height-threshold nil
 split-width-threshold 0
 large-file-warning-threshold 1000000000
 print-length nil
 print-level nil
 eval-expression-print-length nil
 eval-expression-print-level nil)

(setq linum-format "%3d\u2502")

(setq display-buffer-base-action
      '((display-buffer--maybe-same-window
         display-buffer-reuse-window
         display-buffer-in-previous-window
         display-buffer-use-some-window)))

(try-fn tool-bar-mode -1)
(try-fn menu-bar-mode -1)
(try-fn fringe-mode 0)
(try-fn scroll-bar-mode -1)
(try-fn column-number-mode)

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
 mouse-hilight 1
 ;; I rarely don't want to see trailing whitespace. Also, it's not very intrusive in windowed mode.
 show-trailing-whitespace t)

;; Add indicator in mode line for whether a window is dedicated
(setf (nthcdr 6 mode-line-format) (cons
                                   '(:eval (if (window-dedicated-p (get-buffer-window))
                                               "D" "-"))
                                   (nthcdr 6 mode-line-format)))


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; System-specific

(cond ((eq system-type 'windows-nt)
       (setq w32-lwindow-modifier 'super))
      )

;; Answer y or n instead of yes or no at minibar prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

(defun ask-user-about-supersession-threat (fn)
  "blatantly ignore files that changed on disk")
(defun ask-user-about-lock (file opponent)
  "always grab lock" t)

(setq source-directory "~/src/emacs")

(setq
 backup-directory-alist `((".*" . ,(user-subdir "backups")))
 auto-save-file-name-transforms `((".*" ,(user-subdir "auto-saves") t))
 create-lockfiles nil)

(setq custom-file (user-subdir "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq custom-theme-load-path '("~/dev/subatomic256"))
(setq custom-theme-directory (user-subdir "themes"))

(provide 'config)
