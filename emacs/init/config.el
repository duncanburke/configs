;; Alternative: "DejaVu Sans Mono:style=Book:size=12"
(add-to-list 'default-frame-alist '(font . "Terminus:style=Regular:size=10"))

(setq
 ;; gc tuning
 gc-cons-threshold 20000000
 inhibit-splash-screen t
 vc-follow-symlinks nil
 pop-up-windows nil
 split-height-threshold nil
 ;; Make windows split vertically like C-x 3 for things like help, grep, compile, gdb etc.
 split-height-threshold nil
 split-width-threshold 0)

(tool-bar-mode -1)
(menu-bar-mode -1)
(fringe-mode 0)
(scroll-bar-mode -1)
(global-linum-mode)
(column-number-mode)

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

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Answer y or n instead of yes or no at minibar prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

(defun ask-user-about-supersession-threat (fn)
  "blatantly ignore files that changed on disk")
(defun ask-user-about-lock (file opponent)
  "always grab lock" t)

(provide 'config)