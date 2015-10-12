(require 'el-helper)
(require 'utils)

(let ((minibuffer-bindings
       '(("C-n" . next-history-element)
         ("C-t" . previous-history-element)
         ("M-n" . next-matching-history-element)
         ("C-t" . previous-matching-history-element))))
  (my-keys-remap-mode 'minibuffer-local-map
                      minibuffer-bindings)
  (my-keys-remap-mode 'minibuffer-local-ns-map
                      minibuffer-bindings)
  (my-keys-remap-mode 'minibuffer-local-completion-map
                      minibuffer-bindings))

(my-keys-remap-mode 'multi-query-replace-map)
(my-keys-remap-mode 'query-replace-map)

;; Inbuilt Modes
;; -------------

;; bs
(with-eval-after-load "bs"
  (my-keys-remap-mode 'bs-mode-map
                      '(("M-s" . scroll-right)
                        ("M-h" . scroll-left)
                        ("C-t" . bs-up)
                        ("C-n" . bs-down))))

;; cc-mode
(with-eval-after-load "cc-mode"
  (my-keys-remap-mode 'c-mode-base-map)
  (my-keys-remap-mode 'c-mode-map)
  (my-keys-remap-mode 'c++-mode-map)
  (my-keys-remap-mode 'objc-mode-map)
  (my-keys-remap-mode 'java-mode-map)
  (my-keys-remap-mode 'idl-mode-map)
  (my-keys-remap-mode 'pike-mode-map)
  (my-keys-remap-mode 'awk-mode-map))

(add-hook-anon 'c-mode-hook
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
               (subword-mode t))

;; comint
(with-eval-after-load "comint"
  (my-keys-remap-mode
   'comint-mode-map
   '(("M-a" . comint-history-isearch-backward-regexp))))

;; conf-mode
;; conf-unix-mode, conf-windows-mode, conf-space-mode, conf-colon-mode
(with-eval-after-load "conf-mode"
  (my-keys-remap-mode 'conf-mode-map))

(add-hook 'conf-mode-hook #'tabstop-hook)

;; dired
(with-eval-after-load "dired"
  (my-keys-remap-mode 'dired-mode-map)
  ;; Allow dired to recursive delete without confirmation
  (setq dired-recursive-deletes 'always)
  ;; Stop dired from spamming windows as you navigate
  (put 'dired-find-alternate-file 'disabled nil))

;; wdired
(with-eval-after-load "wdired"
  (my-keys-remap-mode 'wdired-mode-map))

;; edmacro-mode
(with-eval-after-load "edmacro"
  (my-keys-remap-mode 'edmacro-mode-map))

;; eshell
;; (with-eval-after-load "esh-mode"
;;   (my-keys-remap-mode 'eshell-mode-map))

;; flyspell-mode
(with-eval-after-load "flyspell"
  (my-keys-remap-mode 'flyspell-mode-map))

;; fundamental-mode

;; help-mode

(with-eval-after-load "help-mode"
  (my-keys-remap-mode 'help-mode-map))

;; ido
(defvar saved-ido-common-completion-map nil)
(defvar saved-ido-file-dir-completion-map nil)
(defvar saved-ido-file-completion-map nil)
(defvar saved-ido-buffer-completion-map nil)
(defvar saved-ido-completion-map nil)

(add-hook-anon 'ido-setup-hook
               (if saved-ido-common-completion-map
                   (progn
                     (setq ido-common-completion-map saved-ido-common-completion-map
                           ido-file-dir-completion-map saved-ido-file-dir-completion-map
                           ido-file-completion-map saved-ido-file-completion-map
                           ido-completion-map saved-ido-completion-map))
                 (progn
                   (my-keys-remap-mode 'ido-common-completion-map
                                       '(("C-h" . nil)
                                         ("C-s" . nil)
                                         ("C-l" . nil)
                                         ("C-," . ido-exit-minibuffer)
                                         ("C-." . nil)
                                         ("C-g" . nil)
                                         ("M-g" . nil)
                                         ("C-t" . ido-prev-match)
                                         ("C-n" . ido-next-match)))
                   (my-keys-remap-mode 'ido-file-dir-completion-map
                                       '(("C-g" . ido-delete-backward-updir)
                                         ("M-c" . ido-delete-backward-word-updir)
                                         ("C-t" . ido-prev-match-dir)
                                         ("C-n" . ido-next-match-dir)
                                         ("C-f" . ido-fallback-command)
                                         ("C-b" . ido-enter-dired)))
                   (my-keys-remap-mode 'ido-file-completion-map)
                   (my-keys-remap-mode 'ido-buffer-completion-map)
                   (my-keys-remap-mode 'ido-completion-map)
                   (setq saved-ido-common-completion-map ido-common-completion-map
                         saved-ido-file-dir-completion-map ido-file-dir-completion-map
                         saved-ido-file-completion-map ido-file-completion-map
                         saved-ido-buffer-completion-map ido-buffer-completion-map
                         saved-ido-completion-map ido-completion-map)))
               (setq ido-enable-flex-matching t
                     ido-use-filename-at-point nil
                     ido-create-new-buffer 'always
                     ido-default-file-method 'selected-window
                     ido-default-buffer-method 'selected-window
                     ido-enable-flex-matching t
                     ido-use-faces nil
                     ido-auto-merge-work-directories-length -1))

(ido-mode 1)
(ido-everywhere 1)

;; ielm
(with-eval-after-load "ielm"
  (my-keys-remap-mode 'ielm-map))

;; isearch-mode
(with-eval-after-load "isearch"
  (my-keys-remap-mode
   'isearch-mode-map
   '(("C-a" . isearch-repeat-backward)
     ("C-o" . isearch-repeat-forward)
     ("M-a" . isearch-repeat-backward)
     ("M-o" . isearch-repeat-forward)
     ("C-," . isearch-abort)
     ("C-e" . isearch-quote-char)
     ("M-p" . isearch-ring-retreat)
     ("M-t" . isearch-ring-advance)
     ("C-g" . isearch-delete-char)))
  (my-keys-remap-mode
   'minibuffer-local-isearch-map
   '(("TAB" . isearch-complete-edit)
     ("C-a" . isearch-reverse-exit-minibuffer)
     ("C-o" . isearch-forward-exit-minibuffer)
     ("M-a" . isearch-reverse-exit-minibuffer)
     ("M-o" . isearch-forward-exit-minibuffer))))


;; lisp-mode
(with-eval-after-load "lisp-mode"
  (my-keys-remap-mode 'lisp-mode-shared-map)
  (my-keys-remap-mode 'lisp-mode-map)
  (my-keys-remap-mode 'emacs-lisp-mode-map)
  (my-keys-remap-mode 'lisp-interaction-mode-map))

(add-hook 'emacs-lisp-mode-hook #'rainbow-blocks-mode)
(add-hook 'lisp-mode-hook #'rainbow-blocks-mode)

;; package
(with-eval-after-load "package"
  (my-keys-remap-mode 'package-menu-mode-map))

;; prog-mode
(with-eval-after-load "prog-mode"
  (my-keys-remap-mode 'prog-mode-map))

(add-hook 'prog-mode-hook #'linum-hook)

;; python-mode
(with-eval-after-load "python"
  (my-keys-remap-mode 'python-mode-map
                      '("RET" . 'newline-and-indent))
  (my-keys-remap-mode 'inferior-python-mode-map))

(add-hook-anon 'python-mode-hook
               (setq python-check-command "pychecker --stdlib -# 0 -xXT"
                     tab-width 4
                     python-indent 4))

;; tabulated-list
(with-eval-after-load "tabulated-list"
  (define-key tabulated-list-mode-map "p" nil)
  (define-key tabulated-list-mode-map "t" 'previous-line))

;; term
(with-eval-after-load "term"
  (my-keys-remap-mode 'term-mode-map))

;; text-mode
(with-eval-after-load "text-mode"
  (my-keys-remap-mode 'text-mode-map))

(add-hook 'text-mode-hook #'tabstop-hook)
(add-hook 'text-mode-hook #'linum-hook)

;; Packages
;; --------

;; company
;; hooks: company-completion-(started|cancelled|finished)-hook
(el-register-package
 :name company
 :type elpa
 :after
 (progn
   (require 'company)
   (my-keys-remap-mode 'company-mode-map)
   (my-keys-remap-mode 'company-active-map)
   (add-to-list 'company-backends 'company-ghc)))

;; company-ghc
(el-use-package "company-ghc")

;; flx
(el-register-package
 :name flx
 :type elpa)

;; flx-ido
(el-register-package
 :name flx-ido
 :type elpa
 :after
 (flx-ido-mode))

;; ghc
;; (el-use-package "ac-ghc-mod")
(el-register-package
 :name ghc-mod
 :type github
 :pkgname "kazu-yamamoto/ghc-mod"
 :load-path "elisp")

(setq ghc-interactive-command "ghc-modi")
;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)

;; git modes
;; commit, rebase, config, ignore
(el-use-package "git-modes")

;; haskell-mode
(defvar haskell-mode-remapped nil)
(el-register-package
 :name haskell-mode
 :type github
 :pkgname "haskell/haskell-mode"
 :info "."
 :build `(("make" ,(format "EMACS=%s" el-get-emacs) "all"))
 :post-init (require 'haskell-mode-autoloads))

(with-eval-after-load "haskell-cabal"
  (my-keys-remap-mode 'haskell-cabal-mode-map))

(with-eval-after-load "haskell-interactive-mode"
  (my-keys-remap-mode 'haskell-interactive-mode-map))

(with-eval-after-load "haskell-mode"
  (my-keys-remap-mode 'haskell-mode-map))

(add-hook-anon
 'haskell-mode-hook
 (setq indent-tabs-mode nil
       tab-width 2
       haskell-indentation-cycle-warn nil
       ghc-hlint-options '("--ignore=Use camelCase")
       show-trailing-whitespace t
       ghc-display-error 'minibuffer
       ghc-display-hole 'other-buffer)
 ;; (ghc-init)
 ;; (unless haskell-mode-remapped
 ;;   ;; ghc-mode edits `haskell-mode-map`, so we need to defer the
 ;;   ;; remapping until ghc has been loaded for the first time
 ;;   (my-keys-remap-mode 'haskell-mode-map)
 ;;   (setq haskell-mode-remapped t))
 (flyspell-prog-mode)
 (company-mode)
 (auto-fill-mode 1)
 (turn-on-haskell-indentation)
 (set (make-local-variable
       'fill-nobreak-predicate)
      (lambda ()
        (not (eq (get-text-property (point) 'face)
                 'font-lock-comment-face)))))

(mode-extension #'haskell-mode ".hs")
(mode-extension #'haskell-mode ".hs-boot")
(mode-extension #'haskell-cabal-mode ".cabal")

;; help-fns+
(el-use-package "help-fns+")

;; magit
(el-register-package
 :name magit
 :type elpa
 :after
 (progn
   (require 'magit)
   (setq magit-last-seen-setup-instructions "1.4.0")
   (my-keys-remap-mode 'git-commit-mode-map)
   (my-keys-remap-mode 'git-rebase-mode-map)
   (my-keys-remap-mode 'magit-mode-map)
   (my-keys-remap-mode 'magit-commit-mode-map)
   (my-keys-remap-mode 'magit-status-mode-map)
   (my-keys-remap-mode 'magit-log-mode-map)
   (my-keys-remap-mode 'magit-cherry-mode-map)
   (my-keys-remap-mode 'magit-reflog-mode-map)
   (my-keys-remap-mode 'magit-diff-mode-map)
   (my-keys-remap-mode 'magit-wazzup-mode-map)
   (my-keys-remap-mode 'magit-branch-manager-mode-map)
   (my-keys-remap-mode 'magit-process-mode-map)
   (my-keys-remap-mode 'magit-section-jump-map)))

;; monkey-patch magit to show patch on commit buffer
(advice-add #'magit-key-mode-popup-committing :after
            (lambda ()
              (magit-key-mode-toggle-option (quote committing) "--verbose")))

;; markdown-mode
(el-register-package
 :name markdown-mode
 :type elpa
 :after
 (progn
   (require 'markdown-mode)
   (my-keys-remap-mode
    'markdown-mode-map
    '(("M-h" . nil)
      ("M-t" . nil)
      ("M-n" . nil)
      ("M-s" . nil)))))

(add-hook-anon
 'markdown-mode-hook
 (flyspell-mode)
 (visual-line-mode)
 (wc-mode))

(push '("---\\(.\\|\n\\)*format:\\s-*markdown" . markdown-mode) magic-mode-alist)
(mode-extension #'markdown-mode ".md")
(mode-extension #'markdown-mode ".mdown")
(mode-extension #'markdown-mode ".markdown")

;; org
(el-register-package
 :name org
 :type elpa
 :after
 (progn
   (require 'org)
   (my-keys-remap-mode 'org-mode-map)))

(add-hook-anon
 'org-mode-hook
 (setq org-hide-leading-stars t
       org-startup-indented nil
       org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE"))))

(push '("---\\(.\\|\n\\)*format:\\s-*org" . org-mode) magic-mode-alist)

;; rainbow-blocks-mode
(el-register-package
 :name rainbow-blocks
 :type elpa)

;; smart-tabs-mode
(el-use-package "smarttabs")

;; subatomic256
(el-register-package
 :name subatomic256-theme
 :type github
 :pkgname "duncanburke/subatomic256"
 :after
 (progn
   (load-file "subatomic256-theme.el")
   (load-theme 'subatomic256 t)))

;; visual-fill-column
(el-register-package
 :name visual-fill-column
 :type github
 :pkgname "duncanburke/visual-fill-column")

;; wc-mode
(el-use-package "wc-mode")

;; yaml-mode
(el-use-package "yaml-mode")
(mode-extension #'yaml-mode ".yaml")

;; Custom Modes

(define-derived-mode writing-mode
  markdown-mode "Writing"
  "Major mode for writing large bodies of text."
  (interactive)
  (visual-line-mode)
  (setq visual-fill-column-width 95)
  (visual-fill-column-mode)
  (flyspell-mode)
  (wc-mode))

(provide 'my-modes)
