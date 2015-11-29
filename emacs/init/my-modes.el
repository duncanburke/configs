(require 'el-helper)
(require 'utils)

;; Inbuilt Modes
;; -------------

;; bs
(with-eval-after-load "bs"
  (keymap-define-kbd
   bs-mode-map
   ("C-c")
   ("C-k C-k" 'bs-kill)
   ("C-d")
   ("C-g" 'bs-delete-backward)
   ("C-g")
   ("M-," 'bs-abort)
   ("C-o")
   ("C-w" 'bs-tmp-select-other-window)
   ("C-t" 'bs-up)
   ("C-n" 'bs-down)))

;; cc-mode
(with-eval-after-load "cc-mode"
  (keymap-define-kbd
   c-mode-base-map
   ("C-d")
   ("C-l" 'c-electric-delete-forward)
   ("C-k" (lookup-key c-mode-base-map [?\C-c]))
   ("C-c")
   ("C-M-a")
   ("C-M-e")
   ("C-M-h")
   ("C-M-q"))
  (keymap-define-kbd
   c-mode-map
   ("C-k C-e" 'c-macro-expand)
   ("RET" 'newline-and-indent
   ("C-c"))
  (keymap-define-kbd
   c++-mode-map
   ("C-k :" 'c-scope-operator)
   ("C-c"))
  (keymap-define-kbd
   awk-mode-map
   ("C-M-a")
   ("C-M-e")
   ("M-a")
   ("M-e")))

  (add-hook-anon
   'c-mode-hook
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
   (subword-mode t))
  )


;; comint
(with-eval-after-load "comint"
  (keymap-define-kbd
   comint-mode-map
   ("C-d")
   ("C-l" 'comint-delchar-or-maybe-eof)
   ("<C-down>")
   ("<C-up>")
   ("M-n")
   ("M-p")
   ("M-t" 'comint-previous-input)
   ("M-n" 'comint-next-input)
   ("C-k" (lookup-key comint-mode-map [?\C-c]))
   ("C-c")
   ("M-r")
   ("M-a" 'comint-history-isearch-backward-regexp)
   ("C-M-l")))

;; conf-mode
;;; conf-unix-mode, conf-windows-mode, conf-space-mode, conf-colon-mode
;;(add-hook 'conf-mode-hook #'tabstop-hook)
(with-eval-after-load "conf-mode"
  (keymap-define-kbd
   conf-mode-map
   ("C-k" (lookup-key conf-mode-map [?\C-c]))
   ("C-c")))

;; dired
(with-eval-after-load "dired"
  (keymap-define-kbd
   dired-mode-map
   ("C-o")
   ("C-t")
   ("C-M-d")
   ("C-M-n")
   ("C-M-p")
   ("C-M-u")
   ("M-s"))
  ;; Allow dired to recursive delete without confirmation
  (setq dired-recursive-deletes 'always)
  ;; Stop dired from spamming windows as you navigate
  (put 'dired-find-alternate-file 'disabled nil))

;; wdired
(with-eval-after-load "wdired"
  (keymap-define-kbd
   wdired-mode-map
   ("C-p")
   ("C-t" 'wdired-previous-line)
   ("C-k" (lookup-key wdired-mode-map [?\C-c]))
   ("C-c")))

;; edmacro-mode
(with-eval-after-load "edmacro"
  (keymap-define-kbd
   edmacro-mode-map
   ("C-k" (lookup-key edmacro-mode-map [?\C-c]))
   ("C-c")))

;; eshell

;; flyspell-mode
(with-eval-after-load "flyspell"
  (keymap-define-kbd
   flyspell-mode-map
   ("C-c")
   ("C-k $" 'flyspell-correct-word-before-point)
   ("C-,")
   ("C-.")
   ("C-;")))

;; fundamental-mode

;; help-mode
(with-eval-after-load "help-mode"
  (keymap-define-kbd
   help-mode-map
   ("C-c")
   ("M-t" 'help-go-back)
   ("M-n" 'help-fo-forward)))

;; ido
(with-eval-after-load "ido"
  (defun ido-init-completion-maps ())

  (setq ido-common-completion-map (make-sparse-keymap))
  (set-keymap-parent ido-common-completion-map minibuffer-local-map)
  (keymap-define-kbd
   ido-common-completion-map
   ("C-h")
   ("C-s")
   ("TAB" 'ido-complete)
   ("RET" 'ido-exit-minibuffer)
   ("M-h" 'ido-prev-match)
   ("M-s" 'ido-next-match)
   ("SPC" 'ido-complete-space)
   ("M-k" 'ido-completion-help)
   ("C-j" 'ido-select-text)
   ("RET" 'ido-exit-minibuffer)
   ("C-SPC" 'ido-restrict-to-matches)
   ("M-SPC" 'ido-take-first-match)
   ("M-t" 'ido-prev-work-directory)
   ("M-n" 'ido-next-work-directory)
   ("M-w" 'ido-undo-merge-work-directory)
   ("C-w" 'ido-merge-work-directories)
   ("C-v" 'ido-forget-work-directory)
   ("C-M-g" 'ido-toggle-literal)
   ("C-M-c" 'ido-toggle-ignore)
   ("C-M-r" 'ido-toggle-virtual-buffers)
   )

  (setq ido-file-dir-completion-map (make-sparse-keymap))
  (set-keymap-parent ido-file-dir-completion-map ido-common-completion-map)
  (keymap-define-kbd
   ido-file-dir-completion-map
   ("C-b" 'ido-enter-switch-buffer)
   ("C-f" 'ido-fallback-command)
   ("C-d" 'ido-enter-dired)
   ("C-t" 'ido-prev-match-dir)
   ("C-n" 'ido-next-match-dir)
   ("<backspace>" 'ido-delete-backward-updir)
   ("<C-backspace>" 'ido-up-directory)
   ("C-g" 'ido-delete-backward-updir)
   ("M-g" 'ido-delete-backward-word-updir)
   ("M-T" 'ido-prev-work-file)
   ("M-N" 'ido-next-work-file))

  (setq ido-file-completion-map (make-sparse-keymap))
  (set-keymap-parent ido-file-completion-map ido-file-dir-completion-map)
  (keymap-define-kbd
   ido-file-completion-map
   ("C-z" 'ido-delete-file-at-head))

  (setq ido-buffer-completion-map (make-sparse-keymap))
  (set-keymap-parent ido-buffer-completion-map ido-common-completion-map)
  (keymap-define-kbd
   ido-buffer-completion-map
   ("C-f" 'ido-enter-find-file)
   ("C-b" 'ido-fallback-command)
   ("C-z" 'ido-kill-buffer-at-head))

  (setq ido-enable-flex-matching t
        ido-use-filename-at-point nil
        ido-create-new-buffer 'never
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-use-faces t
        ido-buffer-disable-smart-matches nil
        ido-max-prospects 64
        ido-auto-merge-work-directories-length 0
        ido-auto-merge-delay-time 0
        ido-enter-matching-directory t
        ido-use-virtual-buffers t)
  )

(ido-mode 1)
(ido-everywhere 1)

;; ielm
(with-eval-after-load "ielm"
  (keymap-define-kbd
   ielm-map
   ("C-c")
   ("C-k C-b" 'ielm-change-working-buffer)
   ("C-k C-f" 'ielm-display-working-buffer)
   ("C-k C-v" 'ielm-print-working-buffer)))

;; info
(with-eval-after-load "info"
  (setq Info-mode-map (make-keymap))
  (suppress-keymap Info-mode-map)
  (keymap-define-kbd
   Info-mode-map
   ("RET" 'Info-follow-nearest-node)
   ("TAB" 'Info-next-reference)
   ("M-TAB" 'Info-prev-reference)
   ("M-T" 'Info-scroll-down)
   ("M-N" 'Info-scroll-up)
   ("q" 'Info-exit)
   ("h" 'Info-prev)
   ("s" 'Info-next)
   ("t" 'Info-up)
   ("o" 'Info-search)
   ("e" 'Info-copy-current-node-name)
   ("<" 'Info-top-node)
   (">" 'Info-final-node)
   ("w" 'Info-menu))

  (keymap-define-kbd
   Info-edit-mode-map
   ("C-c")
   ("C-k C-k" 'Info-cease-edit)))

;; lisp-mode
(with-eval-after-load "lisp-mode"
  (keymap-define-kbd
   lisp-mode-map
   ("C-c")
   ("C-k C-z" 'run-lisp)))

;; outline-mode
(with-eval-after-load "outline"
  (setq outline-mode-map (make-sparse-keymap))
  (keymap-define-kbd
   outline-mode-map
   ("C-M-g" 'outline-backward-same-level)
   ("C-M-c" 'outline-previous-visible-heading)
   ("C-M-r" 'outline-next-visible-heading)
   ("C-M-l" 'outline-forward-same-level)

   ("C-M-d" 'outline-up-heading)

   ("C-M-m" 'outline-promote)
   ("C-M-w" 'outline-move-subtree-up)
   ("C-M-v" 'outline-move-subtree-down)
   ("C-M-z" 'outline-demote)

   ;; Alternate bindings for terminals
   ("C-k C-g" [?\C-\M-g])
   ("C-k C-c" [?\C-\M-c])
   ("C-k C-r" [?\C-\M-r])
   ("C-k C-l" [?\C-\M-l])
   ("C-k C-d" [?\C-\M-d])
   ("C-k C-m" [?\C-\M-m])
   ("C-k C-w" [?\C-\M-w])
   ("C-k C-v" [?\C-\M-v])
   ("C-k C-z" [?\C-\M-z])


   ("C-k -" 'outline-insert-heading)
   ))
   ;; show-all
   ;; hide-entry  show-entry
   ;; hide-subtree show-subtree
   ;; show-children
   ;; show-branches
   ;; hide-other
   ;; hide-leaves
   ;; hide-sublevels
   ;; hide-body


;; package
(with-eval-after-load "package"
  (keymap-define-kbd
   package-menu-mode-map
   ("M-t" 'scroll-down-command)
   ("M-n" 'scroll-up-command)))

;; prog-mode
(with-eval-after-load "prog-mode"
  (add-hook 'prog-mode-hook #'linum-hook))

;; python-mode
(with-eval-after-load "python"
  (keymap-define-kbd
   python-mode-map
   ("C-k" (lookup-key python-mode-map [?\C-c]))
   ("C-c"))

  (add-hook-anon
   'python-mode-hook
   (setq python-check-command "pychecker --stdlib -# 0 -xXT"
         tab-width 4
         python-indent 4)))

;;sh-mode
(with-eval-after-load "sh-script"
  (keymap-define-kbd
   sh-mode-map
   ("C-k" (lookup-key sh-mode-map [?\C-c]))
   ("C-c"))

  (add-hook-anon
   'sh-mode-hook
   (sh-electric-here-document-mode -1)))

;; term
(with-eval-after-load "term"
  (keymap-define-kbd
   term-mode-map
   ("C-k" (lookup-key term-mode-map [?\C-c]))
   ("C-c")
   ("C-d")
   ("C-l" 'term-delchar-or-maybe-eof)
   ("M-p")
   ("M-t" 'term-previous-input)
   ("M-r")
   ("M-s")
   ("C-a" 'term-previous-matching-input)
   ("C-o" 'term-next-matching-input)))

;; tabulated-list

(with-eval-after-load "tabulated-list"
  (add-hook-anon
   'tabulated-list-mode-hook
   (setq show-trailing-whitespace nil)))

;; text-mode
;;(add-hook 'text-mode-hook #'tabstop-hook)
(add-hook 'text-mode-hook #'linum-hook)

;; with-editor

(with-eval-after-load "with-editor"
  (keymap-define-kbd
   with-editor-mode-map
   ("C-c")
   ("C-k C-k" 'with-editor-cancel)
   ("C-k C-c" 'with-editor-finish)))

;; Packages
;; --------

;; company
;; hooks: company-completion-(started|cancelled|finished)-hook
;; company-mode-map company-active-map
(el-register-package
 :name company
 :type elpa
 :after
 (progn
   (require 'company)
   ;; (add-to-list 'company-backends 'company-ghc)
   (keymap-define-kbd
    company-active-map
    ("C-g")
    ("M-," 'company-abort)
    ("C-h")
    ("M-k" 'company-show-doc-buffer)
    ("C-s")
    ("C-o" 'company-search-candidates)
    ("C-M-s")
    ("C-M-o" 'company-filter-candidates)
    ("M-n")
    ("M-p")
    ("M-t" 'company-select-previous)
    ("M-n" 'company-select-next)
    ("M-T" 'company-previous-page)
    ("M-N" 'company-next-page))))

;; dash
(el-register-package
 :name dash
 :type elpa)

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

;; git modes
;; commit, rebase, config, ignore
(el-use-package "git-modes")

;; haskell
;; (el-use-package "company-ghc")

;; (el-use-package "ac-ghc-mod")
(el-register-package
 :name ghc-mod
 :type github
 :pkgname "kazu-yamamoto/ghc-mod"
 :load-path "elisp")

(setq ghc-interactive-command "ghc-modi")
;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)

(el-register-package
 :name haskell-mode
 :type github
 :pkgname "haskell/haskell-mode"
 :info "."
 :build `(("make" ,(format "EMACS=%s" el-get-emacs) "all"))
 :post-init (require 'haskell-mode-autoloads)
 :after
 (progn
   (setq indent-tabs-mode nil
         tab-width 2
         haskell-indentation-cycle-warn nil
         ghc-hlint-options '("--ignore=Use camelCase")
         show-trailing-whitespace t
         ghc-display-error 'minibuffer
         ghc-display-hole 'other-buffer)
   (flyspell-prog-mode)
   (company-mode)
   (auto-fill-mode 1)
   (turn-on-haskell-indentation))
 )

;; (set (make-local-variable
;;       'fill-nobreak-predicate)
;;      (lambda ()
;;        (not (eq (get-text-property (point) 'face)
;;                 'font-lock-comment-face))))


;; TODO: ghc-mode edits haskell-mode-map
(with-eval-after-load "haskell-mode"
  (keymap-define-kbd
   haskell-mode-map
   ("C-k" (lookup-key haskell-mode-map [?\C-c]))
   ("C-c")))

(with-eval-after-load "haskell-cabal"
  (setq haskell-cabal-mode-map (make-sparse-keymap))
  (keymap-define-kbd
   haskell-cabal-mode-map
   ("M-t" 'haskell-cabal-previous-subsection)
   ("M-n" 'haskell-cabal-next-subsection)
   ("M-T" 'haskell-cabal-previous-section)
   ("M-N" 'haskell-cabal-next-section)
   ("C-k C-f" 'haskell-cabal-find-or-create-source-file)
   ("C-k C-s" 'haskell-cabal-subsection-arrange-lines)))

(with-eval-after-load "haskell-interactive-mode"
  (keymap-define-kbd
   haskell-interactive-mode-map
   ("C-k" (lookup-key haskell-interactive-mode-map [?\C-c]))
   ("C-c")
   ("C-a")
   ("M-n")
   ("M-p")
   ("M-A" 'haskell-interactive-mode-beginning)
   ("M-t" 'haskell-interactive-mode-history-previous)
   ("M-n" 'haskell-interactive-mode-history-next)
   ("M-G" 'haskell-interactive-mode-kill-whole-line)))

(mode-extension #'haskell-mode ".hs")
(mode-extension #'haskell-mode ".hs-boot")
(mode-extension #'haskell-cabal-mode ".cabal")

;; help-fns+
(el-register-package
 :name help-fns+
 :type elpa
 :features 'help-fns+
 :lazy nil)

;; magit
(el-register-package
 :name magit
 :type elpa
 :after
 (progn
   (require 'magit)
   (setq magit-last-seen-setup-instructions "1.4.0")
   ))

(with-eval-after-load "git-commit"
  (keymap-define-kbd
   git-commit-mode-map
   ("C-k" (lookup-key git-commit-mode-map [?\C-c]))
   ("C-c")
   ("M-n")
   ("M-p")
   ("M-t" 'git-commit-prev-message)
   ("M-n" 'git-commit-next-message)))

(with-eval-after-load "git-rebase"
  (keymap-define-kbd
   git-rebase-mode-map
   ("C-k")
   ("M-L" 'git-rebase-kill-line)
   ("C-x")
   ("p")
   ("t" 'git-rebase-backward-line)
   ("M-T" 'scroll-up-command)
   ("M-N" 'scroll-down-command)
   ("M-p")
   ("M-t" 'git-rebase-move-line-up)
   ("M-n" 'git-rebase-moveline-down)))

(with-eval-after-load "magit-mode"
  (keymap-define-kbd
   magit-mode-map
   ("C-k" (lookup-key magit-mode-map [?\C-c]))
   ("C-c")
   ("C-w")
   ("M-w")
   ("C-e" 'magit-copy-as-kill)
   ("M-e" 'magit-copy-buffer-thing-as-kill)
   ("p")
   ("n")
   ("M-p")
   ("M-n")
   ("M-t" 'magit-section-backward)
   ("M-n" 'magit-section-forward)
   ("M-T" 'magit-section-up)))

(with-eval-after-load "magit-log"
  (keymap-define-kbd
   magit-log-mode-map
   ("C-c")
   ("C-k h" 'magit-go-backward)
   ("C-k s" 'magit-go-forward))
  )

(with-eval-after-load "magit-diff"
  (keymap-define-kbd
   magit-diff-mode-map
   ("C-c")
   ("C-k h" 'magit-go-backward)
   ("C-k s" 'magit-go-forward)
   ("C-k C-d" 'magit-diff-while-committing)))


(with-eval-after-load "magit-popup"
  (keymap-define-kbd
   magit-popup-mode-map
   ("C-c")
   ("C-k C-c" 'magit-popup-set-default-arguments)
   ("C-h i")
   ("M-k i" 'magit-popup-info)
   ("C-g")
   ("M-," 'magit-popup-quit)
   ("C-p")
   ("C-t" 'backward-button)
   ("C-w" 'magit-popup-toggle-show-common-commands)))

;; markdown-mode
(el-register-package
 :name markdown-mode
 :type elpa)

(with-eval-after-load "markdown-mode"
  (setq markdown-mode-map (make-sparse-keymap))
  (keymap-define-kbd
   markdown-mode-map
   ("M-q" 'markdown-jump)
   ("M-;" 'markdown-follow-thing-at-point)

   ("RET" 'markdown-enter-key)
   ("DEL" 'markdown-exdent-or-delete)
   ("TAB" 'indent-for-tab-command)

   ("M-t" 'markdown-backward-paragraph)
   ("M-n" 'markdown-forward-paragraph)

   ("C-M-g" 'outline-backward-same-level)
   ("C-M-c" 'outline-previous-visible-heading)
   ("C-M-r" 'outline-next-visible-heading)
   ("C-M-l" 'outline-forward-same-level)

   ("C-M-d" 'markdown-insert-list-item)

   ("C-M-m" 'markdown-promote)
   ("C-M-w" 'markdown-move-up)
   ("C-M-v" 'markdown-move-down)
   ("C-M-z" 'markdown-demote)

   ("C-s-m" 'markdown-promote-subtree)
   ("C-s-w" 'markdown-move-subtree-up)
   ("C-s-v" 'markdown-move-subtree-down)
   ("C-s-z" 'markdown-demote-subtree)

   ;; Alternate bindings for terminals
   ("C-k C-g" [?\C-\M-g])
   ("C-k C-c" [?\C-\M-c])
   ("C-k C-r" [?\C-\M-r])
   ("C-k C-l" [?\C-\M-l])
   ("C-k C-d" [?\C-\M-d])
   ("C-k C-m" [?\C-\M-m])
   ("C-k C-w" [?\C-\M-w])
   ("C-k C-v" [?\C-\M-v])
   ("C-k C-z" [?\C-\M-z])
   ("C-k M-m" [?\C-\s-m])
   ("C-k M-w" [?\C-\s-w])
   ("C-k M-v" [?\C-\s-v])
   ("C-k M-z" [?\C-\s-z])
   )

  (setq markdown-indent-on-enter nil
        markdown-asymmetric-header t
        ;; markdown-indent-function 'tab-to-tab-stop ;; 'markdown-indent-line
        indent-line-function 'tab-to-tab-stop
        markdown-indent-on-enter nil
        markdown-enable-math t
        markdown-unordered-list-item-prefix "- "
        markdown-font-lock-support-mode nil)
  (add-hook-anon
   'markdown-mode-hook
   (flyspell-mode)
   (visual-line-mode)
   (wc-mode))
  )


(push '("---\\(.\\|\n\\)*format:\\s-*markdown" . markdown-mode) magic-mode-alist)
(mode-extension #'markdown-mode ".md")
(mode-extension #'markdown-mode ".mdown")
(mode-extension #'markdown-mode ".markdown")

;; org
(el-register-package
 :name org
 :type elpa)

(add-hook-anon
 'org-mode-hook
 (setq org-hide-leading-stars t
       org-startup-indented nil
       org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE"))))

(push '("---\\(.\\|\n\\)*format:\\s-*org" . org-mode) magic-mode-alist)

;; smart-tabs-mode
(el-use-package "smarttabs")

;; subatomic256
(el-register-package
 :name subatomic256-theme
 :type github
 :pkgname "duncanburke/subatomic256"
 :depends dash
 :prepare (add-to-list 'custom-theme-load-path
                       default-directory)
 :after
 (progn
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

(define-derived-mode large-text-mode
  text-mode "Large-Text"
  "Major mode for editing very large text files"
  (interactive))

(add-hook-anon 'large-text-mode-hook
               (linum-mode 0))

(mode-extension #'large-text-mode ".ck2")

(provide 'my-modes)
