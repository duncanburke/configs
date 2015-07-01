(let ((minibuffer-bindings
       `((,(kbd "C-n") . next-history-element)
         (,(kbd "C-t") . previous-history-element)
         (,(kbd "M-n") . next-matching-history-element)
         (,(kbd "C-t") . previous-matching-history-element))))
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
                      `((,(kbd "M-s") . scroll-right)
                        (,(kbd "M-h") . scroll-left)
                        (,(kbd "C-t") . bs-up)
                        (,(kbd "C-n") . bs-down))))

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
   `((,(kbd "M-a") . comint-history-isearch-backward-regexp))))

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

;; edmacro-mode
(with-eval-after-load "edmacro"
  (my-keys-remap-mode 'edmacro-mode-map))

;; eshell
(with-eval-after-load "eshell"
  (my-keys-remap-mode 'eshell-mode-map))

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
                                       `((,(kbd "C-h") . nil)
                                         (,(kbd "C-s") . nil)
                                         (,(kbd "C-l") . nil)
                                         (,(kbd "C-,") . ido-exit-minibuffer)
                                         (,(kbd "C-.") . nil)
                                         (,(kbd "C-g") . nil)
                                         (,(kbd "M-g") . nil)
                                         (,(kbd "C-t") . ido-prev-match)
                                         (,(kbd "C-n") . ido-next-match)))
                   (my-keys-remap-mode 'ido-file-dir-completion-map
                                       `((,(kbd "C-g") . ido-delete-backward-updir)
                                         (,(kbd "M-c") . ido-delete-backward-word-updir)
                                         (,(kbd "C-t") . ido-prev-match-dir)
                                         (,(kbd "C-n") . ido-next-match-dir)
                                         (,(kbd "C-f") . ido-fallback-command)
                                         (,(kbd "C-b") . ido-enter-dired)))
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
   `((,(kbd "C-a") . isearch-repeat-backward)
     (,(kbd "C-o") . isearch-repeat-forward)
     (,(kbd "M-a") . isearch-repeat-backward)
     (,(kbd "M-o") . isearch-repeat-forward)
     (,(kbd "C-,") . isearch-abort)
     (,(kbd "C-e") . isearch-quote-char)
     (,(kbd "M-p") . isearch-ring-retreat)
     (,(kbd "M-t") . isearch-ring-advance)
     (,(kbd "C-g") . isearch-delete-char)))
  (my-keys-remap-mode
   'minibuffer-local-isearch-map
   `((,(kbd "TAB") . isearch-complete-edit)
     (,(kbd "C-a") . isearch-reverse-exit-minibuffer)
     (,(kbd "C-o") . isearch-forward-exit-minibuffer)
     (,(kbd "M-a") . isearch-reverse-exit-minibuffer)
     (,(kbd "M-o") . isearch-forward-exit-minibuffer))))


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
                      '(,(kbd "RET") . 'newline-and-indent))
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
(add-to-list 'load-path "/home/duncan/.emacs.d/ghc-mod/")

;; company
;; hooks: company-completion-(started|cancelled|finished)-hook
(use-package company
  :ensure t
  :config
  (my-keys-remap-mode 'company-mode-map)
  (my-keys-remap-mode 'company-active-map)
  (add-to-list 'company-backends 'company-ghc))

;; company-ghc
(use-package company-ghc
  :ensure t)

;; flx
(use-package flx
  :ensure t)

;; flx-ido
(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode 1))

;; ghc
;; (use-package ghc
;;   :ensure t)

(setq ghc-interactive-command "ghc-modi")
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

;; git-commit-mode
(use-package git-commit-mode
  :ensure t
  ;; magit modifies git-commit-mode-map so we defer the
  ;; remapping to after magit
  )

;; git-rebase-mode
(use-package git-rebase-mode
  :ensure t
  :config
  (my-keys-remap-mode 'git-rebase-mode-map))

;; gitconfig-mode
(use-package gitconfig-mode
  :ensure t)

;; gitignore-mode
(use-package gitignore-mode
  :ensure t)

;; haskell-mode
(defvar haskell-mode-remapped nil)
(use-package haskell-mode
  :ensure t)

(with-eval-after-load "haskell-cabal"
  (my-keys-remap-mode 'haskell-cabal-mode-map))

(with-eval-after-load "haskell-interactive-mode"
  (my-keys-remap-mode 'haskell-interactive-mode-map))

(add-hook-anon
 'haskell-mode-hook
 (setq indent-tabs-mode nil
       tab-width 2
       haskell-indentation-cycle-warn nil
       ghc-hlint-options '("--ignore=Use camelCase")
       show-trailing-whitespace t
       ghc-display-error 'minibuffer
       ghc-display-hole 'other-buffer)
 (ghc-init)
 (unless haskell-mode-remapped
   ;; ghc-mode edits `haskell-mode-map`, so we need to defer the
   ;; remapping until ghc has been loaded for the first time
   (my-keys-remap-mode 'haskell-mode-map)
   (setq haskell-mode-remapped t))
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
(use-package help-fns+
  :ensure t)

;; magit
(use-package magit
  :ensure t
  :config
  (my-keys-remap-mode 'git-commit-mode-map)
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
  (my-keys-remap-mode 'magit-section-jump-map))

;; monkey-patch magit to show patch on commit buffer
(advice-add #'magit-key-mode-popup-committing :after
            (lambda ()
              (magit-key-mode-toggle-option (quote committing) "--verbose")))

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (my-keys-remap-mode
   'markdown-mode-map
   `((,(kbd "M-h") . nil)
     (,(kbd "M-t") . nil)
     (,(kbd "M-n") . nil)
     (,(kbd "M-s") . nil))))

(add-hook-anon
 'markdown-mode-hook
 (flyspell-mode)
 (visual-line-mode)
 (wc-mode))

(push '("---\\(.\\|\n\\)*format:\\s-*markdown" . markdown-mode) magic-mode-alist)
(mode-extension #'markdown-mode ".md")

;; org
(use-package org
  :ensure t
  :config
  (my-keys-remap-mode 'org-mode-map))

(add-hook-anon
 'org-mode-hook
 (setq org-hide-leading-stars t
       org-startup-indented nil
       org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE"))))

(push '("---\\(.\\|\n\\)*format:\\s-*org" . org-mode) magic-mode-alist)

;; rainbow-blocks-mode

(use-package rainbow-blocks
  :ensure t)

;; smart-tabs-mode
(use-package smart-tabs-mode
  :ensure t)

(provide 'my-modes)

;; visual-fill-column

(use-package visual-fill-column
  :ensure t)

;; wc-mode

(use-package wc-mode
  :ensure t)

;; yaml-mode
(use-package yaml-mode
  :ensure t)
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
