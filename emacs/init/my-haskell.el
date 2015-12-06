(require 'el-helper)
(require 'utils)

(defvar haskell-enable-ghc-mod t)
(defvar haskell-enable-structured-indentation nil)
(defvar haskell-enable-company nil)
(defvar haskell-enable-hare nil)

(el-register-package
 :name haskell-mode
 :description "A Haskell editing mode"
 :type github
 :pkgname "haskell/haskell-mode"
 :info "."
 :build `(("make" ,(format "EMACS=%s" el-get-emacs) "all"))
 :post-init (require 'haskell-mode-autoloads))

(el-register-package
 :name ghc-mod
 :type github
 :pkgname "kazu-yamamoto/ghc-mod"
 :depends haskell-mode
 :load-path "elisp"
 :after
 (progn
   (autoload 'ghc-init "ghc" nil t)
   (autoload 'ghc-debug "ghc" nil t))
 )

(el-register-package
 :name company-ghc
 :description "Company-mode ghc-mod backend."
 :type github
 :pkgname "iquiw/company-ghc"
 :depends (company-mode ghc-mod)
 )

(when haskell-enable-structured-indentation
  (el-register-package
   :name structured-haskell-mode
   :description "Structured Haskell editing operations."
   :type github
   :pkgname "chrisdone/structured-haskell-mode"
   :depends (haskell-mode)
   :build `(("cabal" "install"))
   :load-path "elisp"
   :post-init (setq shm-program-name
                    (concat default-directory
                            "dist/build/structured-haskell-mode/structured-haskell-mode")))
  )

(with-eval-after-load "haskell-customize"
  (setq haskell-process-type 'stack-ghci
        haskell-stylish-on-save nil
        haskell-tags-on-save nil
        haskell-notify-p nil
        haskell-process-use-presentation-mode nil
        haskell-interactive-mode-include-file-name t
        haskell-interactive-mode-eval-mode nil
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-process-reload-with-fbytecode nil
        haskell-process-show-debug-tips t
        haskell-process-suggest-hoogle-imports nil
        haskell-process-suggest-haskell-docs-imports nil)
  )

(with-eval-after-load "ghc"
  (setq ghc-completion-key [?\C-\M-i]
        ghc-insert-key [?\C-\M-i]
        ghc-document-key []
        ghc-import-key []
        ghc-previous-key [?\M-t]
        ghc-next-key [?\M-n]
        ghc-help-key [?\M-?]
        ghc-sort-key []
        ghc-type-key []
        ghc-info-key []
        ghc-toggle-key []
        ghc-module-key []
        ghc-expand-key []
        ghc-kill-key []
        ghc-hoogle-key []
        ghc-shallower-key []
        ghc-deeper-key []
        ghc-refine-key []
        ghc-auto-key []
        ghc-prev-hole-key []
        ghc-next-hole-key [])

  (defun ghc-init ()
    (ghc-abbrev-init)
    (ghc-type-init)
    (unless ghc-initialized
      ;; (define-key haskell-mode-map ghc-completion-key  'ghc-complete)
      ;; (define-key haskell-mode-map ghc-document-key    'ghc-browse-document)
      ;; (define-key haskell-mode-map ghc-type-key        'ghc-show-type)
      ;; (define-key haskell-mode-map ghc-info-key        'ghc-show-info)
      ;; (define-key haskell-mode-map ghc-expand-key      'ghc-expand-th)
      ;; (define-key haskell-mode-map ghc-import-key      'ghc-import-module)
      ;; (define-key haskell-mode-map ghc-previous-key    'ghc-goto-prev-error)
      ;; (define-key haskell-mode-map ghc-next-key        'ghc-goto-next-error)
      ;; (define-key haskell-mode-map ghc-help-key        'ghc-display-errors)
      ;; (define-key haskell-mode-map ghc-insert-key      'ghc-insert-template-or-signature)
      ;; (define-key haskell-mode-map ghc-sort-key        'ghc-sort-lines)
      ;; (define-key haskell-mode-map ghc-toggle-key      'ghc-toggle-check-command)
      ;; (define-key haskell-mode-map ghc-jump-key        'ghc-jump-file)
      ;; (define-key haskell-mode-map ghc-module-key      'ghc-insert-module)
      ;; (define-key haskell-mode-map ghc-kill-key        'ghc-kill-process)
      ;; (define-key haskell-mode-map ghc-hoogle-key      'haskell-hoogle)
      ;; (define-key haskell-mode-map ghc-shallower-key   'ghc-make-indent-shallower)
      ;; (define-key haskell-mode-map ghc-deeper-key      'ghc-make-indent-deeper)
      ;; ;(define-key haskell-mode-map ghc-case-split-key  'ghc-case-split)
      ;; (define-key haskell-mode-map ghc-refine-key      'ghc-refine)
      ;; (define-key haskell-mode-map ghc-auto-key        'ghc-auto)
      ;; (define-key haskell-mode-map ghc-prev-hole-key   'ghc-goto-prev-hole)
      ;; (define-key haskell-mode-map ghc-next-hole-key   'ghc-goto-next-hole)
      (ghc-comp-init)
      (setq ghc-initialized t)
      (add-hook 'kill-buffer-hook 'ghc-kill-process)
      (defadvice save-buffer (after ghc-check-syntax-on-save activate)
        "Check syntax with GHC when a haskell-mode buffer is saved."
        (when (eq 'haskell-mode major-mode) (ghc-check-syntax)))
      )
    (ghc-import-module)
    (ghc-check-syntax))

  )

(defun my-haskell-mode-hook ()
  (setq indent-tabs-mode nil
        tab-width 2
        haskell-indentation-cycle-warn nil
        ghc-hlint-options '("--ignore=Use camelCase")
        show-trailing-whitespace t
        ghc-display-error 'minibuffer
        ghc-display-hole 'other-buffer)

  (cond
   (haskell-enable-structured-indentation (structured-haskell-mode))
   (t (haskell-indentation-mode)))

  (flyspell-prog-mode)
  (interactive-haskell-mode)
  
  (when haskell-enable-ghc-mod
    (ghc-init)
    (cond
     (haskell-enable-company
      (require 'company)
      (add-to-list 'company-backends 'company-ghc)
      (company-mode)
      (setq company-ghc-show-info t))
     (t
      (setq company-ghc-show-info nil))
     )
    (when haskell-enable-hare
      (hare-init))
    )


  )



;; TODO: ghc-mode edits haskell-mode-map
(with-eval-after-load "haskell-mode"
  (keymap-define-kbd
   haskell-mode-map
   ("C-k" (lookup-key haskell-mode-map [?\C-c]))
   ("C-c"))
  (add-hook 'haskell-mode-hook #'my-haskell-mode-hook)
  )

(with-eval-after-load "haskell-debug"
  (keymap-define-kbd
   haskell-debug-mode-map
   ("t" 'haskell-debug/previous)
   ("w" 'haskell-debug/trace)
   )
  )


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

(provide 'my-haskell)
