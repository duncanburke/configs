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
  (defun ghc-init ()
    (ghc-abbrev-init)
    (ghc-type-init)
    (unless ghc-initialized
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
   ("C-c"))

  (add-hook 'haskell-mode-hook #'my-haskell-mode-hook)

  ;; haskell-move-nested-left/right
  ;; beginning/end-of-defun
  ;; mark-defun
  ;; haskell-navigate-imports
  ;; haskell-mode-jump-to-def-or-tag


  (defhydra hydra-haskell (haskell-mode-map "C-b" :color pink :hint nil)
"
^Process^                   ^Info^                          ^Movement^              ^Actions^
----------------------------------------------------------------------------------------------------
_m_: haskell-compile        _c_: ghc-mod show type          _a_: jump to file       _C-M-h_: make indent shallower
_w_: switch to interactive  _C_: haskell show type          _A_: jump to def or tag _C-M-s_: make indent deeper
_W_: clear interactive      _r_: ghc-mod show info          _C-M-c_: prev hole      _C-M-H_: move nested left
_z_: process load-or-reload _R_: haskell show info          _C-M-r_: next hole      _C-M-S_: move nested right
_Z_: process restart        _l_: expand TH                  _b_: refine hole        _L_: insert template or signature
_v_: ghc-mod import module  _g_: ghc-mod display errors     _B_: auto fill hole     _o_: format imports
^ ^                         _G_: check-command: % -11`ghc-check-command _d_: navigate imports   _O_: sort lines
^ ^                         _C-M-g_: prev error             _C-M-t_: start of defun _e_: insert module
^ ^                         _C-M-l_: next error             _C-M-n_: end of defun   _u_: complete
^ ^                         _/_: browse documentation       _-_: mark defun
^ ^                         _?_: hoogle
"
    ("q" nil "exit" :color blue)
    ;; Process
    ("m" haskell-compile)

    ("w" haskell-interactive-switch)
    ("W" haskell-interactive-mode-clear)

    ("z" haskell-process-load-or-reload)
    ("Z" haskell-process-restart)

    ("v" ghc-import-module)

    ;; Info
    ("c" ghc-show-type)
    ("C" haskell-process-do-type)
    ("r" ghc-show-info)
    ("R" haskell-process-do-info)
    ("l" ghc-expand-th)

    ("g" ghc-display-errors)
    ("G" ghc-toggle-check-command) ;; ghc-check-command
    ("C-M-g" ghc-goto-prev-error)
    ("C-M-l" ghc-goto-next-error)

    ("/" ghc-browse-document)
    ("?" haskell-hoogle)

    ;; Movement
    ("a" ghc-jump-file)
    ("A" haskell-mode-jump-to-def-or-tag)

    ("C-M-c" ghc-goto-prev-hole)
    ("C-M-r" ghc-goto-next-hole)
    ("b" ghc-refine)
    ("B" ghc-auto)

    ("d" haskell-navigate-imports)

    ("C-M-t" beginning-of-defun);
    ("C-M-n" end-of-defun)
    ("-" mark-defun)

    ;; Actions
    ("C-M-h" ghc-make-indent-shallower)
    ("C-M-s" ghc-make-indent-deeper)
    ("C-M-H" haskell-move-nested-left)
    ("C-M-S" haskell-move-nested-right)

    ("L" ghc-insert-template-or-signature)

    ("o" haskell-mode-format-imports)
    ("O" ghc-sort-lines)
    ("e" ghc-insert-module)

    ("u" ghc-complete)

    ("" ignore :exit nil)
    )
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
