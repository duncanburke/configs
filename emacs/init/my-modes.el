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
   ("C-p" 'bs-abort)
   ("C-o")
   ("C-w" 'bs-tmp-select-other-window)
   ("C-t" 'bs-up)
   ("C-n" 'bs-down)))

;; buffer-menu


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
   ("RET" 'newline-and-indent)
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
   ("M-e"))

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

;; compilation-mode

(with-eval-after-load "compile"
  (keymap-define-kbd
   compilation-minor-mode-map
   ("C-c")
   ("C-o")
   ("M-n")
   ("M-p")
   ("M-{")
   ("M-}")

   ("M-t" 'compilation-previous-error)
   ("M-n" 'compilation-next-error)
   ("C-M-g" 'compilation-previous-file)
   ("C-M-l" 'compilation-next-file)

   ("C-k C-g" 'compilation-previous-file)
   ("C-k C-l" 'compilation-next-file)

   ("C-k C-k" 'kill-compilation))

  (keymap-define-kbd
   compilation-mode-map
   ("C-c")
   ("C-o")
   ("M-n")
   ("M-p")
   ("M-{")
   ("M-}")

   ("M-t" 'compilation-previous-error)
   ("M-n" 'compilation-next-error)
   ("C-M-g" 'compilation-previous-file)
   ("C-M-l" 'compilation-next-file)

   ("C-k C-g" 'compilation-previous-file)
   ("C-k C-l" 'compilation-next-file)

   ("C-k C-k" 'kill-compilation))
   )


;; dired
(with-eval-after-load "dired"
  (keymap-define-kbd
   dired-mode-map
   ("C-o")
   ("C-t")
   ("C-w" 'dired-toggle-read-only)
   ("C-M-d")
   ("C-M-n")
   ("C-M-p")
   ("C-M-u")
   ("M-s")
   ("p")
   ("t" 'dired-previous-line)
   ("M-{")
   ("M-}")
   ("M-t" 'dired-prev-marked-file)
   ("M-n" 'dired-next-marked-file)
   ("C-d" 'dired-up-directory)
   )
  ;; Allow dired to recursive delete without confirmation
  (setq dired-recursive-deletes 'always)
  ;; Stop dired from spamming windows as you navigate
  (put 'dired-find-alternate-file 'disabled nil))

;; doc-view
(with-eval-after-load "doc-view"
  (setq doc-view-mode-map (make-sparse-keymap))
  (set-keymap-parent doc-view-mode-map image-mode-map)
  (keymap-define-kbd
   doc-view-mode-map
   ("h" 'doc-view-previous-page)
   ("t" 'backward-page)
   ("n" 'forward-page)
   ("s" 'doc-view-next-page)
   ("C-t" 'doc-view-previous-line-or-previous-page)
   ("C-n" 'doc-view-next-line-or-next-page)
   ("M-t" 'doc-view-scroll-down-or-previous-page)
   ("M-n" 'doc-view-scroll-up-or-previous-page)
   ("M-<" 'doc-view-first-page)
   ("M->" 'doc-view-next-page)
   ("C-q" 'doc-view-goto-page)
   ("RET" 'image-next-line)
   ("+" 'doc-view-enlarge)
   ("=" 'doc-view-enlarge)
   ("-" 'doc-view-shrink)
   ("0" 'doc-view-scale-reset)
   ("m" 'doc-view-fit-height-to-window)
   ("w" 'doc-view-fit-width-to-window)
   ("v" 'doc-view-fit-page-to-window)
   ("k" 'doc-view-kill-proc)
   ("C-o" 'doc-view-search)
   ("C-a" 'doc-view-search-backward)
   ("C-k C-c" 'doc-view-toggle-display)
   ("C-k C-t" 'doc-view-open-text)
   ("g" 'doc-view-revert-buffer)
   )
  (keymap-define-kbd
   doc-view-minor-mode-map
   ("C-c")
   ("C-k C-c" 'doc-view-toggle-display)
   )
  )

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
   ("M-c" 'help-go-back)
   ("M-r" 'help-go-forward))

  (add-hook 'help-mode-hook #'my-help-mode-hook))

(defun my-help-mode-hook ()
  (linum-mode -1)
  (setq show-trailing-whitespace nil))

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

  (defun ido-complete-enter-dired ()
    (interactive)
    (cond
     (ido-matches
      (ido-set-current-directory ido-current-directory (ido-name (car ido-matches)) t)
      (setq ido-text ""
            ido-exit 'dired)
      (exit-minibuffer))
     (t
      (ido-enter-dired))))

  (setq ido-file-dir-completion-map (make-sparse-keymap))
  (set-keymap-parent ido-file-dir-completion-map ido-common-completion-map)
  (keymap-define-kbd
   ido-file-dir-completion-map
   ("C-b" 'ido-enter-switch-buffer)
   ("C-f" 'ido-fallback-command)
   ("C-d" 'ido-complete-enter-dired)
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
        ido-auto-merge-work-directories-length -1
        ido-auto-merge-delay-time 0
        ido-enter-matching-directory nil
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

;; image-mode
(with-eval-after-load "image-mode"
  (keymap-define-kbd
   image-mode-map
   ("C-c")
   ("C-k C-c" 'image-toggle-display)
   ("b")
   ("n")
   ("p")
   ("h" 'image-previous-frame)
   ("t" 'image-previous-file)
   ("n" 'image-next-file)
   ("s" 'image-next-frame)
   ))

;; info
(with-eval-after-load "info"
  (setq Info-mode-map (make-keymap))
  (suppress-keymap Info-mode-map)
  (keymap-define-kbd
   Info-mode-map
   ("RET" 'Info-follow-nearest-node)
   ("TAB" 'Info-next-reference)
   ("<backtab>" 'Info-prev-reference)
   ("d" 'Info-up)
   ("q" 'Info-exit)
   ("h" 'Info-prev)
   ("s" 'Info-next)
   ("t" 'Info-scroll-down)
   ("n" 'Info-scroll-up)
   ("M-t" 'Info-prev-reference)
   ("M-n" 'Info-next-reference)
   ("o" 'Info-search)
   ("e" 'Info-copy-current-node-name)
   ("<" 'Info-top-node)
   (">" 'Info-final-node)
   ("w" 'Info-menu)
   ("a" 'Info-index)
   ("o" 'Info-index-next)
   ("M-A" 'Info-search)
   ("M-O" 'Info-search-next)
   ("C-M-a" 'Info-search-backward)
   )

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

;; lv
(with-eval-after-load "lv"
  (defun lv-window--fixup (fn)
    (let ((original-window (selected-window))
          (lv-window (funcall fn)))
      (select-window lv-window)
      (linum-mode -1)
      (setq show-trailing-whitespace nil)
      (select-window original-window)
      lv-window))
  (advice-add 'lv-window :around #'lv-window--fixup))

;; nxml-mode
(with-eval-after-load "nxml-mode"
  (keymap-define-kbd
   nxml-mode-map
   ("C-c")
   ("C-M-d")
   ("C-M-n")
   ("C-M-p")
   ("C-M-u")
   ("M-h")
   ("M-{")
   ("M-}")
   ("M-h" 'nxml-backward-up-element)
   ("M-t" 'nxml-backward-element)
   ("M-n" 'nxml-forward-element)))

;; outline-mode
(with-eval-after-load "outline"
  (require 'hydra)
  (setq outline-mode-map (make-sparse-keymap))
  (keymap-define-kbd
   outline-mode-map
   ("C-k -" 'outline-insert-heading))

  (defhydra hydra-outline (outline-mode-map "C-b" :color pink :hint nil)
    "
^Movement^              ^Subtree^
^^^^^^^^----------------------------------
_h_: up heading         _C-h_: promote
_t_: next heading       _C-t_: move up
_n_: prev heading       _C-n_: move down
_T_: up same level      _C-s_: demote
_N_: down same level
"
    ("q" nil "exit" :color blue)

    ("h" outline-up-heading)
    ("t" outline-previous-visible-heading)
    ("n" outline-next-visible-heading)
    ("s" ignore)

    ("H" nil)
    ("T" outline-backward-same-level)
    ("N" outline-forward-same-level)
    ("S" ignore)

    ("C-h" outline-promote)
    ("C-t" outline-move-subtree-up)
    ("C-n" outline-move-subtree-down)
    ("C-s" outline-demote)

    ("" ignore :exit nil)
    )
  )

   ;; show-all
   ;; hide-entry  show-entry
   ;; hide-subtree show-subtree
   ;; show-children
   ;; show-branches
   ;; hide-other
   ;; hide-leaves
   ;; hide-sublevels
   ;; hide-body


;; org-mode
(with-eval-after-load "org"
  (keymap-define-kbd
   org-mode-map
   ("C-a")
   ("M-H" 'org-beginning-of-line)
   ("C-e")
   ("M-S" 'org-end-of-line)
   ("C-k" (lookup-key org-mode-map [?\C-c]))
   ("C-c")
   ("C-y")
   ("C-u" 'org-yank)

   ("C-d" 'org-up-element)

   ("C-M-i")
   ("C-M-t")
   ("M-a")
   ("M-e")
   ("M-h")
   ("M-{")
   ("M-}")

   ("<C-S-up>")
   ("<C-S-down>")
   ("<C-S-left>")
   ("<C-S-return>")
   ("<C-S-right>")
   ("<M-S-up>")
   ("<M-S-down>")
   ("<M-S-left>")
   ("<M-S-return>")
   ("<M-S-right>")
   ("<M-up>")
   ("<M-down>")
   ("<M-left>")
   ("<M-return>")
   ("<M-right>")
   ("<S-up>")
   ("<S-down>")
   ("<S-left>")
   ("<S-return>")
   ("<S-right>")

   ;; Cursor Movement
   ("M-7" 'outline-up-heading)
   ("M-8" 'outline-backward-same-level)
   ("M-9" 'outline-previous-visible-heading)
   ("M-0" 'outline-next-visible-heading)
   ("M-[" 'outline-forward-same-level)

   ;; Heading or table row/column movement
   ("M-*" 'org-metaleft)
   ("M-(" 'org-metaup)
   ("M-)" 'org-metadown)
   ("M-{" 'org-metaright)

   ;; Heading movement or table row/column deletion
   ("M-s-g" 'org-shiftmetaleft)
   ("M-s-c" 'org-shiftmetaup)
   ("M-s-r" 'org-shiftmetadown)
   ("M-s-l" 'org-shiftmetaright)

   ;; Context-dependent cycling
   ("C-/" 'org-shiftleft)
   ("C-=" 'org-shiftright)

   ;; Timestamp or priority movement
   ("M-/" 'org-shiftup)
   ("M-=" 'org-shiftdown)

   ;; TODO set cycling
   ("M-?" 'org-shiftcontrolleft)
   ("M-+" 'org-shiftcontrolright)

   ;; Change timestamps synchronously
   ("s-/" 'org-shiftcontrolup)
   ("s-+" 'org-shiftcontroldown)
   )
  )

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

;; reb-mode
(with-eval-after-load "re-builder"
  (keymap-define-kbd
   reb-mode-map
   ("C-k" (lookup-key reb-mode-map [?\C-c]))
   ("C-c")
   ("C-k C-q")
   ("C-k C-k" 'reb-quit)))

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

;; tar-mode

(with-eval-after-load "tar-mode"
  (keymap-define-kbd
   tar-mode-map
   ("p")
   ("t" 'tar-previous-line)))

;; text-mode
;;(add-hook 'text-mode-hook #'tabstop-hook)

(with-eval-after-load "text-mode"
  (add-hook 'text-mode-hook #'linum-hook)
  (add-hook 'text-mode-hook #'visual-line-mode))

;; with-editor

(with-eval-after-load "with-editor"
  (keymap-define-kbd
   with-editor-mode-map
   ("C-c")
   ("C-k C-k" 'with-editor-cancel)
   ("C-k C-c" 'with-editor-finish)))

;; Packages
;; --------

;; bbcode-mode
(el-register-package
 :name bbcode-mode
 :type elpa
 )

(with-eval-after-load "bbcode-mode"
  (keymap-define-kbd
   bbcode-mode-map
   ("C-c"))
  )


;; company
;; hooks: company-completion-(started|cancelled|finished)-hook
;; company-mode-map company-active-map
(el-register-package
 :name company
 :type elpa)

(with-eval-after-load "company"
   ;; (add-to-list 'company-backends 'company-ghc)
   (keymap-define-kbd
    company-active-map
    ("C-g")
    ("C-p" 'company-abort)
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
    ("M-N" 'company-next-page))

   (keymap-define-kbd
    company-search-map
    ("C-g")
    ("C-p" 'company-search-abort)
    ("C-o") ;;company-search-toggle-filtering
    ("C-r")
    ("C-s")
    ("C-a" 'company-search-repeat-backward)
    ("C-o" 'company-search-repeat-forward)
    ("C-g" 'company-search-delete-char)
    ("C-t" 'company-select-previous-or-abort)
    ("C-n" 'company-select-next-or-abort)
    ("M-p")
    ("M-t" 'company-select-previous)
    ("M-n" 'company-select-next)))

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

;; frink-mode
(el-register-package
 :name frink
 :type http
 :url "https://futureboy.us/frinktools/emacs/frink-mode.el"
 :after (autoload 'frink-mode "frink-mode.el"))

(with-eval-after-load "frink-mode"
  (keymap-define-kbd
   frink-mode-map
   ("C-c")
   ("C-k C-c" 'frink-run-buffer)
   ("C-k C-l" 'frink-run-buffer-then-interactive)
   ("C-k C-z" 'run-frink)
   ("{")
   ("}")
   )

  (add-hook-anon
   'frink-mode-hook
   (linum-mode)
   (setq comment-start "//"))
  (defun frink-interactive ()
    (interactive)
    (let ((outbuf (get-buffer-create (concat "*Frink*"))))
      (switch-to-buffer-other-window outbuf)
      (comint-mode)
      (erase-buffer)
      (setq comint-process-echoes t)  ;; Suppress echoing of input.
      (comint-exec outbuf "\"frink\"" "frink" nil '("-k"))
      (end-of-buffer)))
  )
(mode-extension #'frink-mode ".frink")

;; git modes
;; commit, rebase, config, ignore
(el-use-package "git-modes")

;; haskell

(require 'my-haskell)

;; hydra
(el-use-package "hydra")

(with-eval-after-load "hydra"
  (keymap-define-kbd
   hydra-base-map
   ("C-u")
   ("C-p" 'hydra--universal-argument)))

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
  (require 'magit-diff) ;; magit-diff adds a binding
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
   ("M-T" 'scroll-down-command)
   ("M-N" 'scroll-up-command)
   ("M-p")
   ("M-t" 'git-rebase-move-line-up)
   ("M-n" 'git-rebase-move-line-down)))

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
   ("C-p" 'magit-popup-quit)
   ("C-p")
   ("C-t" 'backward-button)
   ("C-w" 'magit-popup-toggle-show-common-commands)))

;; markdown-mode
(el-register-package
 :name markdown-mode
 :depends (hydra)
 :type github
 :pkgname "jrblevin/markdown-mode")

(with-eval-after-load "markdown-mode"
  (require 'hydra)
  (setq markdown-mode-map (make-sparse-keymap))
  (keymap-define-kbd
   markdown-mode-map
   ("M-q" 'markdown-jump)
   ("M-;" 'markdown-follow-thing-at-point)

   ("RET" 'markdown-enter-key)
   ("DEL" 'markdown-exdent-or-delete)
   ("TAB" 'indent-for-tab-command)

   ("M-t" 'markdown-backward-paragraph)
   ("M-n" 'markdown-forward-paragraph))

  (defhydra hydra-markdown (markdown-mode-map "C-b" :color pink :hint nil)
    "
^Movement^              ^Header Actions^    ^Subtree Actions^
^^^^^^^^------------------------------------------------------------
_h_: up heading         _C-h_: promote      _M-h_: promote subtree
_t_: next heading       _C-t_: move up      _M-t_: move subtree up
_n_: prev heading       _C-n_: move down    _M-n_: move subtree down
_T_: up same level      _C-s_: demote       _M-s_: demote subtree
_N_: down same level
"
    ("q" nil "exit" :color blue)

    ("h" outline-up-heading)
    ("t" markdown-previous-visible-heading)
    ("n" markdown-next-visible-heading)
    ("s" ignore)

    ("H" nil)
    ("T" outline-backward-same-level)
    ("N" outline-forward-same-level)
    ("S" ignore)

    ("C-h" markdown-promote)
    ("C-t" markdown-move-up)
    ("C-n" markdown-move-down)
    ("C-s" markdown-demote)

    ("M-h" markdown-promote-subtree)
    ("M-t" markdown-move-subtree-up)
    ("M-n" markdown-move-subtree-down)
    ("M-s" markdown-demote-subtree)

    ("" ignore :exit nil))

    ;;markdown-insert-list-item

  (setq markdown-indent-on-enter nil
        markdown-asymmetric-header t
        ;; markdown-indent-function 'tab-to-tab-stop ;; 'markdown-indent-line
        markdown-indent-on-enter nil
        markdown-enable-math t
        markdown-unordered-list-item-prefix "- "
        markdown-font-lock-support-mode 'jit-lock-mode)
  (add-hook-anon
   'markdown-mode-hook
   (flyspell-mode)
   (visual-line-mode)
   (wc-mode)
   (setq indent-line-function 'tab-to-tab-stop)
   )
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

;; ruby-mode
(with-eval-after-load 'ruby-mode
  (keymap-define-kbd
   ruby-mode-map
   ("M-C-p")
   ("M-C-n")))

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
  bbcode-mode "Writing"
  "Major mode for writing large bodies of text."
  (interactive)
  (visual-line-mode)
  ;; visual-fill-column mode is too buggy
  ;; (setq visual-fill-column-width 95)
  ;; (visual-fill-column-mode)
  (flyspell-mode)
  (wc-mode))

(defun my-writing-mode-hook ()
  (linum-mode -1))

(add-hook 'writing-mode-hook #'my-writing-mode-hook)

(define-derived-mode large-text-mode
  text-mode "Large-Text"
  "Major mode for editing very large text files"
  (interactive))

(add-hook-anon 'large-text-mode-hook
               (linum-mode 0))

(mode-extension #'large-text-mode ".ck2")

(provide 'my-modes)
