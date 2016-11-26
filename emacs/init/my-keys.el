(require 'utils)

(set-quit-char ?\C-p)

(keymap-define-kbd
 global-map
 ("C-'")
 ("M-'" 'comment-region)
 ("M-\"" 'uncomment-region)
 ("C-M-'")

 ("C-,")
 ("M-,")
 ("M-<" 'beginning-of-buffer)
 ("C-M-,")

 ("C-.")
 ("M-." 'keyboard-quit)
 ("M->" 'end-of-buffer)
 ("C-M-.")

 ("C-p" 'keyboard-quit)
 ("M-p" 'universal-argument)
 ("M-P")
 ("C-M-p")

 ("C-y" 'quoted-insert)
 ("M-y")
 ("M-Y")
 ("C-M-y")

 ("C-a" 'isearch-backward)
 ("M-a" 'search-backward-regexp)
 ("M-A" 'query-replace)
 ("C-M-a" 'replace-regexp)

 ("C-o" 'isearch-forward)
 ("M-o" 'isearch-forward-regexp)
 ("M-O" 'query-replace-regexp)
 ("C-M-o" 'replace-regexp)

 ("C-e" 'kill-region)
 ("M-e" 'kill-ring-save)
 ("M-E")
 ("s-e" 'clipboard-kill-ring-save)
 ("C-M-e")

 ("C-u" 'yank)
 ("M-u" 'yank-pop)
 ("M-U")
 ("s-u" 'clipboard-yank)
 ("C-M-u")

 ;; TODO: TABS!!!
 ;; ("C-i" ...)

 ("C-;")
 ("M-;")
 ("M-:" 'eval-expression)
 ("C-M-;")

 ("C-q" 'goto-line)
 ("M-q")
 ("M-Q")
 ("C-M-q")

 ("C-j" 'newline)
 ("M-j")
 ("M-J")
 ("C-M-j")

 ("C-k" 'mode-specific-command-prefix)
 ("M-k" 'help-command)
 ("M-K")
 ("C-M-k")

 ("C-x" 'Control-X-prefix)
 ("M-x")
 ("M-X")
 ("C-M-x")

 ("C-f")
 ("M-f")
 ("M-F")
 ("C-M-f")

 ("C-g" 'backward-delete-char)
 ("M-g" 'generalised-backward-kill-word)
 ("M-G" 'backward-kill-line)
 ("C-M-g")

 ("C-c" 'kill-previous-line)
 ("M-c" 'backward-kill-paragraph)
 ("M-C") ;; TODO
 ("C-M-c")

 ("C-r" 'kill-next-line)
 ("M-r" 'kill-paragraph)
 ("M-R") ;; TODO
 ("C-M-r")

 ("C-l" 'delete-forward-char)
 ("M-l" 'generalised-forward-kill-word)
 ("M-L" 'kill-line)
 ("C-M-l")

 ("C-/")
 ("M-/")
 ("M-?")
 ("C-M-/")

 ("C-=")
 ("M-=")
 ("M-+")
 ("C-M-=")

 ("C-d")
 ("M-d")
 ("M-D")
 ("C-M-d")

 ("C-h" 'backward-char)
 ("M-h" 'generalised-backward-word)
 ("M-H" 'beginning-of-line)
 ("C-M-h" 'windmove-left)

 ("C-t" 'previous-line)
 ("M-t" 'backward-paragraph)
 ("M-T" 'scroll-down-command)
 ("C-M-t" 'windmove-up)

 ("C-n" 'next-line)
 ("M-n" 'forward-paragraph)
 ("M-N" 'scroll-up-command)
 ("C-M-n" 'windmove-down)

 ("C-s" 'forward-char)
 ("M-s" 'generalised-forward-word)
 ("M-S" 'end-of-line)
 ("C-M-s" 'windmove-right)

 ("C--" compose-map)
 ("M--" 'negative-argument)
 ("C-M--")
 ("C-_")
 ("M-_")
 ("C-M-_")

 ("C-b")
 ("M-b")
 ("M-B")
 ("C-M-b")

 ("C-m" 'newline)
 ("M-m")
 ("M-M")
 ("C-M-m")

 ("C-w")
 ("M-w")
 ("M-W")
 ("C-M-w")

 ("C-v")
 ("M-v")
 ("M-V")
 ("C-M-v")

 ("C-z" 'undo)
 ("M-z" 'undo-only)
 ("M-Z")
 ("C-M-z")

 ("<up>")
 ("<C-up>")
 ("<M-up>")
 ("<C-M-up>")
 ("<C-M-s-up>")

 ("<down>")
 ("<C-down>")
 ("<M-down>")
 ("<C-M-down>")
 ("<C-M-s-down>")

 ("<left>")
 ("<C-left>")
 ("<M-left>")
 ("<C-M-left>")
 ("<C-M-s-left>")

 ("<right>")
 ("<C-right>")
 ("<M-right>")
 ("<C-M-right>")
 ("<C-M-s-right>")

 ("<prior>")
 ("<C-prior>")
 ("<M-prior>")
 ("<C-M-prior>")
 ("<C-M-s-prior>")

 ("<next>")
 ("<C-next>")
 ("<M-next>")
 ("<C-M-next>")
 ("<C-M-s-next>")

 ("DEL" 'backward-delete-char)
 ("C-DEL")
 ("M-DEL")
 ("C-M-DEL")
 ("C-M-s-DEL")
 )


(keymap-define-kbd
 ctl-x-map
 ("C-m" 'execute-extended-command)
 ("C-p" 'ignore)
 ("t" 'delete-trailing-whitespace)
 ("w" 'whitespace-mode)
 ("b" 'switch-to-buffer)
 ("l" 'linum-mode)
 ("g" 'magit-status)
 ("x" 'switch-to-scratch)
 ("C-r" 're-builder)


 ("C-k")
 ("C-l")
 ("C-n")
 ("C-o")
 ("C-p")
 ("C-q")
 ("C-u")
 ("C-x")
 ("C-z")

 ;; hydra bindings
 ("r")
 ("C-t")
 ("i")
 )

(keymap-define-kbd
 mode-specific-map
 ("C-p" 'ignore))

(keymap-define-kbd
 help-map
 ("i" 'info)
 ("u" 'Info-virtual-index))

(keymap-define-kbd
 minibuffer-local-map
 ("C-g")
 ("<XF86Back>")
 ("<XF86Forward>")
 ("<down>")
 ("<next>")
 ("<prior>")
 ("<up>")
 ("M-n")
 ("M-p")
 ("M-r")
 ("M-s")

 ("C-p" 'abort-recursive-edit)
 ("C-t" 'previous-history-element)
 ("C-n" 'next-history-element)
 ("C-a" 'previous-matching-history-element)
 ("C-o" 'next-matching-history-element)
 )

(keymap-define-kbd
 minibuffer-local-completion-map
 ("<prior>")
 ("M-v")
 ("C-w" 'switch-to-completions))

(keymap-define-kbd
 completion-list-mode-map
 ("C-w" 'delete-completion-window)
 ("M-t" 'previous-completion)
 ("M-n" 'next-completion)
 ("<left>")
 ("<right>"))

(keymap-define-kbd
 query-replace-map
 ("C-g")
 ("C-p" 'quit)
 ("C-h")
 ("M-k" 'help)
 ("C-l")
 ("C-w" 'recenter)
 ("C-r")
 ("C-w" 'edit)
 ("C-v")
 ("C-e" 'delete-and-edit)
 ("C-]")
 ("<M-next>")
 ("<M-prior>")
 ("<next>")
 ("<prior>")
 ("C-M-v")
 ("M-v")
 ("C-M-s-v")
 ("M-t" 'scroll-up)
 ("M-n" 'scroll-down)
 ("C-M-t" 'scroll-other-window)
 ("C-M-n" 'scroll-other-window-down))

(keymap-define-kbd
 goto-map
 ("c")
 ("g")
 ("p")
 ("t" 'previous-error)
 ("n" 'next-error)
 ("M-g")
 ("M-n")
 ("M-p")
 ("C-q" 'goto-line)
 ("M-q" 'goto-char)
 )

(keymap-define-kbd
 abbrev-map
 ("p")
 ("t" 'expand-jump-to-previous-slot)
 ("C-a")
 ("C-d" 'add-mode-abbrev))

(keymap-define-kbd
 universal-argument-map
 ("C-u")
 ("C-p" 'universal-argument-more))

(with-eval-after-load "isearch"
  (keymap-define-kbd
   isearch-mode-map
   ("C-g")
   ("C-p" 'isearch-abort)
   ("C-q")
   ("C-y" 'isearch-quote-char)
   ("C-r")
   ("C-a" 'isearch-repeat-backward)
   ("C-s")
   ("C-o" 'isearch-repeat-forward)
   ("C-h")
   ("C-k C-k" 'isearch-help-for-help)
   ("C-k ?" 'isearch-help-for-help)
   ("C-k b" 'isearch-describe-bindings)
   ("C-k k" 'isearch-describe-key)
   ("C-k m" 'isearch-describe-mode)
   ("C-k ," 'help-quit)
   ("C-M-r")
   ("M-a" 'isearch-repeat-backward)
   ("C-M-s")
   ("M-o" 'isearch-repeat-forward)

   ("C-w")
   ("C-e" 'isearch-yank-word-or-char)
   ("C-M-w")
   ("C-M-e" 'isearch-del-char)
   ("C-y")
   ("C-u" 'isearch-yank-kill)
   ("C-M-y")
   ("C-M-u" 'isearch-yank-char)
   ("M-y")
   ("M-u" 'isearch-yank-pop)

   ("M-%")
   ("M-A" 'isearch-query-replace)
   ("C-M-%")
   ("M-O" 'isearch-query-replace-regexp)
   ("M-c")
   ("C-w" 'isearch-edit-string)
   ("M-n")
   ("M-n" 'isearch-ring-advance)
   ("M-p")
   ("M-t" 'isearch-ring-retreat)
   ("M-r")
   ("C-v" 'isearch-toggle-regexp)

   ("M-s")
   ("<f1>")
   ("<help>"))
  (setq minibuffer-local-isearch-map (make-sparse-keymap))
  (set-keymap-parent minibuffer-local-isearch-map minibuffer-local-map)
  (keymap-define-kbd
   minibuffer-local-isearch-map
   ("RET" 'exit-minibuffer)
   ("C-M-i" 'isearch-complete-edit)
   ("C-a" 'isearch-reverse-exit-minibuffer)
   ("C-o" 'isearch-forward-exit-minibuffer)
   ("C-s" 'isearch-yank-char-in-minibuffer))
  )

(with-eval-after-load "tabulated-list"
  (keymap-define-kbd
   tabulated-list-mode-map
   ("p")
   ("t" 'previous-line)))

(setq compose-map (make-sparse-keymap))
(compose-map-define
 compose-map
 ("a" 945)
 ("b" 946)
 ("g" 947)
 ("d" 948)
 ("e" 949)
 ("z" 950)
 ("h" 951)
 ("M-t" 952)
 ("i" 953)
 ("k" 954)
 ("l" 955)
 ("m" 956)
 ("n" 957)
 ("x" 958)
 ("o" 959)
 ("p" 960)
 ("r" 961)
 ("s" 963)
 ("t" 964)
 ("u" 965)
 ("q" 966)
 ("M-x" 967)
 ("M-q" 968)
 ("M-o" 969)
 ("A" 913)
 ("B" 914)
 ("G" 915)
 ("D" 916)
 ("E" 917)
 ("Z" 918)
 ("H" 919)
 ("M-T" 920)
 ("I" 921)
 ("K" 922)
 ("L" 923)
 ("M" 924)
 ("N" 925)
 ("X" 926)
 ("O" 927)
 ("P" 928)
 ("R" 929)
 ("S" 931)
 ("T" 932)
 ("U" 933)
 ("Q" 934)
 ("M-X" 935)
 ("M-Q" 936)
 ("M-O" 937)
 ("-" 8212)
 )
(provide 'my-keys)
