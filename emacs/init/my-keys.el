(require 'my-keys-defns)

;; Rebind key prefixes we're going to use
(my-map-remap-key "C-h" "C-.")
(my-map-remap-key "C-u" "C-p")
(my-map-remap-key "C-c" "C-u")
(my-map-remap-key "C-q" "C-e")
(my-map-remap-key "C-g" "C-,")
(my-map-remap-key "C-z" "C-b")
(my-map-remap-key "M-o" "M-'")
(my-map-remap-key "M-p" "M-t")
;; This is just a funny coincidence...
(my-map-remap-key "M-n" "M-n")

;; Cursor Movement Bindings
(my-map-bind-key "C-h" 'backward-char)
(my-map-bind-key "C-t" 'previous-line)
(my-map-bind-key "C-n" 'next-line)
(my-map-bind-key "C-s" 'forward-char)

(my-map-bind-key "M-h" 'left-word)
(my-map-bind-key "M-t" 'backward-paragraph)
(my-map-bind-key "M-n" 'forward-paragraph)
(my-map-bind-key "M-s" 'right-word)

(my-map-bind-key "M-H" 'beginning-of-line)
(my-map-bind-key "M-T" 'scroll-down-command)
(my-map-bind-key "M-N" 'scroll-up-command)
(my-map-bind-key "M-S" 'end-of-line)

;; Kill Bindings
(my-map-bind-key "C-g" 'backward-delete-char)
(my-map-bind-key "C-c" 'nil)
(my-map-bind-key "C-r" 'nil)
(my-map-bind-key "C-l" 'delete-char)

(my-map-bind-key "M-g" 'backward-kill-word)
(my-map-bind-key "M-c" 'backward-kill-paragraph)
(my-map-bind-key "M-r" 'kill-paragraph)
(my-map-bind-key "M-l" 'kill-word)

(my-map-bind-key "M-G" 'backward-kill-line)
(my-map-bind-key "M-C" 'nil)
(my-map-bind-key "M-R" 'nil)
(my-map-bind-key "M-L" 'kill-line)

;; Bindings for window movement
(my-map-bind-key "C-M-h" 'windmove-left)
(my-map-bind-key "C-M-t" 'windmove-up)
(my-map-bind-key "C-M-n" 'windmove-down)
(my-map-bind-key "C-M-s" 'windmove-right)


(my-map-bind-key "C-a" 'isearch-backward)
(my-map-bind-key "C-o" 'isearch-forward)
(my-map-bind-key "M-a" 'isearch-backward-regexp)
(my-map-bind-key "M-o" 'isearch-forward-regexp)
(my-map-bind-key "C-;" 'query-replace)
(my-map-bind-key "C-q" 'query-replace-regexp)
(my-map-bind-key "M-;" 'replace-string)
(my-map-bind-key "M-q" 'replace-regexp)

;; Alternate binding for M-x
(my-map-bind-key "C-x RET" 'execute-extended-command)

(my-map-bind-key "C-b x" 'close-and-kill-next-pane)
(my-map-bind-key "C-b z" 'close-and-kill-this-pane)

;; Keybindings to the X clipboard
(my-map-bind-key "s-u" 'clipboard-yank)
(my-map-bind-key "s-e" 'clipboard-kill-ring-save)
(my-map-bind-key "C-u v" 'clipboard-yank)
(my-map-bind-key "C-u c" 'clipboard-kill-ring-save)

;; Delete trailing whitespace
(my-map-bind-key "C-x t" 'delete-trailing-whitespace)

;; Line wrap at right edge of screen
(my-map-bind-key "C-u t" 'toggle-truncate-lines)

;; Line numbers at left edge of screen
(my-map-bind-key "C-u l" 'linum-mode)

;; replace buff-menu with bs-show
(my-map-bind-key "C-x C-b" 'bs-show)

;; Show-hide menu
(my-map-bind-key "C-x y" 'menu-bar-mode)

;; Show whitespace
(my-map-bind-key "C-u w" 'whitespace-mode)

;; Jump to the specified line number
(my-map-bind-key "C-u a" 'goto-line)

(my-map-bind-key "C-'" 'comment-region)
(my-map-bind-key "M-'" 'uncomment-region)

(my-map-bind-key "C-u x" 'text-mode)

(my-map-bind-key "C-u g" 'magit-status)



;; Disable C-z. Normally, this would cause it to be minimised in a graphical environment, but it gets
;; confused with xmonad
(my-map-unbind-key "C-z")

(my-map-unbind-key "C-n")
(my-map-unbind-key "C-p")
(my-map-unbind-key "C-f")
(my-map-unbind-key "C-b")
(my-map-unbind-key "C-a")
(my-map-unbind-key "C-e")

(my-map-unbind-key "<up>")
(my-map-unbind-key "<down>")
(my-map-unbind-key "<left>")
(my-map-unbind-key "<right>")
(my-map-unbind-key "<prior>")
(my-map-unbind-key "<next>")

(my-map-unbind-key "M-n")
(my-map-unbind-key "M-p")
(my-map-unbind-key "M-f")
(my-map-unbind-key "M-b")
(my-map-unbind-key "M-a")
(my-map-unbind-key "M-e")

(my-map-unbind-key "<C-up>")
(my-map-unbind-key "<C-down>")
(my-map-unbind-key "<C-left>")
(my-map-unbind-key "<C-right>")

(my-map-unbind-key "<M-up>")
(my-map-unbind-key "<M-down>")
(my-map-unbind-key "<M-left>")
(my-map-unbind-key "<M-right>")

(my-map-unbind-key "DEL")
(my-map-unbind-key "<deletechar>")
(my-map-unbind-key "<C-backspace>")
(my-map-unbind-key "<M-backspace>")

(my-map-unbind-key "C-x o")
(my-map-unbind-key "M-x")


(provide 'my-keys)
