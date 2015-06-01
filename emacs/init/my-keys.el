(require 'my-keys-defns)

;; Rebind key prefixes we're going to use
(my-map-remap-key (kbd "C-h") (kbd "C-."))
(my-map-remap-key (kbd "C-u") (kbd "C-p"))
(my-map-remap-key (kbd "C-c") (kbd "C-u"))
(my-map-remap-key (kbd "C-q") (kbd "C-e"))
(my-map-remap-key (kbd "C-g") (kbd "C-,"))
(my-map-remap-key (kbd "C-z") (kbd "C-b"))
(my-map-remap-key (kbd "M-o") (kbd "M-'"))
(my-map-remap-key (kbd "M-p") (kbd "M-t"))
;; This is just a funny coincidence...
(my-map-remap-key (kbd "M-n") (kbd "M-n"))

;; Cursor Movement Bindings
(my-map-bind-key (kbd "C-h") 'backward-char)
(my-map-bind-key (kbd "C-t") 'previous-line)
(my-map-bind-key (kbd "C-n") 'next-line)
(my-map-bind-key (kbd "C-s") 'forward-char)

(my-map-bind-key (kbd "M-h") 'left-word)
(my-map-bind-key (kbd "M-t") 'backward-paragraph)
(my-map-bind-key (kbd "M-n") 'forward-paragraph)
(my-map-bind-key (kbd "M-s") 'right-word)

(my-map-bind-key (kbd "M-H") 'beginning-of-line)
(my-map-bind-key (kbd "M-T") 'scroll-down-command)
(my-map-bind-key (kbd "M-N") 'scroll-up-command)
(my-map-bind-key (kbd "M-S") 'end-of-line)

;; Kill Bindings
(my-map-bind-key (kbd "C-g") 'backward-delete-char)
(my-map-bind-key (kbd "C-c") 'nil)
(my-map-bind-key (kbd "C-r") 'nil)
(my-map-bind-key (kbd "C-l") 'delete-char)

(my-map-bind-key (kbd "M-g") 'backward-kill-word)
(my-map-bind-key (kbd "M-c") 'backward-kill-paragraph)
(my-map-bind-key (kbd "M-r") 'kill-paragraph)
(my-map-bind-key (kbd "M-l") 'kill-word)

(my-map-bind-key (kbd "M-G") 'backward-kill-line)
(my-map-bind-key (kbd "M-C") 'nil)
(my-map-bind-key (kbd "M-R") 'nil)
(my-map-bind-key (kbd "M-L") 'kill-line)

;; Bindings for window movement
(my-map-bind-key (kbd "C-M-h") 'windmove-left)
(my-map-bind-key (kbd "C-M-t") 'windmove-up)
(my-map-bind-key (kbd "C-M-n") 'windmove-down)
(my-map-bind-key (kbd "C-M-s") 'windmove-right)


(my-map-bind-key (kbd "C-a") 'isearch-backward)
(my-map-bind-key (kbd "C-o") 'isearch-forward)
(my-map-bind-key (kbd "M-a") 'isearch-backward-regexp)
(my-map-bind-key (kbd "M-o") 'isearch-forward-regexp)
(my-map-bind-key (kbd "C-;") 'query-replace)
(my-map-bind-key (kbd "C-q") 'query-replace-regexp)
(my-map-bind-key (kbd "M-;") 'replace-string)
(my-map-bind-key (kbd "M-q") 'replace-regexp)

;; Alternate binding for M-x
(my-map-bind-key (kbd "C-x RET") 'execute-extended-command)

(my-map-bind-key (kbd "C-b x") 'close-and-kill-next-pane)
(my-map-bind-key (kbd "C-b z") 'close-and-kill-this-pane)

;; Keybindings to the X clipboard
(my-map-bind-key (kbd "s-u") 'clipboard-yank)
(my-map-bind-key (kbd "s-e") 'clipboard-kill-ring-save)
(my-map-bind-key (kbd "C-u v") 'clipboard-yank)
(my-map-bind-key (kbd "C-u c") 'clipboard-kill-ring-save)

;; Delete trailing whitespace
(my-map-bind-key (kbd "C-x t") 'delete-trailing-whitespace)

;; Line wrap at right edge of screen
(my-map-bind-key (kbd "C-u t") 'toggle-truncate-lines)

;; Line numbers at left edge of screen
(my-map-bind-key (kbd "C-u l") 'linum-mode)

;; replace buff-menu with bs-show
(my-map-bind-key (kbd "C-x C-b") 'bs-show)

;; Show-hide menu
(my-map-bind-key (kbd "C-x y") 'menu-bar-mode)

;; Show whitespace
(my-map-bind-key (kbd "C-u w") 'whitespace-mode)

;; Jump to the specified line number
(my-map-bind-key (kbd "C-u a") 'goto-line)

(my-map-bind-key (kbd "C-'") 'comment-region)
(my-map-bind-key (kbd "M-'") 'uncomment-region)

(my-map-bind-key (kbd "C-u x") 'text-mode)

(my-map-bind-key (kbd "C-u g") 'magit-status)



;; Disable C-z. Normally, this would cause it to be minimised in a graphical environment, but it gets
;; confused with xmonad
(global-unset-key (kbd "C-z"))

;; (global-unset-key (kbd "C-n"))
;; (global-unset-key (kbd "C-p"))
;; (global-unset-key (kbd "C-f"))
;; (global-unset-key (kbd "C-b"))
;; (global-unset-key (kbd "C-a"))
;; (global-unset-key (kbd "C-e"))

;; (global-unset-key (kbd "<up>"))
;; (global-unset-key (kbd "<down>"))
;; (global-unset-key (kbd "<left>"))
;; (global-unset-key (kbd "<right>"))
;; (global-unset-key (kbd "<prior>"))
;; (global-unset-key (kbd "<next>"))

;; (global-unset-key (kbd "M-n"))
;; (global-unset-key (kbd "M-p"))
;; (global-unset-key (kbd "M-f"))
;; (global-unset-key (kbd "M-b"))
;; (global-unset-key (kbd "M-a"))
;; (global-unset-key (kbd "M-e"))

;; (global-unset-key (kbd "<C-up>"))
;; (global-unset-key (kbd "<C-down>"))
;; (global-unset-key (kbd "<C-left>"))
;; (global-unset-key (kbd "<C-right>"))

;; (global-unset-key (kbd "<M-up>"))
;; (global-unset-key (kbd "<M-down>"))
;; (global-unset-key (kbd "<M-left>"))
;; (global-unset-key (kbd "<M-right>"))

;; (global-unset-key (kbd "<backspace>"))
;; (global-unset-key (kbd "DEL"))
;; (global-unset-key (kbd "<deletechar>"))
;; (global-unset-key (kbd "<C-backspace>"))
;; (global-unset-key (kbd "<M-backspace>"))

;; (global-unset-key (kbd "C-x o"))
;; (global-unset-key (kbd "M-x"))


(provide 'my-keys)
