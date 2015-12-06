
(require 'hydra)
(require 'dash)

(defun char-left ()
  (interactive)
  (when (> (current-column) 0)
    (right-char -1)))

(defun char-right ()
  (interactive)
  (let ((start-column (current-column))
        (next-column (save-excursion (right-char 1) (current-column))))
    (cond ((> next-column start-column) (right-char 1))
          (t (insert " ")))))

(defun char-up ()
  (interactive)
  (let ((start-column (current-column)))
    (previous-line 1)
    (when (< (current-column) start-column)
      (insert (make-string (- start-column (current-column)) ? )))))

(defun char-down ()
  (interactive)
  (let ((start-column (current-column))
        (start-line (line-number-at-pos)))
    (next-line 1)
    (when (eq (line-number-at-pos) start-line)
      (end-of-line)
      (insert ?\C-j))
    (when (< (current-column) start-column)
      (insert (make-string (- start-column (current-column)) ? )))))

(defun insert-overwrite (string)
  (insert string)
  (delete-char (min (- (save-excursion (end-of-line) (point)) (point))
                    (length string))))

(defun rectangle-write (rectangle)
  (let ((lines rectangle)
        (start-column (current-column))
        (current-line))
    (push-mark)
    (while lines
      (insert-overwrite (pop lines))
      (when lines
        (setq current-line (line-number-at-pos))
        (forward-line 1)
        (when (eq (line-number-at-pos) current-line)
          (end-of-line)
          (insert ?\C-j))
        (move-to-column start-column t)
        (insert (make-string (- start-column (current-column)) ? ))))
  ))

(defun yank-overwrite-rectangle ()
  (interactive "*")
  (rectangle-write killed-rectangle))

(defun rectangle-move (start end fn)
  (let ((rectangle))
    (setq rectangle (extract-rectangle start end))
    (clear-rectangle start end)
    (goto-char (region-beginning))
    (funcall fn)
    (rectangle-write rectangle)))

(defun rectangle-left (start end)
  (interactive "r")
  (rectangle-move start end #'char-left))

(defun rectangle-right (start end)
  (interactive "r")
  (rectangle-move start end #'char-right))

(defun rectangle-up (start end)
  (interactive "r")
  (rectangle-move start end #'char-up))

(defun rectangle-down (start end)
  (interactive "r")
  (rectangle-move start end #'char-down))

(defhydra hydra-rectangle (ctl-x-map "r" :color pink :hint nil)
  "
^Killing^           ^Actions^           ^Movement^  ^Rectangle Movement^
^^^^^^^^^^^^------------------------------------------------------------
_e_: kill           _o_: open           _h_: left   _C-h_: move left
_E_: copy as kill   _i_: insert string  _t_: up     _C-t_: move up
_u_: yank           ^ ^                 _n_: down   _C-n_: move down
_U_: yank overwrite ^ ^                 _s_: right  _C-s_: move right
"

  ("q" nil "exit" :color blue)
  ("?" ignore "help" :color red)

  ("e" kill-rectangle)
  ("E" copy-rectangle-as-kill)
  ("u" yank-rectangle)
  ("U" yank-overwrite-rectangle)

  ("o" open-rectangle)
  ("i" string-insert-rectangle)

  ("h" char-left)
  ("t" char-up)
  ("n" char-down)
  ("s" char-right)

  ("C-h" rectangle-left)
  ("C-t" rectangle-up)
  ("C-n" rectangle-down)
  ("C-s" rectangle-right)
  )

(defvar-local hydra-indent-modal-function nil)



(defvar indentation-functions
  '(tab-to-tab-stop
    indent-relative
    indent-relative-maybe
    indent-to-left-margin))

(defun hydra-indent-set-modal-line-function ()
  (when (and indent-line-function
             (not (-contains-p indentation-functions indent-line-function))
             (not hydra-indent-modal-function))
    (setq hydra-indent-modal-function indent-line-function)))

(defun hydra-indent-set-modal-line-function-prompt (function)
  (interactive "aindent-line-function: ")
  (setq hydra-indent-modal-function function))

(defun hydra-indent-set-left-margin (margin)
  (interactive "nleft-margin: ")
  (unless (and (integerp margin)
               (> margin 0))
    (error "invalid left-margin %s" margin))
  (setq left-margin margin))

(defun hydra-indent-set-tab-width (width)
  (interactive "ntab-width: ")
  (unless (and (integerp width)
               (> width 0))
    (error "invalid tab-width %s" width))
  (setq tab-width width))

(defmacro hydra-set-indent-line-function (function)
  `(setq indent-line-function ,function))

(defhydra hydra-indent (ctl-x-map "i" :color pink :hint nil :pre hydra-indent-set-modal-line-function)
  "
^indent-line-function^                   ^margin^                                       ^fill^
^^^^-------------------------------------------------------------------------------------------------------------------
^ ^= % -26`indent-line-function          _m_: set buffer left-margin: % -17`left-margin _a_: fill-column: %`fill-column
_f_: % -33`hydra-indent-modal-function   _w_: set region left margin                    _o_: auto-fill-mode
_F_: set modal                           _W_: set region right margin                   _e_: fill-region
_g_: tab-to-tab-stop                     ^ ^                                            _E_: fill-region-as-paragraph
_c_: indent-relative                     ^ ^                                            _u_: fill-paragraph
_r_: indent-relative-maybe               ^ ^                                            _U_: fill-individual-paragraphs
_l_: indent-to-left-margin
_h_: electric-indent-mode: %`electric-indent-mode
_t_: indent-tabs-mode: %`indent-tabs-mode
_T_: tab-width: %`tab-width
_n_: tab-always-indent: %`tab-always-indent
"
  ("q" nil "exit" :color blue)
  ("?" ignore "help" :color red)

  ("f" (hydra-set-indent-line-function hydra-indent-modal-function))
  ("F" hydra-indent-set-modal-line-function-prompt)
  ("g" (hydra-set-indent-line-function 'tab-to-tab-stop))
  ("c" (hydra-set-indent-line-function 'indent-relative))
  ("r" (hydra-set-indent-line-function 'indent-relative-maybe))
  ("l" (hydra-set-indent-line-function 'indent-to-left-margin))

  ("h" electric-indent-mode)
  ("t" (setq indent-tabs-mode (not indent-tabs-mode)))
  ("T" hydra-indent-set-tab-width)
  ("n" (setq tab-always-indent (not tab-always-indent)))

  ("m" hydra-indent-set-left-margin)
  ("w" set-left-margin)
  ("W" set-right-margin)

  ("a" set-fill-column)
  ("o" auto-fill-mode)
  ("e" fill-region)
  ("E" fill-region-as-paragraph)
  ("u" fill-paragraph)
  ("U" fill-individual-paragraphs)
  )





(provide 'my-hydra)
