(require 'cl-macs)
(require 'cl-extra)

;; Often things will open new windows for some reason. These can be useful, but one can be
;; left with a multitude of useless buffers lying around. Rather than just switching away from the offending
;; buffer, use C-z z. C-z x is somewhat unpredictable, as one isn't sure exactly which other buffer it's going
;; to close; so it's best used when there are two panes.
(defun close-and-kill-next-pane ()
  "Close the other pane and kill the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-buffer)
  (delete-window))

(defun close-and-kill-this-pane ()
  "Close this pane and kill the buffer in it also."
  (interactive)
  (kill-buffer)
  (delete-window))

(defun backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0))

(defun diff-region ()
  "Select a region to compare"
  (interactive)
  (when (use-region-p) ; there is a region
    (let (buf)
      (setq buf (get-buffer-create "*Diff-regionA*"))
      (save-current-buffer
        (set-buffer buf)
        (erase-buffer))
      (append-to-buffer buf (region-beginning) (region-end))))
  (message "Now select other region to compare and run `diff-region-now`"))

(defun diff-region-now ()
  "Compare current region with region already selected by `diff-region`"
  (interactive)
  (when (use-region-p)
    (let (bufa bufb)
      (setq bufa (get-buffer-create "*Diff-regionA*"))
      (setq bufb (get-buffer-create "*Diff-regionB*"))
      (save-current-buffer
        (set-buffer bufb)
        (erase-buffer))
      (append-to-buffer bufb (region-beginning) (region-end))
      (ediff-buffers bufa bufb))))

(defvar my-offset 4 "My indentation offset. ")
(defun backspace-whitespace-to-tab-stop ()
  "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
  (interactive)
  (if (or indent-tabs-mode
          (region-active-p)
          (save-excursion
            (> (point) (progn (back-to-indentation)
                              (point)))))
      (call-interactively 'backward-delete-char-untabify)
    (let ((movement (% (current-column) my-offset))
          (p (point)))
      (when (= movement 0) (setq movement my-offset))
      ;; Account for edge case near beginning of buffer
      (setq movement (min (- p 1) movement))
      (save-match-data
        (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char (- (match-end 1) (match-beginning 1)))
          (call-interactively 'backward-delete-char))))))


(defun enable-debug ()
  (setq debug-on-error t)
  (setq edebug-all-defs t))

(defmacro add-hook-anon (hook &rest rst)
  `(add-hook ,hook
             ,(append (lambda ()) rst)))

(defun tabstop-hook ()
  (define-key (current-local-map) (kbd "TAB") 'tab-to-tab-stop)
  (define-key (current-local-map) (kbd "<backspace>") 'delete-to-tabstop)
  (setq tab-width 4
        indent-tabs-mode nil
        tab-stop-list (number-sequence 4 200 4)))

(defun delete-to-tabstop ()
  (interactive)
  (let ((prev-tabstop (indent-next-tab-stop (current-column) t))
        (prev-whitespace (lambda () (or (eq (preceding-char) ?\s)
                                        (eq (preceding-char) ?\t))))
        )
    (if (funcall prev-whitespace)
        (while (and
                (> (current-column) prev-tabstop)
                (funcall prev-whitespace))
          (delete-backward-char 1))
      (delete-backward-char 1))))

(defun linum-hook ()
  (linum-mode))

(defun mode-extension (mode extension)
  (add-to-list 'auto-mode-alist `(,(concat "\\" extension "$") . ,mode)))

(defmacro try-fn (fn &rest args)
  `(if (symbol-function (quote ,fn))
       (,fn ,@args)))

(defun force-define-key (keymap key def)
  (let ((lookup (lookup-key keymap key)))
    (when (numberp lookup)
      (define-key keymap (cl-subseq key 0 lookup) nil))
    (define-key keymap key def)))

(defmacro keymap-define (keymap &rest bindings)
  `(progn ,@(cl-mapcar
       (lambda (binding)
         `(force-define-key ,keymap ,(car binding) ,(cadr binding))
         )
       bindings)
    ))

(defmacro keymap-define-kbd (keymap &rest bindings)
  `(keymap-define
    ,keymap
    ,@(cl-mapcar
       (lambda (binding)
         `(,(kbd (car binding)) ,(cadr binding)))
       bindings)))

(provide 'utils)
