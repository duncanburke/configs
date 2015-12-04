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

(defun enable-debug ()
  (setq debug-on-error t)
  (setq edebug-all-defs t))

(defmacro add-hook-anon (hook &rest rst)
  `(add-hook ,hook
             ,(append (lambda ()) rst)))

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

(defun kill-nearby-line (&optional backward)
  (interactive)
  (let ((line-start (save-excursion (beginning-of-line) (point)))
        (kill-whole-line t))
    (save-excursion
      (forward-line (cond (backward -1)
                          (t         1)))
      (unless (eq line-start
                  (point))
        (kill-line)))))

(defun kill-previous-line ()
  (interactive)
  (kill-nearby-line t))

(defun kill-next-line ()
  (interactive)
  (kill-nearby-line nil))

(defun switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(provide 'utils)
