(defconst original-global-map (copy-keymap global-map))

(defvar my-keys-bindings nil "alist of defined key->command bindings")
(defvar my-keys-remaps nil "alist of defined old->new remaps")
(defvar my-keys-calc-remaps nil "alist of defined and inferred old->new remaps")
(defvar my-keys-calc-shadowed nil "alist of shadowed global bindings")
(defvar my-keys-calc-bindings nil "alist of defined and inferred global bindings")
(defvar my-keys-remapped-hash (make-hash-table :test 'equal))

(setq my-keys-bindings nil)
(setq my-keys-remaps nil)

(defvar my-keys-minor-mode-map (make-keymap))

(defun my-map-set-key (key command)
  (define-key my-keys-minor-mode-map key command))

(defun my-map-bind-key (key command)
  (push (cons key command) my-keys-bindings))
(defun my-map-remap-key (old new)
  (push (cons old new) my-keys-remaps))

(defun debug-princ (x) nil)

(defun my-keys-process-bindings ()
  (setq my-keys-calc-remaps my-keys-remaps)
  (setq my-keys-calc-shadowed nil)
  (setq my-keys-calc-bindings my-keys-bindings)
  ;; For each binding in my-keys-bindings, see if it already has a binding in global-map
  ;; If so, save this as a remap association.
  (dolist (b my-keys-bindings)
    (if (cdr b)
        (dolist (k (where-is-internal (cdr b) original-global-map))
          (if (not (assoc k my-keys-calc-remaps))
              (push (cons k (car b)) my-keys-calc-remaps)))))
  ;; For each binding in my-keys-bindings, see what existing global binding (if any) it overrides.
  ;; If a overidden binding is not rebound, add it to my-keys-calc-shadowed
  (dolist (b my-keys-bindings)
    (let ((f (lookup-key original-global-map (car b))))
      (cond
       ;; It doesn't shadow an existing binding
       ((not f))
       ;; It shadows a prefix
       ((numberp f) (push (cons (car b) (lookup-key original-global-map (substring (car b) 0 f))) my-keys-calc-shadowed))
       ;; We've rebound it explicitly
       ((rassoc f my-keys-bindings))
       ;; It's been remapped
       ((assoc (car b) my-keys-calc-remaps))
       ;; It's been shadowed
       (t (push (cons (car b) f) my-keys-calc-shadowed)))))
  (dolist (b my-keys-remaps)
    (let ((f (lookup-key original-global-map (car b))))
      (cond
       ((not f))
       ((numberp f))
       (t (push (cons (cdr b) f) my-keys-calc-bindings))))))

(defun my-keys-remap-keymap (m)
  (if m
      (let ((rebound))
        (dolist (b my-keys-calc-remaps)
          (let ((f (lookup-key m (car b))))
            (if f (progn
                    (push (cons (cdr b) f) rebound)
                    (define-key m (car b) nil)))))
        (dolist (b my-keys-bindings)
          (define-key m (car b) nil))
        (dolist (b rebound)
          (define-key m (car b) (cdr b))))))

(defun my-keys-apply-global ()
  (my-keys-remap-keymap global-map)
  (dolist (b my-keys-bindings)
    (define-key global-map (car b) (cdr b))))

(defun my-keys-remap-minor-modes ()
  (debug-princ "my-keys-remap-minor-modes\n")
  (dolist (a minor-mode-map-alist)
    (debug-princ (car a))
    (debug-princ "\n")
    (if (not (get (car a) 'my-keys-remapped))
        (progn
          (my-keys-remap-keymap (cdr a))
          (put (car a) 'my-keys-remapped t))))
  (debug-princ "my-keys-remap-minor-modes done\n"))

(defun my-keys-remap-current-major-mode ()
  (debug-princ "my-keys-remap-current-major-mode: ")
  (debug-princ major-mode)
  (debug-princ "\n")
  (if (not (get major-mode 'my-keys-remapped))
      (progn
        (my-keys-remap-keymap (current-local-map))
        (put major-mode 'my-keys-remapped t)))
  (debug-princ "my-keys-remap-current-major-mode done\n"))

(defun my-keys-remap-minibuffer ()
  (mapc #'my-keys-remap-keymap
        '(minibuffer-local-map
          minibuffer-local-ns-map
          minibuffer-local-completion-map
          minibuffer-local-filename-completion-map)))

(defun safe-undefine-key (m k)
  (let ((l (lookup-key m k)))
    (cond
     ;; key is too long, undefine the appropriate prefix
     ((numberp l) (define-key m (substring k 0 l) nil))
     ;; key is already undefined
     ((not l))
     ;; undefine the key
     (t (define-key m k nil)))))

(defun my-keys-remap-mode-internal (m &optional defb)
  (let* ((rebound)
         (conflicts)
         (mv (symbol-value m))
         (mp (keymap-parent mv)))

    (dolist (b my-keys-calc-remaps)
      (let* ((fm (lookup-key mv (car b)))
             (fmp (if (and mp (not (numberp fm))) (lookup-key mp (car b))))
             (prev (if (and fm (not (numberp fm))) (gethash fm my-keys-remapped-hash))))
        (cond
         ((not fm))
         ((numberp fm))
         (fmp)
         ((member (car b) prev))
         ((equal (car b) (cdr b)))
         (t (puthash fm (push (car b) prev) my-keys-remapped-hash)
            (push (list (car b) (cdr b) fm) rebound)
            (safe-undefine-key mv (car b))))))
    (dolist (b my-keys-calc-bindings)
      (let* ((fm (lookup-key mv (car b)))
             (k (if (numberp fm) (substring (car b) 0 fm) (car b)))
             (fmv (lookup-key mv k))
             (fmp (if mp (lookup-key mp k))))
        (cond
          ((not fmv))
          (fmp (push (list '*km/pnt*-mk (car b) fmv (cdr b)) conflicts))
          ((and (not (numberp fm)) (equal fmv (cdr b))))
          (t ;;implicit binding is overriden by the global map
           (safe-undefine-key mv (car b))
           (push (list 'km-*mk* (car b) fmv (cdr b)) conflicts)))))
    (dolist (b rebound)
      (define-key mv (nth 1 b) (nth 2 b)))
    (dolist (b defb)
      (let* ((fm (lookup-key mv (car b)))
            (fmv (if (numberp fm) (lookup-key mv (substring (car b) 0 fm)) fm))
            (fg (rassoc (car b) my-keys-calc-bindings)))
        (if (numberp fm)
            (safe-undefine-key mv (car b)))
        (if (and fmv (or (numberp fm) (not (equal (cdr b) fm))))
            (push (list '*def*-km (car b) (cdr b) fmv) conflicts))
        (if (and fg (not (equal (cdr b) fg)))
            (push (list '*def*-mk (car b) (cdr b) fg) conflicts)))
      (define-key mv (car b) (cdr b)))
    (put m 'my-keys-rebound (push rebound (get m 'my-keys-rebound)))
    (put m 'my-keys-conflicts conflicts)))

(defmacro my-keys-remap-mode (m &optional defb)
  `(my-keys-remap-mode-internal ,m
                                (mapcar (lambda (b) (cons (kbd (car b)) (cdr b)))
                                        ,defb)))

(provide 'my-keys-defns)
