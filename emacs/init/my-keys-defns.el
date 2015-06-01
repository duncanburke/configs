(defconst original-global-map (copy-keymap global-map))

(defvar my-keys-bindings nil "alist of defined key->command bindings")
(defvar my-keys-remaps nil "alist of defined old->new remaps")
(defvar my-keys-calc-remaps nil "alist of defined and inferred old->new remaps")
(defvar my-keys-calc-shadowed nil "alist of shadowed global bindings")
(defvar my-keys-remapped-hash (make-hash-table :test 'eq))

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
  ;; For each binding in my-keys-bindings, see if it already has a binding in global-map
  ;; If so, save this as a remap association.
  (dolist (b my-keys-bindings)
    (if (cdr b)
        (dolist (k (where-is-internal (cdr b) original-global-map))
          (if (not (assoc k my-keys-calc-remaps))
              (push (cons k (car b)) my-keys-calc-remaps)))))
  ;; For each binding in my-keys-bindings, see what existing global binding (if any) it overrides.
  ;; If a overriden binding is not rebound, add it to my-keys-calc-shadowed
  (dolist (b my-keys-bindings)
    (let ((f (lookup-key original-global-map (car b))))
      (cond
       ;; It didn't shadow an existing binding
       ((not f) nil)
       ;; We've rebound it explicitly
       ((rassoc f my-keys-bindings) nil)
       ;; It's been remapped
       ((assoc (car b) my-keys-calc-remaps) nil)
       ;; It's been shadowed
       (t (push (cons (car b) f) my-keys-calc-shadowed))))))

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

(defun my-keys-remap-mode (m &optional defb)
  (let ((rebound)
        (conflicts)
        (mv (symbol-value m)))

    (dolist (b my-keys-calc-remaps)
      (let ((fm (lookup-key mv (car b))))
        (if fm
            (let ((prev-remapped (gethash fm my-keys-remapped-hash)))
              ;; Don't remap if we've already remapped *to* this binding
              (if (not (memq (car b) prev-remapped))
                  (progn
                    (puthash fm (push (car b) prev-remapped) my-keys-remapped-hash)
                    (push (list (car b) (cdr b) fm) rebound)
                    (define-key mv (car b) nil)))))))
    (dolist (b my-keys-bindings)
      (let ((fm (lookup-key mv (car b))))
        (if fm (progn
                 ;; An implicit binding is overriden by the global map
                 (define-key mv (car b) nil)
                 (push (list 'km-*mk* (car b) fm (cdr b)) conflicts)))))
    (princ rebound)
    (dolist (b rebound)
      (define-key mv (nth 1 b) (nth 2 b)))
    (dolist (b defb)
      (let ((fm (lookup-key mv (car b)))
            (fg (rassq (car b) my-keys-bindings)))
        (if fm (push (list '*def*-km (car b) (cdr b) fm) conflicts))
        (if fg (push (list '*def*-mk (car b) (cdr b) fg) conflicts)))
      (define-key mv (car b) (cdr b)))
    (put m 'my-keys-rebound rebound)
    (put m 'my-keys-conflicts conflicts)))

(provide 'my-keys-defns)
