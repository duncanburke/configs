(defconst dot-emacs (concat (getenv "HOME") "/" ".emacs_main.el"))

(require 'bytecomp)
(setq compiled-dot-emacs (byte-compile-dest-file dot-emacs))

(if (or (not (file-exists-p compiled-dot-emacs))
	(file-newer-than-file-p dot-emacs compiled-dot-emacs)
	(equal (nth 4 (file-attributes dot-emacs)) (list 0 0)))
    (if (byte-compile-file dot-emacs)
	(message compiled-dot-emacs " recompiled")
      (error "compilation failed")))

(load compiled-dot-emacs)
