
(defun user-subdir (dir)
  (concat (file-name-as-directory user-emacs-directory) dir))
(add-to-list 'load-path (user-subdir "init"))
(require 'config)

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                          ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(add-to-list 'load-path (user-subdir "el-get/el-get"))
(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(require 'utils)

(require 'my-keys)
(my-keys-process-bindings)
(my-keys-apply-global)

(require 'el-helper)
(require 'my-modes)

(el-get-packages)
