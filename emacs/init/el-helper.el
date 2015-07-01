
(defvar el-get-packages nil "list of packages to be installed by el-get")

(defmacro el-register-package (&rest rst)
  `(let ((entry (quote ,rst)))
     (push entry el-get-sources)
     (push (el-get-source-name entry) el-get-packages)))

(defmacro el-use-package (name)
  `(push ,name el-get-packages))

(defun el-get-packages ()
  (el-get 'sync el-get-packages))

(provide 'el-helper)
