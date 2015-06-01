(add-to-list 'load-path "~/.emacs.d/init/")
(require 'config)

(require 'package)
(package-initialize)
(require 'use-package)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                          ("gnu" . "http://elpa.gnu.org/packages/")))

(use-package subatomic256-theme)

(load-theme 'subatomic256 t)

(require 'utils)

(require 'my-keys)
(my-keys-process-bindings)
(my-keys-apply-global)

(require 'my-modes)
