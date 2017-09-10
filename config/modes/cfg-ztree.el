;;; Package --- Summary
;;; ztree configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package ztree
  :defer t
  :init
  (add-hook 'ztree-mode-hook '(lambda() (setq show-trailing-whitespace nil)))

  :config
  (push (substitute-in-file-name "path-to-ztree-directory") load-path))


(provide 'cfg-ztree)

;;; ————————————————————————————————————————————————————————
;;; cfg-ztree.el ends here
