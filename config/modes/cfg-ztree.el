;;; Package --- Summary
;;; ztree configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package ztree-diff
  :init
  (add-hook 'ztree-mode-hook '(lambda() (setq show-trailing-whitespace nil)))

  :config
  (setq-default ztree-diff-filter-list (cons "^.*\\.pyc" ztree-diff-filter-list))
  (setq-default ztree-diff-filter-list (cons "^.*\\.elc" ztree-diff-filter-list)))

(use-package ztree-dir
  :init
  (add-hook 'ztree-mode-hook '(lambda() (setq show-trailing-whitespace nil)))

  :config
  (setq-default ztree-dir-filter-list (cons "^.*\\.pyc" ztree-dir-filter-list))
  (setq-default ztree-dir-filter-list (cons "^.*\\.elc" ztree-dir-filter-list)))

(provide 'cfg-ztree)

;;; ————————————————————————————————————————————————————————
;;; cfg-ztree.el ends here
