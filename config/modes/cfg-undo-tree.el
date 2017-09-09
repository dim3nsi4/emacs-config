;;; Package --- Summary
;;; undo-tree configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 16:37:36 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package undo-tree
  :defer t

  :bind
  (:map undo-tree-map
        ("C-z" . undo)
        ("M-z" . undo-tree-redo))
  :config
  ;; (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t))

(provide 'cfg-undo-tree)

;;; ————————————————————————————————————————————————————————
;;; cfg-undo-tree.el ends here
