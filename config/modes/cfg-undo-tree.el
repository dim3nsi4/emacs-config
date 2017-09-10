;;; Package --- Summary
;;; undo-tree configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
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
