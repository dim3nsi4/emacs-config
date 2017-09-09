;;; Package --- Summary
;;; doc-view configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 16:38:32 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package doc-view
  :defer t
  :config
  (setq doc-view-continuous t
        doc-view-resolution 200))

(provide 'cfg-doc-view)

;;; ————————————————————————————————————————————————————————
;;; cfg-doc-view.el ends here
