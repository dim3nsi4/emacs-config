;;; Package --- Summary
;;; doc-view configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package doc-view
  :magic
  ("%PDF" . pdf-view-mode)

  :config
  (setq doc-view-continuous t
        doc-view-resolution 200))

(provide 'cfg-doc-view)

;;; ————————————————————————————————————————————————————————
;;; cfg-doc-view.el ends here
