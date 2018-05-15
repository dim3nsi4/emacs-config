;;; Package --- Summary
;;; drag-stuff configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package drag-stuff
  :bind
  (("C-«" . drag-stuff-left)
   ("C-»" . drag-stuff-right)
   ("C-+" . drag-stuff-up)
   ("C--" . drag-stuff-down)))

(provide 'cfg-drag-stuff)

;;; ————————————————————————————————————————————————————————
;;; cfg-drag-stuff.el ends here
