;;; Package --- Summary
;;; expand-region configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 15:36:26 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package expand-region
  :defer t
  :bind
  (("C-=" . er/expand-region)
   ("C-%" . er/contract-region)))

(provide 'cfg-expand-region)

;;; ————————————————————————————————————————————————————————
;;; cfg-expand-region.el ends here
