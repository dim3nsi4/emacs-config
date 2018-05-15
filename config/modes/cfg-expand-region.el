;;; Package --- Summary
;;; expand-region configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C-%" . er/contract-region)))

(provide 'cfg-expand-region)

;;; ————————————————————————————————————————————————————————
;;; cfg-expand-region.el ends here
