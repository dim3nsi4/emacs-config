;;; Package --- Summary
;;; diminish configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 16:45:44 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package diminish
  :defer t
  :config
  (diminish 'abbrev-mode)
  (diminish 'eldoc-mode))

(provide 'cfg-diminish)

;;; ————————————————————————————————————————————————————————
;;; cfg-diminish.el ends here
