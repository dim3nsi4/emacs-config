;;; Package --- Summary
;;; diminish configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
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
