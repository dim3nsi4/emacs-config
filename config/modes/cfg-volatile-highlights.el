;;; Package --- Summary
;;; volatile-highlights configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 16:40:59 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package volatile-highlights
  :defer t
  :diminish volatile-highlights-mode

  :config
  (volatile-highlights-mode t))

(provide 'cfg-volatile-highlights)

;;; ————————————————————————————————————————————————————————
;;; cfg-volatile-highlights.el ends here
