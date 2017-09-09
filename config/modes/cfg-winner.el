;;; Package --- Summary
;;; winner configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 15:33:13 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

;; Undo/Redo window configuration changes (C-c left, C-c right)
(use-package winner
  :defer t
  :config
  (winner-mode t))

(provide 'cfg-winner)

;;; ————————————————————————————————————————————————————————
;;; cfg-winner.el ends here
