;;; Package --- Summary
;;; winner configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

;; Undo/Redo window configuration changes (C-c left, C-c right)
(use-package winner
  :commands
  (winner-mode)

  :config
  (winner-mode t))

(provide 'cfg-winner)

;;; ————————————————————————————————————————————————————————
;;; cfg-winner.el ends here
