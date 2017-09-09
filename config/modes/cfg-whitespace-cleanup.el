;;; Package --- Summary
;;; whitespace-cleanup configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 15:30:54 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

;; Cleanup whitespace if the file was originally clean
(use-package whitespace-cleanup-mode
  :demand
  :diminish whitespace-cleanup-mode

  :config
  (global-whitespace-cleanup-mode))

(provide 'cfg-whitespace-cleanup)

;;; ————————————————————————————————————————————————————————
;;; cfg-whitespace-cleanup.el ends here
