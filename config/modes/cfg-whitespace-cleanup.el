;;; Package --- Summary
;;; whitespace-cleanup configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
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
