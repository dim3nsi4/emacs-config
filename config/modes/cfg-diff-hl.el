;;; Package --- Summary
;;; diff-hl configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package diff-hl
  :hook
  (dired-mode . diff-hl-dired-mode))

(provide 'cfg-diff-hl)

;;; ————————————————————————————————————————————————————————
;;; cfg-diff-hl.el ends here
