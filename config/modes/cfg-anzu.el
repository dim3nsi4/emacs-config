;;; Package --- Summary
;;; anzu configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 16:06:58 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package anzu
  :demand

  :bind
  (("<f5>"   . anzu-query-replace)
   ("<S-f5>" . anzu-query-replace-regexp))

  :config
  (global-anzu-mode 1))

(provide 'cfg-anzu)

;;; ————————————————————————————————————————————————————————
;;; cfg-anzu.el ends here
