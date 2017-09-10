;;; Package --- Summary
;;; which-key configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-10 12:01:33 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package which-key
  :demand
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(provide 'cfg-which-key)

;;; ————————————————————————————————————————————————————————
;;; cfg-which-key.el ends here
