;;; Package --- Summary
;;; which-key configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 19:18:17 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package which-key
  :demand
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

(provide 'cfg-which-key)

;;; ————————————————————————————————————————————————————————
;;; cfg-which-key.el ends here
