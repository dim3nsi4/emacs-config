;;; Package --- Summary
;;; google-c-style configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 16:47:09 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package google-c-style
  :defer t
  :init
  (add-hook 'java-mode-hook     'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-set-c-style))

(provide 'cfg-google-c-style)

;;; ————————————————————————————————————————————————————————
;;; cfg-google-c-style.el ends here
