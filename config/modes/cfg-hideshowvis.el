;;; Package --- Summary
;;; hideshowvis configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 19:28:34 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package hideshowvis
  :init
  (add-hook 'emacs-lisp-mode-hook 'hideshowvis-enable))

(provide 'cfg-hideshowvis)

;;; ————————————————————————————————————————————————————————
;;; cfg-hideshowvis.el ends here
