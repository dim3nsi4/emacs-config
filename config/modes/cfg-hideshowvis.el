;;; Package --- Summary
;;; hideshowvis configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package hideshowvis
  :init
  (add-hook 'emacs-lisp-mode-hook 'hideshowvis-enable))

(provide 'cfg-hideshowvis)

;;; ————————————————————————————————————————————————————————
;;; cfg-hideshowvis.el ends here
