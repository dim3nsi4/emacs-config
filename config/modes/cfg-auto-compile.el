;;; Package --- Summary
;;; auto-compile configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 17:05:22 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package auto-compile
  :demand
  :config
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t)

  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(provide 'cfg-auto-compile)

;;; ————————————————————————————————————————————————————————
;;; cfg-auto-compile.el ends here
