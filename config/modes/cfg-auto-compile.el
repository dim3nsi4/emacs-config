;;; Package --- Summary
;;; auto-compile configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package auto-compile
  :demand

  :defines
  (auto-compile-display-buffer
   auto-compile-mode-line-counter)

  :commands
  (auto-compile-on-load-mode
   auto-compile-on-save-mode)

  :config
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t)

  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(provide 'cfg-auto-compile)

;;; ————————————————————————————————————————————————————————
;;; cfg-auto-compile.el ends here
