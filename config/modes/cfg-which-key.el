;;; Package --- Summary
;;; which-key configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package which-key
  :demand
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(provide 'cfg-which-key)

;;; ————————————————————————————————————————————————————————
;;; cfg-which-key.el ends here
