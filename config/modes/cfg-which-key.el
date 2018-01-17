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

  :commands
  (which-key-setup-side-window-bottom
   which-key-mode)

  :config
  (setq which-key-idle-delay 1.0
        which-key-sort-order 'which-key-description-order)
  (which-key-setup-side-window-right)
  (which-key-mode))

(provide 'cfg-which-key)

;;; ————————————————————————————————————————————————————————
;;; cfg-which-key.el ends here
