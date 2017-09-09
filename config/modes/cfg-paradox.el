;;; Package --- Summary
;;; paradox configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 20:37:51 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package paradox
  :defer t
  :commands (paradox-enable)
  :bind
  (("<f12>" . paradox-list-packages))
  :config
  (paradox-enable)
  (setq paradox-execute-asynchronously t
        paradox-automatically-star nil))

(provide 'cfg-paradox)

;;; ————————————————————————————————————————————————————————
;;; cfg-paradox.el ends here
