;;; Package --- Summary
;;; paradox configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
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
