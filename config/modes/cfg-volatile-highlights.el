;;; Package --- Summary
;;; volatile-highlights configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package volatile-highlights
  :diminish volatile-highlights-mode

  :commands
  (volatile-highlights-mode)

  :config
  (volatile-highlights-mode t))

(provide 'cfg-volatile-highlights)

;;; ————————————————————————————————————————————————————————
;;; cfg-volatile-highlights.el ends here
