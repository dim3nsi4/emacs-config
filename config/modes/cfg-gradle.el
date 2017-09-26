;;; Package --- Summary
;;; gradle configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package gradle-mode
  :config
  (gradle-mode +1))

;; ——

(use-package groovy-mode
  :mode ("\\.gradle\\'" . groovy-mode))

;; ——

(provide 'cfg-gradle)

;;; ————————————————————————————————————————————————————————
;;; cfg-gradle.el ends here
