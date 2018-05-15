;;; Package --- Summary
;;; smartparens configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package smartparens-config
  :demand
  :diminish smartparens-mode

  :hook
  (emacs-lisp-mode . smartparens-mode)
  (python-mode     . smartparens-mode)
  (java-mode       . smartparens-mode)

  :config
  (show-smartparens-global-mode +1))

(provide 'cfg-smartparens)

;;; ————————————————————————————————————————————————————————
;;; cfg-smartparens.el ends here
