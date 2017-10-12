;;; Package --- Summary
;;; smartparens configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package smartparens-config
  :demand
  :diminish smartparens-mode

  :config
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'python-mode-hook     'smartparens-mode)
  (add-hook 'java-mode-hook       'smartparens-mode)
  (show-smartparens-global-mode +1))

(provide 'cfg-smartparens)

;;; ————————————————————————————————————————————————————————
;;; cfg-smartparens.el ends here
