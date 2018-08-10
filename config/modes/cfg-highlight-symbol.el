;;; Package --- Summary
;;; highlight-symbol configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package highlight-symbol
  :diminish

  :hook
  ((java-mode       . highlight-symbol-mode)
   (emacs-lisp-mode . highlight-symbol-mode)
   (python-mode     . highlight-symbol-mode))

  :bind
  (("C-c h h" . highlight-symbol)
   ("C-c h c" . highlight-symbol-remove-all))

  :config
  (setq highlight-symbol-idle-delay 0.5
        highlight-symbol-highlight-single-occurrence nil))

(provide 'cfg-highlight-symbol)

;;; ————————————————————————————————————————————————————————
;;; cfg-highlight-symbol.el ends here
