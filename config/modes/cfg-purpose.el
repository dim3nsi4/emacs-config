;;; Package --- Summary
;;; purpose configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package window-purpose
  :config
  (add-to-list 'purpose-user-mode-purposes '(python-mode . py))
  (add-to-list 'purpose-user-mode-purposes '(inferior-python-mode . py-repl))
  (purpose-compile-user-configuration)

  (purpose-mode))

;; ——

(use-package window-purpose-x
  :config
  (purpose-x-magit-multi-on)
  (purpose-x-popwin-setup)
  (purpose-x-kill-setup))

;; ——

;; (req-package ivy-purpose
;;   :defer t
;;   :after purpose
;;   :require purpose
;;   :bind
;;   (("C-x C-b" . 'ivy-purpose-switch-buffer-with-some-purpose))

;;   :config
;;   (ivy-purpose-setup))

;; ——

(provide 'cfg-purpose)

;;; ————————————————————————————————————————————————————————
;;; cfg-purpose.el ends here
