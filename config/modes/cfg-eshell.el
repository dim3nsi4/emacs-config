;;; Package --- Summary
;;; eshell configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 15:31:10 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package eshell
  :defer t
  :commands (eshell-send-input)
  :config
  ;; Allows to completely clear the eshell buffer using C-l
  (defun my/eshell-clear-buffer ()
    "Clear terminal."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))
  ;; Hooks
  (add-hook 'eshell-mode-hook '(lambda() (setq show-trailing-whitespace nil)))
  (add-hook 'eshell-mode-hook '(lambda() (local-set-key (kbd "C-l") 'my/eshell-clear-buffer))))

(provide 'cfg-eshell)

;;; ————————————————————————————————————————————————————————
;;; cfg-eshell.el ends here
