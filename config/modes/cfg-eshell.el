;;; Package --- Summary
;;; eshell configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package eshell
  :commands
  (eshell-send-input)

  :hook
  (eshell-mode . my/eshell-disable-trailing-whitespace)
  (eshell-mode . my/eshell-keybind-clear-buffer)

  :bind
  (("<f4>" . eshell))

  :config
  (defun my/eshell-disable-trailing-whitespace()
    (setq show-trailing-whitespace nil))

  (defun my/eshell-keybind-clear-buffer()
    (local-set-key (kbd "C-l") 'my/eshell-clear-buffer))

  ;; Allows to completely clear the eshell buffer using C-l
  (defun my/eshell-clear-buffer ()
    "Clear terminal."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input))))

(provide 'cfg-eshell)

;;; ————————————————————————————————————————————————————————
;;; cfg-eshell.el ends here
