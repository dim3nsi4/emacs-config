;;; Package --- Summary
;;; ediff configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package ediff
  :hook
  ((ediff-before-setup . my/ediff-bsh)
   (ediff-quit         . my/ediff-qh))

  :init
  (add-hook 'ediff-after-setup-windows-hook 'my/ediff-ash 'append)

  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally)

  ;; Restore window configuration after ediff
  (defvar my/ediff-bwin-config nil "Window configuration before ediff.")
  (defcustom my/ediff-bwin-reg ?b
    "*Register to be set up to hold `my/ediff-bwin-config' configuration."
    :group 'ediff)

  (defvar my/ediff-awin-config nil "Window configuration after ediff.")
  (defcustom my/ediff-awin-reg ?e
    "*Register to be used to hold `my/ediff-awin-config' window configuration."
    :group 'ediff)

  (defun my/ediff-bsh ()
    "Function to be called before any buffers or window setup for ediff."
    (setq my/ediff-bwin-config (current-window-configuration))
    (when (characterp my/ediff-bwin-reg)
      (set-register my/ediff-bwin-reg
                    (list my/ediff-bwin-config (point-marker)))))

  (defun my/ediff-ash ()
    "Function to be called after buffers and window setup for ediff."
    (setq my/ediff-awin-config (current-window-configuration))
    (when (characterp my/ediff-awin-reg)
      (set-register my/ediff-awin-reg
                    (list my/ediff-awin-config (point-marker)))))

  (defun my/ediff-qh ()
    "Function to be called when ediff quits."
    (when my/ediff-bwin-config
      (set-window-configuration my/ediff-bwin-config))))

(provide 'cfg-ediff)

;;; ————————————————————————————————————————————————————————
;;; cfg-ediff.el ends here
