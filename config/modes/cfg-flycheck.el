;;; Package --- Summary
;;; flycheck configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package flycheck
  :demand

  :commands
  (global-flycheck-mode)

  :hook
  (python-mode . my/flycheck-python-hook)

  :init
  (setq flycheck-keymap-prefix (kbd "C-c s"))

  :config
  ;; (setq flycheck-temp-prefix "/tmp/.flycheck")
  (setq flycheck-emacs-lisp-load-path 'inherit)

  (defun my/flycheck-python-hook ()
    "Custom hook for flycheck in python mode."
    (interactive)
    (setq flycheck-checker 'python-pylint
          flycheck-checker-error-threshold 900
          flycheck-pylintrc ".pylintrc"))

  (global-flycheck-mode t))

;; ——

(use-package hydra
  :after flycheck

  :commands
  (flycheck-buffer
   flycheck-clear
   flycheck-previous-error
   flycheck-next-error
   flycheck-list-errors
   flycheck-explain-error-at-point
   flycheck-display-error-at-point
   flycheck-select-checker
   flycheck-disable-checker
   flycheck-describe-checker
   flycheck-verify-setup
   flycheck-version
   flycheck-manual
   hydra-default-pre
   hydra-keyboard-quit
   hydra--call-interactively-remap-maybe
   hydra-idle-message
   hydra-set-transient-map)

  :bind
  (:map flycheck-mode-map
        ("C-c s" . my/hydra-flycheck/body))

  :config
  (defhydra my/hydra-flycheck (:color teal :hint nil :idle 0.25)
    "
Flycheck

[_b_] check buffer      [_l_] list-errors               [_s_] select checker      [_v_] verify-setup
[_c_] clear buffer      [_h_] display error at point    [_x_] disable checker     [_V_] version
[_p_] previous error    [_e_] explain error at point    [_?_] describe checker    [_i_] manual
[_n_] next error
    "
    ("<escape>" nil :exit t)
    ("C-o"      nil :exit t)
    ("C-g"      nil :exit t)
    ("q"        nil :exit t)

    ("b" flycheck-buffer)
    ("c" flycheck-clear)
    ("p" flycheck-previous-error)
    ("n" flycheck-next-error)

    ("l" flycheck-list-errors)
    ("e" flycheck-explain-error-at-point)
    ("h" flycheck-display-error-at-point)

    ("s" flycheck-select-checker)
    ("x" flycheck-disable-checker)
    ("?" flycheck-describe-checker)

    ("v" flycheck-verify-setup)
    ("V" flycheck-version)
    ("i" flycheck-manual)))

;; ——

(use-package flycheck-pos-tip
  :after flycheck

  :commands
  (flycheck-pos-tip-mode)

  :config
  (setq flycheck-pos-tip-timeout 10)
  (flycheck-pos-tip-mode))

;; ——

(provide 'cfg-flycheck)

;;; ————————————————————————————————————————————————————————
;;; cfg-flycheck.el ends here
