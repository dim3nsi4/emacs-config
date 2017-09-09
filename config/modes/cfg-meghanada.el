;;; Package --- Summary
;;; meghanada configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 15:44:55 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package meghanada
  :defer t

  :init
  (setq meghanada-mode-key-prefix (kbd "C-c c"))
  (add-hook 'java-mode-hook #'meghanada-mode)

  :bind
  (:map meghanada-mode-map
        ("M-," . comment-dwim))

  :config
  (add-hook 'java-mode-hook 'meghanada-mode))

;; ——

(req-package hydra
  :defer t
  :after meghanada
  :require meghanada

  :bind
  (:map meghanada-mode-map
        ("C-c C-c" . my/hydra-meghanada/body))

  :config
  (defhydra my/hydra-meghanada (:color teal :hint nil :idle 0.25)
    "
Meghanada

[_c_] compile-file           [_i_] import-all         [_S_] server-start         [_R_] restart
[_C_] compile-project        [_o_] optimize-import    [_K_] server-kill          [_U_] update-server
[_m_] run-task               [_b_] code-beautify      [_D_] client-disconnect    [_I_] install-server
[_t_] run-junit-test-case    [_l_] locate-variable    [_P_] ping                 [_?_] version
[_T_] run-junit-class        [_s_] switch-testcase
    "
    ("<escape>" nil :exit t)

    ("c" meghanada-compile-file)
    ("C" meghanada-compile-project)
    ("m" meghanada-run-task)
    ("t" meghanada-run-junit-test-case)
    ("T" meghanada-run-junit-class)
    ("s" meghanada-switch-testcase)
    ("l" meghanada-local-variable)
    ("b" meghanada-code-beautify)
    ("i" meghanada-import-all)
    ("o" meghanada-optimize-import)
    ("S" meghanada-server-start)
    ("K" meghanada-server-kill)
    ("D" meghanada-client-disconnect)
    ("P" meghanada-ping)
    ("R" meghanada-restart)
    ("U" meghanada-update-server)
    ("I" meghanada-install-server)
    ("?" meghanada-version)))

;; ——

(provide 'cfg-meghanada)

;;; ————————————————————————————————————————————————————————
;;; cfg-meghanada.el ends here
