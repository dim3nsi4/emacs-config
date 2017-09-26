;;; Package --- Summary
;;; meghanada configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package meghanada
  :defer t

  :init
  (setq meghanada-mode-key-prefix (kbd "C-c C-c"))
  (add-hook 'java-mode-hook #'meghanada-mode)

  :bind
  (:map meghanada-mode-map
        ("M-," . comment-dwim)))

;; ——

(req-package meghanada
  :require company
  :init
  (add-hook 'meghanada-mode-hook
            (lambda ()
              (add-to-list 'company-backends '(company-files)))))

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

[_c_] compile-file       [_m_] run-task                [_S_] server-start         [_R_] restart
[_C_] compile-project    [_t_] run-junit-test-case     [_K_] server-kill          [_U_] update-server
[_i_] import-all         [_T_] run-junit-class         [_D_] client-disconnect    [_I_] install-server
[_o_] optimize-import    [_s_] switch-testcase         [_P_] ping                 [_?_] version
[_b_] code-beautify      [_k_] kill-running-process    [_l_] locate-variable
    "
    ("<escape>" nil :exit t)

    ("c" meghanada-compile-file)
    ("C" meghanada-compile-project)
    ("i" meghanada-import-all)
    ("o" meghanada-optimize-import)
    ("b" meghanada-code-beautify)

    ("m" meghanada-run-task)
    ("t" meghanada-run-junit-test-case)
    ("T" meghanada-run-junit-class)
    ("s" meghanada-switch-testcase)
    ("k" meghanada-kill-running-process)

    ("S" meghanada-server-start)
    ("K" meghanada-server-kill)
    ("D" meghanada-client-disconnect)
    ("P" meghanada-ping)
    ("l" meghanada-local-variable)

    ("R" meghanada-restart)
    ("U" meghanada-update-server)
    ("I" meghanada-install-server)
    ("?" meghanada-version)))

;; ——

(provide 'cfg-meghanada)

;;; ————————————————————————————————————————————————————————
;;; cfg-meghanada.el ends here
