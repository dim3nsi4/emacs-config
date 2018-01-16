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
  :hook
  (java-mode . meghanada-mode)

  :init
  (setq meghanada-mode-key-prefix (kbd "C-c C-c"))

  :bind
  (:map meghanada-mode-map
        ("M-,"   . comment-dwim)
        ("C-S-j" . meghanada-import-all)
        ("C-S-n" . meghanada-optimize-import)
        ("C-S-f" . meghanada-code-beautify))

  :config
  (meghanada-eldoc-enable))

;; ——

(use-package meghanada
  :requires company

  :defines
  (company-backends)

  :hook
  (meghanada-mode . my/meghanada-setup-company-backends)

  :config
  (defun my/meghanada-setup-company-backends ()
    (add-to-list 'company-backends '(company-files))))

;; ——

(use-package hydra
  :after meghanada

  :commands
  (hydra--call-interactively-remap-maybe
   hydra-default-pre
   hydra-idle-message
   hydra-keyboard-quit
   hydra-set-transient-map
   meghanada-clear-cache
   meghanada-client-disconnect
   meghanada-compile-file
   meghanada-compile-project
   meghanada-debug-main
   meghanada-exec-main
   meghanada-install-server
   meghanada-kill-running-process
   meghanada-local-variable
   meghanada-ping
   meghanada-reference
   meghanada-restart
   meghanada-run-junit-class
   meghanada-run-junit-test-case
   meghanada-run-task
   meghanada-server-kill
   meghanada-server-start
   meghanada-switch-testcase
   meghanada-typeinfo
   meghanada-update-server
   meghanada-version)

  :bind
  (:map meghanada-mode-map
        ("C-c C-c" . my/hydra-meghanada/body))

  :config
  (defhydra my/hydra-meghanada (:color teal :hint nil :idle 0.25)
    "
Meghanada

[_c_] compile-file       [_x_] exec-main              [_k_] kill-running-process    [_S_] server-start         [_U_] update-server
[_C_] compile-project    [_d_] debug-main             [_s_] switch-testcase         [_K_] server-kill          [_I_] install-server
[_i_] import-all         [_t_] run-junit-test-case    [_l_] local-variable          [_R_] restart              [_Z_] clear-cache
[_o_] optimize-import    [_T_] run-junit-class        [_y_] typeinfo                [_D_] client-disconnect    [_?_] version
[_b_] code-beautify      [_m_] run-task               [_r_] reference               [_P_] ping
    "
    ("<escape>" nil :exit t)

    ("c" meghanada-compile-file)
    ("C" meghanada-compile-project)
    ("i" meghanada-import-all)
    ("o" meghanada-optimize-import)
    ("b" meghanada-code-beautify)

    ("x" meghanada-exec-main)
    ("d" meghanada-debug-main)
    ("t" meghanada-run-junit-test-case)
    ("T" meghanada-run-junit-class)
    ("m" meghanada-run-task)

    ("k" meghanada-kill-running-process)
    ("s" meghanada-switch-testcase)
    ("l" meghanada-local-variable)
    ("y" meghanada-type-info)
    ("r" meghanada-reference)

    ("S" meghanada-server-start)
    ("K" meghanada-server-kill)
    ("R" meghanada-restart)
    ("D" meghanada-client-disconnect)
    ("P" meghanada-ping)

    ("U" meghanada-update-server)
    ("I" meghanada-install-server)
    ("Z" meghanada-clear-cache)
    ("?" meghanada-version)))

;; ——

(provide 'cfg-meghanada)

;;; ————————————————————————————————————————————————————————
;;; cfg-meghanada.el ends here
