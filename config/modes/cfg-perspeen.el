;;; Package --- Summary
;;; perspeen configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package perspeen
  :demand

  :commands
  (perspeen-mode
   perspeen-tab-del)

  :defines
  (perspeen-use-tab
   perspeen-mode-map
   perspeen-keymap-prefix)

  :init
  (setq perspeen-keymap-prefix (kbd "C-c t"))

  :bind
  (:map perspeen-mode-map
        ("<C-tab>" . perspeen-tab-next)
        ("C-c t q" . perspeen-tab-del))

  :config
  (setq perspeen-use-tab t)
  (perspeen-mode))

;; ——

(use-package hydra
  :requires perspeen

  :defines
  (perspeen-mode-map)

  :commands
  (hydra--call-interactively-remap-maybe
   hydra-idle-message
   hydra-keyboard-quit
   hydra-default-pre
   hydra-set-transient-map
   perspeen-change-root-dir
   perspeen-create-ws
   perspeen-delete-ws
   perspeen-goto-last-ws
   perspeen-goto-ws
   perspeen-next-ws
   perspeen-previous-ws
   perspeen-rename-ws
   perspeen-tab-create-tab
   perspeen-tab-prev
   perspeen-ws-eshell)

  :bind
  (:map perspeen-mode-map
        ("C-c t" . my/hydra-perspeen/body))

  :config
  (defhydra my/hydra-perspeen (:color teal :hint nil :idle 0.25)
    "
Perspeen

[_c_] create-ws    [_p_] previous-ws     [_d_] change-root-dir    [_t_] create-tab
[_k_] delete-ws    [_n_] next-ws         [_e_] ws-eshell          [_q_] delete-tab
[_r_] rename-ws    [_'_] goto-last-ws    [_s_] goto-ws
    "
    ("<escape>" nil :exit t)

    ;; Workspaces
    ("c" perspeen-create-ws)
    ("k" perspeen-delete-ws)
    ("r" perspeen-rename-ws)
    ("p" perspeen-previous-ws)
    ("n" perspeen-next-ws)
    ("'" perspeen-goto-last-ws)
    ("d" perspeen-change-root-dir)
    ("e" perspeen-ws-eshell)
    ("s" perspeen-goto-ws)

    ;; Tabs
    ("t" perspeen-tab-create-tab)
    ("q" perspeen-tab-del)
    ("<left>"     perspeen-tab-prev)
    ("<right>"    perspeen-tab-next)
    ("<C-tab>"    perspeen-tab-next)))

;; ——

(provide 'cfg-perspeen)

;;; ————————————————————————————————————————————————————————
;;; cfg-perspeen.el ends here
