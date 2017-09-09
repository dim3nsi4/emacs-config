;;; Package --- Summary
;;; perspeen configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 15:40:13 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package perspeen
  :demand

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

(req-package hydra
  :defer t
  :after perspeen
  :require perspeen

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
