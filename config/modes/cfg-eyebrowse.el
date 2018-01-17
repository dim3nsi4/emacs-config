;;; Package --- Summary
;;; eyebrowse configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package eyebrowse
  :demand
  :diminish

  :commands
  (eyebrowse-mode)

  :bind
  (:map eyebrowse-mode-map
        ("<C-tab>"   . eyebrowse-next-window-config)
        ("<backtab>" . eyebrowse-prev-window-config)
        ("C-c t t"   . eyebrowse-create-window-config)
        ("C-c t c"   . eyebrowse-close-window-config)
        ("C-c t n"   . eyebrowse-next-window-config)
        ("C-c t p"   . eyebrowse-prev-window-config))

  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c t"))

  :config
  (setq eyebrowse-wrap-around t
        eyebrowse-switch-back-and-forth t
        eyebrowse-mode-line-style 'always)
  (eyebrowse-mode t))

(use-package hydra
  :after eyebrowse

  :bind
  (:map eyebrowse-mode-map
        ("C-c t" . my/hydra-eyebrowse/body))

  :config
  (defhydra my/hydra-eyebrowse (:color teal :hint nil :idle 0.25)
    "
Eyebrowse's window configuration

[_n_] next       [_0_] switch to 0     [_5_] switch to 5
[_p_] previous   [_1_] switch to 1     [_6_] switch to 6
[_t_] create     [_2_] switch to 2     [_7_] switch to 7
[_c_] close      [_3_] switch to 3     [_8_] switch to 8
[_r_] rename     [_4_] switch to 4     [_9_] switch to 9
"
  ("<escape>" nil :exit t)
  ("C-o"      nil :exit t)
  ("C-g"      nil :exit t)
  ("q"        nil :exit t)

  ("t" eyebrowse-create-window-config)
  ("c" eyebrowse-close-window-config)
  ("p" eyebrowse-prev-window-config)
  ("n" eyebrowse-next-window-config)
  ("r" eyebrowse-rename-window-config)
  ("0" eyebrowse-switch-to-window-config-0)
  ("1" eyebrowse-switch-to-window-config-1)
  ("2" eyebrowse-switch-to-window-config-2)
  ("3" eyebrowse-switch-to-window-config-3)
  ("4" eyebrowse-switch-to-window-config-4)
  ("5" eyebrowse-switch-to-window-config-5)
  ("6" eyebrowse-switch-to-window-config-6)
  ("7" eyebrowse-switch-to-window-config-7)
  ("8" eyebrowse-switch-to-window-config-8)
  ("9" eyebrowse-switch-to-window-config-9)))

;; ——

(provide 'cfg-eyebrowse)

;;; ————————————————————————————————————————————————————————
;;; cfg-eyebrowse.el ends here
