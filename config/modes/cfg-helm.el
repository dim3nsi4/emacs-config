;;; Package --- Summary
;;; helm configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package helm
  :defer t

  ;; :bind
  ;; (("M-x"     . helm-M-x)
  ;;  ("C-x C-b" . helm-buffers-list)
  ;;  ("C-x b"   . helm-mini)
  ;;  ("C-x C-f" . helm-find-files)
  ;;  ("C-x f"   . helm-recentf)
  ;;  ("C-S-h"   . helm-apropos)
  ;;  ("<f9>"    . helm-imenu)
  ;;  ("<S-f9>"  . helm-imenu-in-all-buffers)
  ;;  ("<f12>"   . helm-list-elisp-packages))

  :config
  ;; (helm-mode 1)
  (helm-autoresize-mode 1)
  (setq helm-autoresize-max-height 30
        helm-autoresize-min-height 30
        helm-split-window-in-side-p nil))

(provide 'cfg-helm)

;;; ————————————————————————————————————————————————————————
;;; cfg-helm.el ends here
