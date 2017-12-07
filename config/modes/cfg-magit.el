;;; Package --- Summary
;;; magit configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package magit
  :bind
  (("C-x g" . magit-status))

  :config
  (setq magit-item-highlight-face 'bold))

;; ——

(use-package ivy
  :requires magit

  :config
  (setq magit-completing-read-function 'ivy-completing-read))

;; ——

(use-package diff-hl
  :requires magit

  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; ——

(provide 'cfg-magit)

;;; ————————————————————————————————————————————————————————
;;; cfg-magit.el ends here
