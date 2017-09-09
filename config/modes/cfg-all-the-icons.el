;;; Package --- Summary
;;; all-the-icons configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 17:01:53 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package all-the-icons
  :defer t)

;; ——

(req-package all-the-icons-dired
  :defer t
  :after dired
  :require all-the-icons

  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; ——

(provide 'cfg-all-the-icons)

;;; ————————————————————————————————————————————————————————
;;; cfg-all-the-icons.el ends here
