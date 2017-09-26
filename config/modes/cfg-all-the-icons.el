;;; Package --- Summary
;;; all-the-icons configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package all-the-icons)

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
