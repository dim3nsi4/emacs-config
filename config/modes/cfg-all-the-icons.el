;;; Package --- Summary
;;; all-the-icons configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package all-the-icons)

;; ——

(use-package all-the-icons-dired
  :after (:any dired dired-sidebar)

  :hook
  ((dired-mode . all-the-icons-dired-mode)))

;; ——

(provide 'cfg-all-the-icons)

;;; ————————————————————————————————————————————————————————
;;; cfg-all-the-icons.el ends here
