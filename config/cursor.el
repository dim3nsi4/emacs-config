;;; Package --- Summary
;;; Cursor settings.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 14:01:28 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

;; Disable blinking cursor
(blink-cursor-mode 0)

;; Change cursor color according to mode
(defvar my/set-cursor-color-color "")
(defvar my/set-cursor-color-buffer "")

(defun my/set-cursor-color-according-to-mode ()
  "Change cursor color according to some minor modes."
  (let ((color (cond (buffer-read-only "DodgerBlue2")
                     (overwrite-mode "red2")
                     (t "gray25"))))
    (unless (and (string= color my/set-cursor-color-color)
                 (string= (buffer-name) my/set-cursor-color-buffer))
      (set-cursor-color (setq my/set-cursor-color-color color))
      (setq my/set-cursor-color-buffer (buffer-name)))))

(add-hook 'post-command-hook #'my/set-cursor-color-according-to-mode)

(provide 'cursor)

;;; ————————————————————————————————————————————————————————
;;; cursor.el ends here
