;;; Package --- Summary
;;; Scrolling configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(setq scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-preserve-screen-position 1
      scroll-down-aggressively 0.01)

;;scroll window up/down by one line
(global-set-key (kbd "<C-S-up>")   (kbd "C-u 1 M-v"))
(global-set-key (kbd "<C-S-down>") (kbd "C-u 1 C-v"))

(use-package view
  :commands
  (View-scroll-half-page-forward
   View-scroll-half-page-backward)

  :bind
  (("<next>"  . my/half-page-forward)
   ("<prior>" . my/half-page-backward))

  :config
  (defun my/half-page-forward ()
    (interactive)
    (condition-case nil (View-scroll-half-page-forward) (end-of-buffer (goto-char (point-max)))))

  (defun my/half-page-backward ()
    (interactive)
    (condition-case nil (View-scroll-half-page-backward) (beginning-of-buffer (goto-char (point-min))))))

(provide 'scrolling)

;;; ————————————————————————————————————————————————————————
;;; scrolling.el ends here
