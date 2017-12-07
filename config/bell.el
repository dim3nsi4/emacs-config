;;; Package --- Summary
;;; Alarm bell configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

;; Disable the bell when scrolling to limits
(defun my/bell-function ()
  "Custom alarm bell to avoid unnecessary alarms event."
  (unless (memq this-command
                '(View-scroll-half-page-backward
                  View-scroll-half-page-forward
                  abort-recursive-edit
                  backward-char
                  company-abort
                  down
                  exit-minibuffer
                  forward-char
                  isearch-abort
                  keyboard-quit
                  mwheel-scroll
                  next-line
                  previous-line
                  scroll-down
                  scroll-up
                  up))
    (ding)))
(setq ring-bell-function 'my/bell-function)

;; Visual bell
(setq visible-bell 1)

(provide 'bell)

;;; ————————————————————————————————————————————————————————
;;; bell.el ends here
