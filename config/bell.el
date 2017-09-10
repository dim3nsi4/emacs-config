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
                '(isearch-abort
                  abort-recursive-edit
                  exit-minibuffer
                  keyboard-quit
                  mwheel-scroll
                  down
                  up
                  View-scroll-half-page-forward
                  View-scroll-half-page-backward
                  scroll-down
                  scroll-up
                  next-line
                  previous-line
                  backward-char
                  forward-char))
    (ding)))
(setq ring-bell-function 'my/bell-function)

;; Visual bell
(setq visible-bell 1)

(provide 'bell)

;;; ————————————————————————————————————————————————————————
;;; bell.el ends here
