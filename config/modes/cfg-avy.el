;;; Package --- Summary
;;; avy configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

;; Fast go to char/word/line/... in the current view
(use-package avy
  :defer t
  :bind
  (("M-s" . avy-goto-char))

  :config
  ;; Keys for bépo keyboard
  (setq avy-keys '(?v ?d ?l ?j ?t ?s ?r ?n ?q ?g ?h ?f)))

(req-package zzz-to-char
  :defer t
  :after avy
  :require avy

  :bind
  (("M-z" . zzz-up-to-char))

  :config
  (setq zzz-to-char-reach 320))

(provide 'cfg-avy)

;;; ————————————————————————————————————————————————————————
;;; cfg-avy.el ends here
