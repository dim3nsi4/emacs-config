;;; Package --- Summary
;;; avy configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package avy
  :bind
  (("M-s" . avy-goto-char))

  :config
  ;; Keys for bépo keyboard
  (setq avy-keys '(?v ?d ?l ?j ?t ?s ?r ?n ?q ?g ?h ?f)))

;; ——

(use-package zzz-to-char
  :bind
  (("M-z" . zzz-up-to-char))

  :config
  (setq zzz-to-char-reach 320))

;; ——

(provide 'cfg-avy)

;;; ————————————————————————————————————————————————————————
;;; cfg-avy.el ends here
