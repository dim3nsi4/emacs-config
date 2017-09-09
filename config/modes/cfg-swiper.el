;;; Package --- Summary
;;; swiper configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 15:49:45 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(req-package swiper
  :after counsel
  :require counsel
  :defer t

  :bind
  (("M-i" . counsel-grep-or-swiper)))

(provide 'cfg-swiper)

;;; ————————————————————————————————————————————————————————
;;; cfg-swiper.el ends here
