;;; Package --- Summary
;;; swiper configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
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
