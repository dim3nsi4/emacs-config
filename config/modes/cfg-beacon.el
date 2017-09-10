;;; Package --- Summary
;;; beacon configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package beacon
  :config
  (setq beacon-color "LightSteelBlue3"
        beacon-size 15
        beacon-blink-delay 0.5
        beacon-blink-duration 0.1)
  (beacon-mode 1))

(provide 'cfg-beacon)

;;; ————————————————————————————————————————————————————————
;;; cfg-beacon.el ends here
