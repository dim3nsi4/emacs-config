;;; Package --- Summary
;;; Packages configuration
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 15:12:23 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(require 'use-package)
(require 'req-package)

(use-package use-package-chords
  :config
  (key-chord-mode 1)
  (setq key-chord-one-key-delay 0.2
        key-chord-two-keys-delay 0.1))

(provide 'cfg-use-package)

;;; ————————————————————————————————————————————————————————
;;; cfg-use-package.el ends here
