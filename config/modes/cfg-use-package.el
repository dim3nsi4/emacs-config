;;; Package --- Summary
;;; Packages configuration
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(require 'use-package)

(setq use-package-compute-statistics t)

(use-package use-package-chords
  :config
  (key-chord-mode 1)
  (setq key-chord-one-key-delay 0.2
        key-chord-two-keys-delay 0.1))

(provide 'cfg-use-package)

;;; ————————————————————————————————————————————————————————
;;; cfg-use-package.el ends here
