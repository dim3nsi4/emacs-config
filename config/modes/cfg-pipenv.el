;;; Package --- Summary
;;; pipenv configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(provide 'cfg-pipenv)

(use-package pipenv
  :hook
  (projectile-after-switch-project-hook . pipenv-activate))

;;; ————————————————————————————————————————————————————————
;;; cfg-pipenv.el ends here
