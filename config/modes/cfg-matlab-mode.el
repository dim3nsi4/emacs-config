;;; Package --- Summary
;;; matlab-mode configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package matlab-mode
  :defer t
  :mode "\\.m$"
  :defines (matlab-indent-function
            matlab-shell-command)

  :config
  (autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
  (setq matlab-indent-function t)
  (setq matlab-shell-command "matlab"))

(provide 'cfg-matlab-mode)

;;; ————————————————————————————————————————————————————————
;;; cfg-matlab-mode.el ends here
