;;; Package --- Summary
;;; helpful configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package helpful
  :demand
  :requires counsel

  :defines
  (counsel-describe-function-function
   counsel-describe-variable-function)

  :bind
  (("C-h f"   . helpful-callable)
   ("C-h v"   . helpful-variable)
   ("C-h k"   . helpful-key)
   ("C-h C-h" . helpful-at-point))

  :config
  (setq counsel-describe-function-function 'helpful-function
        counsel-describe-variable-function 'helpful-variable))


(provide 'cfg-helpful)

;;; ————————————————————————————————————————————————————————
;;; cfg-helpful.el ends here
