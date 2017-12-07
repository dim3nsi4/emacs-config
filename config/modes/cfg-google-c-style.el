;;; Package --- Summary
;;; google-c-style configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package google-c-style
  :defines
  (c-basic-offset)

  :commands
  (google-make-newline-indent
   google-set-c-style)

  :hook
  ((c-mode-common . my/init-google-c-style)
   (java-mode     . my/init-google-c-style))

  :config
  (defun my/init-google-c-style()
    (subword-mode)
    (google-set-c-style)
    (google-make-newline-indent)
    (setq c-basic-offset 4)
    (c-set-offset 'inher-intro 0)
    (c-set-offset 'case-label 0)
    (c-set-offset 'substatement-open '0)
    (c-set-offset 'func-decl-cont 0)))

(provide 'cfg-google-c-style)

;;; ————————————————————————————————————————————————————————
;;; cfg-google-c-style.el ends here
