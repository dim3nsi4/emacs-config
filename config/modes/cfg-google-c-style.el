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
  :defer t
  :init
  (add-hook 'c-mode-common-hook
            (lambda()
              (subword-mode)
              (google-set-c-style)
              (google-make-newline-indent)
              (setq c-basic-offset 4)
              (c-set-offset 'inher-intro 0)
              (c-set-offset 'case-label 0)
              (c-set-offset 'substatement-open '0)
              (c-set-offset 'func-decl-cont 0)))

  (add-hook 'java-mode-hook
            (lambda()
              (subword-mode)
              (google-set-c-style)
              (google-make-newline-indent)
              (setq c-basic-offset 4)
              (c-set-offset 'inher-intro 0)
              (c-set-offset 'case-label 0)
              (c-set-offset 'substatement-open '0)
              (c-set-offset 'func-decl-cont 0))))

(provide 'cfg-google-c-style)

;;; ————————————————————————————————————————————————————————
;;; cfg-google-c-style.el ends here
