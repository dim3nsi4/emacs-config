;;; Package --- Summary
;;; company configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package company
  :demand
  :diminish company-mode

  :bind
  (:map company-active-map
        ("<tab>"    . company-complete-selection)
        ("<escape>" . company-abort))

  :config
  (setq company-idle-delay 0.
        company-minimum-prefix-length 1
        company-show-numbers nil
        company-tooltip-margin 1
        company-tooltip-align-annotations nil
        company-dabbrev-downcase nil)

  ;; Activate company globally
  (add-hook 'after-init-hook 'global-company-mode)
  ;; Remove dabbrev from company's backends
  (setq company-backends (delete 'company-dabbrev company-backends)))

;; ——

(req-package company-quickhelp
  :defer t
  :after company
  :require company

  :bind
  (:map company-active-map
        ("M-h" . company-quickhelp-manual-begin))
  :config
  ;; delay before displaying the help
  (setq company-quickhelp-delay 0.
        company-quickhelp-max-lines 100)
  ;; Activate quickhelp
  (company-quickhelp-mode 1))

;; ——

(req-package company-jedi
  :defer t
  :after company
  :require company
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook))

;; ——

(req-package company-auctex
  :defer t
  :after company
  :require (company auctex)
  :config
  (company-auctex-init))

;; ——

(provide 'cfg-company)

;;; ————————————————————————————————————————————————————————
;;; cfg-company.el ends here
