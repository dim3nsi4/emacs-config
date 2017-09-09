;;; Package --- Summary
;;; company configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 15:52:54 seimandp>
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
  (setq company-idle-delay 0.5          ; delay before displaying auto completion choices
        company-minimum-prefix-length 2 ; number of characters needed before auto completion popup
        company-show-numbers nil        ; show/hide the quick access numbers
        company-dabbrev-downcase nil)   ; whether dabbrev candidates are case sensitive or not

  ;; Activate company globally
  (add-hook 'after-init-hook 'global-company-mode)
  ;; Remove dabbrev from company's backends
  (delete "company-dabbrev" company-backends))

;; ——

(req-package company-quickhelp
  :defer t
  :after company
  :require company

  :bind
  (:map company-active-map
        ("M-h" . company-quickhelp-manual-begin))

  :config
  ;; Activate quickhelp
  (company-quickhelp-mode 1)

  ;; delay before displaying the help
  (setq company-quickhelp-delay 0.))

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
