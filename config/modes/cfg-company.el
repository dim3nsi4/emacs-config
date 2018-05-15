;;; Package --- Summary
;;; company configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package company
  :demand
  :diminish company-mode

  :hook
  ((after-init . global-company-mode))

  :bind
  (:map company-active-map
        ("<tab>"    . company-complete-selection)
        ("<escape>" . company-abort))

  :config
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-margin 1
        company-tooltip-align-annotations nil
        company-dabbrev-downcase nil)
  ;; Remove dabbrev from company's backends
  (setq company-backends (delete 'company-dabbrev company-backends)))

;; ——

(use-package company-quickhelp
  :demand
  :after company

  :commands
  (company-quickhelp-mode)

  :bind
  (:map company-active-map
        ("M-h" . company-quickhelp-manual-begin))

  :config
  (setq company-quickhelp-delay 0.1
        company-quickhelp-max-lines 100)
  (company-quickhelp-mode 1))

;; ——

(use-package company-jedi
  :after company

  :hook
  (python-mode . my/python-mode-hook)

  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi)))

;; ——

(use-package company-auctex
  :after company

  :commands
  (company-auctex-init)

  :config
  (company-auctex-init))

;; ——

;; (use-package company-eshell-autosuggest
;;   :defer t
;;   :after company

;;   :hook (eshell-mode . setup-company-eshell-autosuggest)

;;   :config
;;   (defun setup-company-eshell-autosuggest ()
;;   (with-eval-after-load 'company
;;     (setq-local company-backends '(company-eshell-autosuggest))
;;     (setq-local company-frontends '(company-preview-frontend)))))

;; ——

(provide 'cfg-company)

;;; ————————————————————————————————————————————————————————
;;; cfg-company.el ends here
