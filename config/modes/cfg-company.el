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

  :hook
  ((after-init . global-company-mode))

  :bind
  (:map company-active-map
        ("<tab>"    . company-complete-selection)
        ("<escape>" . company-abort))

  :config
  (setq company-idle-delay 0.
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-margin 1
        company-tooltip-align-annotations nil
        company-dabbrev-downcase nil)
  ;; Remove dabbrev from company's backends
  (setq company-backends (delete 'company-dabbrev company-backends))

  ;; Candidate selection using digits
  (defun my/company-number ()
    "Forward to `company-complete-number'.

     Unless the number is potentially part of the candidate.
     In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (cl-find-if (lambda (s) (string-match re s))
                      company-candidates)
          (self-insert-command 1)
        (company-complete-number (string-to-number k)))))

  ;; Binds digits to their corresponding candidate, unbinds RET and binds SPC to
  ;; close company popup.
  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'my/company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1)))
    (define-key map (kbd "<return>") nil)))

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
