;;; Package --- Summary
;;; yasnippet configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 15:59:01 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode

  :bind
  (("C-c C-y"     . nil)
   ("M-n"         . yas-insert-snippet)
   ("C-c C-y C-y" . yas-insert-snippet)
   ("C-c C-y C-n" . yas-new-snippet)

   :map yas-minor-mode-map
   ("TAB" . nil)
   ("<tab>" . nil)

   :map yas-keymap
   ("M-«" . yas-prev-field)
   ("M-»" . yas-next-field))

  :config
  (yas-global-mode 1))

(req-package company
  :defer t
  :after yasnippet
  :require yasnippet

  :bind
  ("C-c C-y C-u" . company-yasnippet)

  :config
  (defun my/company-yasnippet-or-completion ()
    (interactive)
    (let ((yas-maybe-expand nil))
      (unless (yas-expand)
        (call-interactively #'company-complete-common))))

  (add-hook 'company-mode-hook
            (lambda () (substitute-key-definition 'company-complete-common
                                                  'company-yasnippet-or-completion company-active-map))))

;; ——

(provide 'cfg-yasnippet)

;;; ————————————————————————————————————————————————————————
;;; cfg-yasnippet.el ends here
