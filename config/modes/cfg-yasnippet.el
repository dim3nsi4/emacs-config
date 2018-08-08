;;; Package --- Summary
;;; yasnippet configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package yasnippet
  :diminish yas-minor-mode

  :commands
  (yas-reload-all)

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

  :hook
  (prog-mode . yas-minor-mode)

  :config
  (yas-reload-all))

;; ——

(use-package company
  :after yasnippet

  :commands
  (company-complete-common
   yas-expand)

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
