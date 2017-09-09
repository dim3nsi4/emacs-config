;;; Package --- Summary
;;; ivy configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 18:59:16 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package ivy
  :demand
  :diminish ivy-mode

  :bind
  (("C-x C-b" . ivy-switch-buffer)
   ("C-c C-r" . ivy-resume)

   :map ivy-minibuffer-map
   ("<escape>"   . keyboard-escape-quit)
   ("<M-return>" . ivy-call)
   ("<return>"   . ivy-alt-done)
   ("<prior>"    . ivy-scroll-down-command)
   ("<next>"     . ivy-scroll-up-command)
   ("<C-next>"   . ivy-end-of-buffer)
   ("<C-prior>"  . ivy-beginning-of-buffer)
   ("<C-up>"     . ivy-previous-history-element)
   ("<C-down>"   . ivy-next-history-element))

  :config
  (ivy-mode 1)

  (setq ivy-height 11
        ivy-fixed-height-minibuffer t
        ivy-wrap t
        ivy-action-wrap t
        ivy-use-virtual-buffers t
        ivy-format-function 'ivy-format-function-line
        ivy-count-format ""
        ivy-extra-directories nil
        ivy-ignore-buffers '("\\` " "\\*.+" "^\\:.+$")
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

;; ——

(req-package ivy-rich
  :defer t
  :after ivy
  :require ivy

  :config
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-abbreviate-paths t
        ivy-rich-switch-buffer-name-max-length 40
        ivy-rich-switch-buffer-project-max-length 15
        ivy-rich-switch-buffer-mode-max-length 20
        ivy-rich-switch-buffer-align-virtual-buffer t)

  (add-hook 'minibuffer-setup-hook (lambda () (setq show-trailing-whitespace nil)))
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

;; ——

(provide 'cfg-ivy)

;;; ————————————————————————————————————————————————————————
;;; cfg-ivy.el ends here
