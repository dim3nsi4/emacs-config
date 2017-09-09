;;; Package --- Summary
;;; origami configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 16:12:18 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package origami
  :defer t

  :bind
  (("M-p"     . origami-recursively-toggle-node)
   ("C-c o t" . origami-toggle-node)
   ("C-c o o" . origami-open-node)
   ("C-c o O" . origami-open-node-recursively)
   ("C-c o c" . origami-close-node)
   ("C-c o C" . origami-close-node-recursively)
   ("C-c o t" . origami-toggle-node)
   ("C-c o T" . origami-forward-toggle-node)
   ("C-c o n" . origami-forward-fold)
   ("C-c o p" . origami-previous-fold)
   ("C-c o R" . origami-reset))

  :init
  (add-hook 'java-mode-hook       'origami-mode)
  (add-hook 'python-mode-hook     'origami-mode)
  (add-hook 'c-mode-common-hook   'origami-mode)
  (add-hook 'emacs-lisp-mode-hook 'origami-mode))

(provide 'cfg-origami)

;;; ————————————————————————————————————————————————————————
;;; cfg-origami.el ends here
