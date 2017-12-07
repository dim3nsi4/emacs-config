;;; Package --- Summary
;;; origami configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package origami
  :hook
  ((java-mode-hook       . origami-mode)
   (python-mode-hook     . origami-mode)
   (c-mode-common-hook   . origami-mode)
   (emacs-lisp-mode-hook . origami-mode))

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
   ("C-c o R" . origami-reset)))


(provide 'cfg-origami)

;;; ————————————————————————————————————————————————————————
;;; cfg-origami.el ends here
