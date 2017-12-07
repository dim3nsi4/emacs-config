;;; Package --- Summary
;;; multiple-cursors configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package multiple-cursors
  :bind
  (("C-c C-e C-e" . mc/edit-lines)
   ("M-«"         . mc/mark-previous-like-this)
   ("M-»"         . mc/mark-next-like-this)
   ("C-»"         . mc/unmark-previous-like-this)
   ("C-«"         . mc/unmark-next-like-this)
   ("C-c C-e C-p" . mc/mark-all-like-this-dwim)
   ("C-c C-e C-a" . mc/mark-all-like-this)
   ("S-<mouse-1>" . mc/add-cursor-on-click)))

(provide 'cfg-multiple-cursors)

;;; ————————————————————————————————————————————————————————
;;; cfg-multiple-cursors.el ends here
