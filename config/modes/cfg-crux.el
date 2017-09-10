;;; Package --- Summary
;;; crux configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package crux
  :defer t
  :bind
  (([remap kill-whole-line] . crux-kill-whole-line)
   ([remap move-beginning-of-line] . crux-move-beginning-of-line)))

(provide 'cfg-crux)

;;; ————————————————————————————————————————————————————————
;;; cfg-crux.el ends here
