;;; Package --- Summary
;;; lua-mode configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 16:26:55 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package lua-mode
  :defer t

  :mode
  ("\\.lua$" . lua-mode)
  :interpreter
  ("lua" . lua-mode)

  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t))

(provide 'cfg-lua-mode)

;;; ————————————————————————————————————————————————————————
;;; cfg-lua-mode.el ends here
