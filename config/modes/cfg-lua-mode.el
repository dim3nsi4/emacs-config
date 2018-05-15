;;; Package --- Summary
;;; lua-mode configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package lua-mode
  :mode
  ("\\.lua$" . lua-mode)

  :interpreter
  ("lua" . lua-mode)

  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t))

(provide 'cfg-lua-mode)

;;; ————————————————————————————————————————————————————————
;;; cfg-lua-mode.el ends here
