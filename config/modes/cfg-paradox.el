;;; Package --- Summary
;;; paradox configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package paradox
  :defer t
  :commands (paradox-enable)
  :bind
  (("<f12>" . paradox-list-packages))
  :config
  ;; The "paradox-token" file is supposed to contain this line:
  ;; (setq paradox-github-token "<YOUR_TOKEN>")
  (load (locate-user-emacs-file "paradox-token") :noerror :nomessage)
  (setq paradox-execute-asynchronously t
        paradox-automatically-star nil)
  (paradox-enable))

(provide 'cfg-paradox)

;;; ————————————————————————————————————————————————————————
;;; cfg-paradox.el ends here
