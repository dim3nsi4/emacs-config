;;; Package --- Summary
;;; sql configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 16:42:31 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(add-to-list 'same-window-buffer-names "*SQL*")

(defun my/sql-save-history-hook ()
  (let ((lval 'sql-input-ring-file-name)
        (rval 'sql-product))
    (if (symbol-value rval)
        (let ((filename
               (concat "~/.emacs.d/sql/"
                       (symbol-name (symbol-value rval))
                       "-history.sql")))
          (set (make-local-variable lval) filename))
      (error
       (format "SQL history will not be saved because %s is nil"
               (symbol-name rval))))))

(add-hook 'sql-interactive-mode-hook 'my/sql-save-history-hook)

(use-package sqlup-mode
  :config
  ;; Capitalize keywords in SQL mode
  (add-hook 'sql-mode-hook 'sqlup-mode)
  ;; Capitalize keywords in an interactive session (e.g. psql)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode))

(provide 'cfg-sql)

;;; ————————————————————————————————————————————————————————
;;; cfg-sql.el ends here
