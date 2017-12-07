;;; Package --- Summary
;;; sql configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
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
  :hook
  ((sql-mode . sqlup-mode)
   (sql-interactive-mode . sqlup-mode))

  :config
  (setq sqlup-blacklist '("schema" "table")))

(provide 'cfg-sql)

;;; ————————————————————————————————————————————————————————
;;; cfg-sql.el ends here
