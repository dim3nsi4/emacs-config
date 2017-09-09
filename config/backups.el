;;; Package --- Summary
;;; Backups policy configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 13:49:22 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(setq backup-by-copying t     ; copy all files, don't rename them.
      delete-old-versions t   ; don't ask to delete excess backup versions.
      kept-new-versions 10    ; number of newest versions to keep.
      kept-old-versions 0     ; number of oldest versions to keep.
      version-control t       ; use version numbers for backups
      vc-make-backup-files t) ; backup version controlled files

;; Directory for the backup per save
(setq backup-directory-alist '(("." . "~/.backups/per-save")))

(defun my/force-backup-of-buffer ()
  "Make a special \"per session\" backup at the first save of each emacs session."
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.backups/per-session")))
          (kept-new-versions 5))
      (backup-buffer)))

  ;; Make a "per save" backup on each save. The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering of
  ;; per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook 'my/force-backup-of-buffer)

;; Disable lock file creation (#* files)
(setq create-lockfiles nil)

(provide 'backups)

;;; ————————————————————————————————————————————————————————
;;; backups.el ends here
