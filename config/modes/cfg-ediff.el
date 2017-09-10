;;; Package --- Summary
;;; ediff configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-10 12:00:01 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package ediff
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally)

  ;; Restore window configuration after ediff
  (defvar my/ediff-bwin-config nil "Window configuration before ediff.")
  (defcustom my/ediff-bwin-reg ?b
    "*Register to be set up to hold `my/ediff-bwin-config' configuration."
    :group 'ediff)

  (defvar my/ediff-awin-config nil "Window configuration after ediff.")
  (defcustom my/ediff-awin-reg ?e
    "*Register to be used to hold `my/ediff-awin-config' window configuration."
    :group 'ediff)

  (defun my/ediff-bsh ()
    "Function to be called before any buffers or window setup for ediff."
    (setq my/ediff-bwin-config (current-window-configuration))
    (when (characterp my/ediff-bwin-reg)
      (set-register my/ediff-bwin-reg
                    (list my/ediff-bwin-config (point-marker)))))

  (defun my/ediff-ash ()
    "Function to be called after buffers and window setup for ediff."
    (setq my/ediff-awin-config (current-window-configuration))
    (when (characterp my/ediff-awin-reg)
      (set-register my/ediff-awin-reg
                    (list my/ediff-awin-config (point-marker)))))

  (defun my/ediff-qh ()
    "Function to be called when ediff quits."
    (when my/ediff-bwin-config
      (set-window-configuration my/ediff-bwin-config)))

  (add-hook 'ediff-before-setup-hook 'my/ediff-bsh)
  (add-hook 'ediff-after-setup-windows-hook 'my/ediff-ash 'append)
  (add-hook 'ediff-quit-hook 'my/ediff-qh))

;; --

(eval
 (let ((directory-files-original (symbol-function 'directory-files)))


   `(defun my/directory-files-recursive (directory &optional full match nosort)
      "Like `directory-files' but recurses into subdirectories. Does not follow symbolic links."
      (let* ((prefix (or (and full "") directory))
             dirs
             files)
        (mapc (lambda (p)
                (let ((fullname (if full p (concat prefix "/" p))))
                  (when (and (file-directory-p fullname)
                             (null (or (string-match "\\(^\\|/\\).$" p)
                                       (string-match "\\(^\\|/\\)..$" p)
                                       (file-symlink-p fullname))))
                    (setq dirs (cons p dirs)))))
              (funcall ,directory-files-original directory full nil nosort))
        (setq dirs (nreverse dirs))
        (mapc (lambda (p)
                (when (null (file-directory-p (if full p (concat prefix "/" p))))
                  (setq files (cons p files))))
              (funcall ,directory-files-original directory full match nosort))
        (setq files (nreverse files))
        (mapc (lambda (d)
                (setq files
                      (append files
                              (if full
                                  (apply 'my/directory-files-recursive (list d full match nosort))
                                (mapcar (lambda (n)
                                          (concat d "/" n))
                                        (apply 'my/directory-files-recursive (list (concat prefix "/" d) full match nosort)))))))
              dirs)
        files))))

(eval
 `(defun ediff-directories-recursive (dir1 dir2 regexp)
    "Like `ediff-directories' but recurses into sub-directories. Does not follow symbolic links."
    ,(interactive-form (symbol-function 'ediff-directories))
    (let ((directory-files-original (symbol-function 'directory-files)))
      (unwind-protect
          (progn
            (fset 'directory-files (symbol-function 'my/directory-files-recursive))
            (ediff-directories dir1 dir2 regexp)
            (fset 'directory-files directory-files-original))))))

;; --

(provide 'cfg-ediff)

;;; ————————————————————————————————————————————————————————
;;; cfg-ediff.el ends here
