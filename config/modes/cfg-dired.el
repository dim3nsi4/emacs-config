;;; Package --- Summary
;;; dired configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 16:10:20 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(req-package dired
  :defer t
  :require ediff
  :bind
  (:map dired-mode-map
        ("°" . dired-diff)
        ("=" . my/dired-ediff-files))
  :commands (dired-dwim-target-directory)

  :config
  (defun my/dired-ediff-files ()
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2 (if (cdr files)
                           (cadr files)
                         (read-file-name "file: "
                                         (dired-dwim-target-directory)))))
            (if (file-newer-than-file-p file1 file2)
                (ediff-files file2 file1)
              (ediff-files file1 file2))
            (add-hook 'ediff-after-quit-hook-internal
                      (lambda ()
                        (setq ediff-after-quit-hook-internal nil)
                        (set-window-configuration wnd))))
        (error "No more than 2 files should be marked"))))

  (setq dired-listing-switches "-AlXh --group-directories-first")
  (setq wdired-allow-to-change-permissions t)
  (setq directory-free-space-args "-Pmh")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)

  (add-hook 'dired-mode-hook 'auto-revert-mode))

;; ——

(use-package dired-x
  :defer t
  :after dired
  :bind
  (:map dired-mode-map
        ("z" . dired-omit-mode))
  :config

  (setq-default dired-omit-files-p t)
  (setq dired-omit-verbose nil)
  (setq dired-omit-files
        (format "\\(?:\\.%s\\'\\)\\|%s\\|\\`\\.[^.]"
                (regexp-opt
                 '("pickle"))
                (regexp-opt
                 '("__pycache__")))))

;; ——

(use-package dired-narrow
  :defer t
  :after dired
  :bind
  (:map dired-mode-map
        ("/" . dired-narrow-fuzzy)))

;; ——

(use-package dired-collapse
  :defer t
  :after dired
  :commands (dired-collapse-mode)
  :init
  (add-hook 'dired-mode-hook #'dired-collapse-mode))

;; ——

(req-package dired-subtree
  :defer t
  :after dired
  :require dired

  :bind
  (:map dired-mode-map
        ("<enter>"        . my/dwim-toggle-or-open)
        ("<return>"       . my/dwim-toggle-or-open)
        ("<tab>"          . my/dwim-toggle-or-open))

  :config
  (setq dired-subtree-use-backgrounds nil)
  (setq dired-subtree-line-prefix (lambda (depth) (make-string (* 2 depth) ?\s)))

  (defun my/dwim-toggle-or-open ()
    "Toggle subtree or open the file."
    (interactive)
    (if (file-directory-p (dired-get-file-for-visit))
        (progn
          (dired-subtree-toggle)
          (revert-buffer))
      (dired-find-file))))

;; ——

(use-package dired-sidebar
  :defer t
  :bind
  (("<f10>" . dired-sidebar-toggle-sidebar))
  :config
  (setq dired-sidebar-width 30)
  (add-to-list 'dired-sidebar-special-refresh-commands 'dired-subtree-cycle)
  (add-to-list 'dired-sidebar-special-refresh-commands 'dired-subtree-toggle))

;; ——

(req-package hl-anything
  :defer t
  :after dired-sidebar
  :require dired-sidebar

  :init
  (add-hook 'dired-sidebar-mode-hook #'hl-line-mode))

;; ——

(provide 'cfg-dired)

;;; ————————————————————————————————————————————————————————
;;; cfg-dired.el ends here
