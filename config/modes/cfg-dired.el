;;; Package --- Summary
;;; dired configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(req-package dired
  :defer t
  :require ediff
  :commands (dired-dwim-target-directory)

  :bind
  (:map dired-mode-map
        ("<C-down>" . dired-find-alternate-file)
        ("RET"      . dired-find-alternate-file)
        ("°"        . dired-diff)
        ("="        . my/dired-ediff-files))

  :init
  (add-hook 'dired-mode-hook #'my/rename-dired-buffer-name)
  (add-hook 'dired-mode-hook (lambda ()
                               (define-key dired-mode-map (kbd "<C-up>")      (lambda () (interactive) (find-alternate-file "..")))
                               (define-key dired-mode-map (kbd "<backspace>") (lambda () (interactive) (find-alternate-file "..")))
                               (define-key dired-mode-map (kbd "^")           (lambda () (interactive) (find-alternate-file "..")))))

  :config
  ;; enable dired-find-alternate-file command
  (put 'dired-find-alternate-file 'disabled nil)

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

  (defun my/rename-dired-buffer-name ()
    "Rename the dired buffer name to distinguish it from file buffers.
     It adds extra strings at the front and back of the default
     dired buffer name."
    (let ((name (buffer-name)))
      (if (not (string-match "/$" name))
          (rename-buffer (concat "*Dired* " name "/") t))))

  (defun my/dired-do-eshell-command (command)
  "Run an Eshell COMMAND on the marked files."
  (interactive "sEshell command: ")
  (let ((files (dired-get-marked-files t)))
    (eshell-command
     (format "%s %s" command (mapconcat #'identity files " ")))))

  (setq dired-listing-switches "-Alvh1 --group-directories-first")
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
        ("<tab>" . dired-subtree-toggle))

  :config
  (setq dired-subtree-use-backgrounds nil))

;; ——

(req-package dired-avfs
  :defer t
  :after dired
  :require dired)

;; ——

(use-package dired-sidebar
  :defer t
  :bind
  (("<f10>" . dired-sidebar-toggle-sidebar))

  :init
  (add-hook 'dired-sidebar-mode-hook (lambda ()
                                       (define-key dired-sidebar-mode-map (kbd "^")           'dired-sidebar-up-directory)
                                       (define-key dired-sidebar-mode-map (kbd "<backspace>") 'dired-sidebar-up-directory)
                                       (define-key dired-sidebar-mode-map (kbd "<C-up>")      'dired-sidebar-up-directory)
                                       (define-key dired-sidebar-mode-map (kbd "<C-down>")    'dired-sidebar-find-file)
                                       (define-key dired-sidebar-mode-map (kbd "RET")         'dired-sidebar-find-file)))

  :config
  (setq dired-sidebar-width 35)
  (add-to-list 'dired-sidebar-special-refresh-commands 'dired-subtree-cycle)
  (add-to-list 'dired-sidebar-special-refresh-commands 'dired-subtree-toggle))

;; ——

(req-package dired-open
  :defer t
  :after dired
  :require dired
  :config
  (setq dired-open-extensions '(("html" . "firefox")
                                ( "mp4" . "smplayer")
                                ( "mkv" . "smplayer")
                                ( "avi" . "smplayer"))))

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
