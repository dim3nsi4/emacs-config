;;; Package --- Summary
;;; dired configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package dired
  :diminish

  :commands
  (dired-dwim-target-directory
   dired-get-file-for-visit
   dired-find-alternate-file
   dired-find-file
   dired-get-marked-files)

  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . auto-revert-mode)
   (dired-mode . my/rename-dired-buffer-name))

  :bind
  (("<f7>"   . my/dired-default-directory)
   ("<S-f7>" . dired)
   :map dired-mode-map
        ;;; ("<mouse-2>"    . my/dired-find-file)
        ("<C-down>"    . my/dired-find-file)
        ("RET"         . my/dired-find-file)
        ("°"           . dired-diff)
        ("="           . my/dired-ediff-files)
        ("<C-up>"      . my/dired-up-directory)
        ("<backspace>" . my/dired-up-directory)
        ("^"           . my/dired-up-directory))

  :config
  ;; enable dired-find-alternate-file command
  (put 'dired-find-alternate-file 'disabled nil)

  (defun my/dired-default-directory ()
    "Run dired on the default directory."
    (interactive)
    (dired default-directory))

  (defun my/dired-up-directory ()
    "Use find-alternate-file to open the parent directory."
    (interactive)
    (find-alternate-file ".."))

  (defun my/dired-find-file ()
    "Use dired-find-alternate-file if its a directory, otherwise use find-file."
    (interactive)
    (if (file-directory-p (dired-get-file-for-visit))
        (progn
          (dired-find-alternate-file)
          (revert-buffer))
      (dired-find-file)))

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

  (setq dired-listing-switches "-Alvh1 --group-directories-first --dereference"
        wdired-allow-to-change-permissions t
        directory-free-space-args "-Pmh"
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-dwim-target t)

  ;; dired-x
  (setq dired-omit-verbose nil)
  (setq dired-omit-files
        (format "\\(?:\\.%s\\'\\)\\|%s\\|\\`\\.[^.]"
                (regexp-opt
                 '("pickle"))
                (regexp-opt
                 '("__pycache__")))))

;; ——

(use-package dired-rsync
  :after dired
  :diminish

  :bind
  (:map dired-mode-map
        ("J" . dired-rsync)))

;; ——

(use-package dired-filter
  :after dired
  :diminish

  :config
  (setq dired-filter-inherit-filter-stack t)
  (add-hook 'dired-mode-hook 'dired-filter-mode))

;; ——

(use-package dired-narrow
  :after dired
  :diminish

  :bind
  (:map dired-mode-map
        ("@"   . dired-narrow-fuzzy)
        ("/ @" . dired-narrow-fuzzy)
        ("/ l" . dired-narrow-fuzzy))

  :config
  (setq dired-narrow-exit-when-one-left t))

;; ——

(use-package dired-collapse
  :after dired
  :diminish)
  ;; :commands (dired-collapse-mode)
  ;; :init
  ;; (add-hook 'dired-mode-hook #'dired-collapse-mode))

;; ——

(use-package dired-subtree
  :after (:any dired dired-sidebar)
  :diminish

  :commands
  (dired-subtree-toggle)

  :bind
  (:map dired-mode-map
        ("<mouse-2>" . my/dired-dwim-toggle-or-open)
        ("TAB"       . my/dired-dwim-toggle-or-open))

  :config
  (defun my/dired-dwim-toggle-or-open ()
    "Toggle subtree or open the file."
    (interactive)
    (if (file-directory-p (dired-get-file-for-visit))
        (progn
          (dired-subtree-toggle)
          (revert-buffer))
      (dired-open-file)))
  (setq dired-subtree-use-backgrounds nil))

;; ——

(use-package dired-avfs
  :after dired
  :diminish)

;; ——

(use-package dired-sidebar
  :diminish

  :bind
  (("<f10>" . dired-sidebar-toggle-sidebar))

  :config
  (setq dired-sidebar-width 35)

  (add-to-list 'dired-sidebar-special-refresh-commands 'dired-subtree-cycle)
  (add-to-list 'dired-sidebar-special-refresh-commands 'dired-subtree-toggle)

  (define-key dired-sidebar-mode-map [remap dired-find-alternate-file] 'dired-sidebar-find-file)
  (define-key dired-sidebar-mode-map [remap my/dired-find-file]        'dired-sidebar-find-file)
  (define-key dired-sidebar-mode-map [remap my/dired-up-directory]     'dired-sidebar-up-directory))

;; ——

(use-package dired-open
  :after dired

  :config
  (define-key dired-mode-map [remap dired-find-file]           'dired-open-file)
  (define-key dired-mode-map [remap dired-find-alternate-file] 'dired-open-file)
  (define-key dired-mode-map [remap my/dired-find-file]        'dired-open-file)

  (setq dired-open-find-file-function 'my/dired-find-file)
  (setq dired-open-extensions '(("png"  . "eog")
                                ("jpg"  . "eog")
                                ("jpeg" . "eog")
                                ("tif"  . "eog")
                                ("tiff" . "eog")
                                ("pdf"  . "evince")
                                ("html" . "firefox")
                                ( "mp3" . "smplayer")
                                ( "mp4" . "smplayer")
                                ( "mkv" . "smplayer")
                                ( "avi" . "smplayer"))))

;; ——

(use-package dired-du
  :defer t
  :after dired
  :diminish

  ;; :init
  ;; (add-hook 'dired-mode-hook #'dired-du-mode)

  :config
  (setq dired-du-bind-human-toggle t
        dired-du-update-headers t
        dired-du-size-format t))

;; ——

(use-package hl-anything
  :after dired-sidebar
  :requires dired-sidebar

  :hook
  (dired-sidebar-mode . hl-line-mode))

;; ——

(provide 'cfg-dired)

;;; ————————————————————————————————————————————————————————
;;; cfg-dired.el ends here
