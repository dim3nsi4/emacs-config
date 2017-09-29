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
        ("<mouse-2>"    . my/dired-find-file)
        ("<C-down>"     . my/dired-find-file)
        ("RET"          . my/dired-find-file)
        ("°"            . dired-diff)
        ("="            . my/dired-ediff-files)
        ("<C-up>"       . my/dired-up-directory)
        ("<backspace>>" . my/dired-up-directory)
        ("^"            . my/dired-up-directory)
        ("J"            . my/dired-rsync))

  :init
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'auto-revert-mode)
  (add-hook 'dired-mode-hook #'my/rename-dired-buffer-name)

  :config
  ;; enable dired-find-alternate-file command
  (put 'dired-find-alternate-file 'disabled nil)

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

  (defun my/dired-rsync (dest)
    (interactive
     (list
      (expand-file-name
       (read-file-name
        "Rsync to:"
        (dired-dwim-target-directory)))))
    ;; store all selected files into "files" list
    (let ((files (dired-get-marked-files
                  nil current-prefix-arg))
          ;; the rsync command
          (tmtxt/rsync-command
           "rsync -arvz --progress "))
      ;; add all selected file names as arguments
      ;; to the rsync command
      (dolist (file files)
        (setq tmtxt/rsync-command
              (concat tmtxt/rsync-command
                      (shell-quote-argument file)
                      " ")))
      ;; append the destination
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument dest)))
      ;; run the async shell command
      (async-shell-command tmtxt/rsync-command "*rsync*")
      ;; finally, switch to that window
      (other-window 1)))

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

(use-package dired-filter
  :defer t
  :after dired
  :config
  (setq dired-filter-inherit-filter-stack t)
  (add-hook 'dired-mode-hook 'dired-filter-mode))

;; ——

(use-package dired-narrow
  :defer t
  :after dired
  :bind
  (:map dired-mode-map
        ("@"   . dired-narrow-fuzzy)
        ("/ @" . dired-narrow-fuzzy)
        ("/ l" . dired-narrow-fuzzy))
  :config
  (setq dired-narrow-exit-when-one-left t))

;; ——

(use-package dired-collapse
  :defer t
  :after dired)
  ;; :commands (dired-collapse-mode)
  ;: init
  ;; (add-hook 'dired-mode-hook #'dired-collapse-mode))

;; ——

(req-package dired-subtree
  :defer t
  :after dired
  :require dired

  :bind
  (:map dired-mode-map
        ("TAB" . my/dwim-toggle-or-open))

  :config
  (defun my/dwim-toggle-or-open ()
    "Toggle subtree or open the file."
    (interactive)
    (if (file-directory-p (dired-get-file-for-visit))
        (progn
          (dired-subtree-toggle)
          (revert-buffer))
      (dired-find-file)))
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

  :config
  (setq dired-sidebar-width 35)
  (add-to-list 'dired-sidebar-special-refresh-commands 'dired-subtree-cycle)
  (add-to-list 'dired-sidebar-special-refresh-commands 'dired-subtree-toggle)
  (define-key dired-sidebar-mode-map [remap dired-find-alternate-file] 'dired-sidebar-find-file)
  (define-key dired-sidebar-mode-map [remap my/dired-find-file]        'dired-sidebar-find-file)
  (define-key dired-sidebar-mode-map [remap my/dired-up-directory]     'dired-sidebar-up-directory))

;; ——

(req-package dired-open
  :defer t
  :after dired
  :require dired
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
