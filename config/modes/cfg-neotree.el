;;; Package --- Summary
;;; neotree configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package neotree
  :bind
  (("<S-f10>" . neotree-toggle))

  :defines
  (neo-smart-open
   neo-autorefresh
   neo-window-fixed-size
   neo-window-position
   neo-confirm-delete-directory-recursively
   neo-confirm-change-root
   neo-window-width
   neo-confirm-kill-buffers-for-files-in-directory
   neo-theme)

  :config
  (setq neo-smart-open t
        neo-autorefresh nil
        neo-window-fixed-size t
        neo-window-position 'left
        neo-confirm-delete-directory-recursively 'off-p
        neo-confirm-change-root 'off-p
        neo-window-width 30
        neo-confirm-kill-buffers-for-files-in-directory 'off-p
        neo-theme 'icons))

;; ——

(use-package hl-anything
  :requires neotree

  :hook
  (neotree-mode-hook . hl-line-mode))

;; ——

(use-package projectile
  :after neotree

  :commands
  (projectile-project-root
   neotree-hide
   neotree-show
   neotree-dir
   neotree-find)

  :bind
  (:map projectile-mode-map
        ("<S-f10>" . my/neotree-project-dir-toggle))

  :config
  (defun my/neotree-project-dir-toggle ()
    "Open NeoTree using the project root, using find-file-in-project,
or the current buffer directory."
    (interactive)
    (let ((project-dir
           (ignore-errors
             ;; Pick one: projectile or find-file-in-project
             (projectile-project-root)
             ;; (ffip-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name)))))))

;; ——

(provide 'cfg-neotree)

;;; ————————————————————————————————————————————————————————
;;; cfg-neotree.el ends here
