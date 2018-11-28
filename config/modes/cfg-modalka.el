;;; Package --- Summary
;;; modalka configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package modalka
  :requires key-chord

  :defines
  (modalka-cursor-type
   modalka-excluded-modes
   modalka-mode-map
   modalka-mode)

  :chords
  ("xx" . modalka-mode)

  :hook
  (post-command . my/set-powerline-color-according-to-mode)

  :bind
  (:map modalka-mode-map
   ("q"   . modalka-mode)
   ("SPC" . cua-set-mark)
   ("1"   . delete-other-windows)
   ("2"   . split-window-vertically)
   ("3"   . split-window-horizontally)
   ("d"   . delete-char)
   ("z"   . undo)
   ("%"   . er/contract-region)
   ("="   . er/expand-region)
   ("w"   . my/kill-ring-save)
   ("k"   . kill-line)
   ("«"   . mc/mark-previous-like-this)
   ("»"   . mc/mark-next-like-this)
   ("<"   . mc/unmark-previous-like-this)
   (">"   . mc/unmark-next-like-this)
   ("-"   . drag-stuff-down)
   ("+"   . drag-stuff-up)
   ("y"   . cua-paste)
   ("."   . repeat)
   ("p"   . my/hydra-projectile/body)
   ("f"   . my/hydra-vimish-fold/body))

  :config
  (setq modalka-cursor-type 'hollow)

  ;; Change powerline background color according to mode
  (defvar my/set-powerline-color-bg-color "")
  (defvar my/set-powerline-color-fg-color "")
  (defvar my/set-powerline-color-buffer "")

  (defun my/set-powerline-color-according-to-mode ()
    "Change powerline color according to some minor modes."
    (let ((bg-color (cond (modalka-mode "azure4") (t "gray95")))
          (fg-color (cond (modalka-mode "white") (t "black"))))

      (unless (and (string= bg-color my/set-powerline-color-bg-color)
                   (string= (buffer-name) my/set-powerline-color-buffer))

        (setq my/set-powerline-color-fg-color fg-color)
        (setq my/set-powerline-color-bg-color bg-color)
        (setq my/set-powerline-color-buffer (buffer-name))

        (set-face-foreground 'spaceline-highlight-face fg-color)
        (set-face-background 'spaceline-highlight-face bg-color)
        (set-face-foreground 'mode-line fg-color)
        (set-face-background 'mode-line bg-color))))

  (add-to-list 'modalka-excluded-modes 'magit-status-mode))

(provide 'cfg-modalka)

;;; ————————————————————————————————————————————————————————
;;; cfg-modalka.el ends here
