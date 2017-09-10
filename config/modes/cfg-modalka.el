;;; Package --- Summary
;;; modalka configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(req-package modalka
  :defer t
  :require key-chord

  :chords
  ("xx" . modalka-mode)

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
   ("w"   . kill-ring-save)
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
  (defvar my/set-powerline-color-color "")
  (defvar my/set-powerline-color-buffer "")

  (defun my/set-powerline-color-according-to-mode ()
    "Change powerline color according to some minor modes."
    (let ((color (cond (modalka-mode "gray70")
                       (t "gray95"))))
      (unless (and (string= color my/set-powerline-color-color)
                   (string= (buffer-name) my/set-powerline-color-buffer))
        (setq my/set-powerline-color-color color)
        (set-face-background 'spaceline-highlight-face color)
        (set-face-background 'mode-line color)
        ;; (set-face-background 'powerline-active1 color)
        ;; (set-face-background 'powerline-active2 color)
        (setq my/set-powerline-color-buffer (buffer-name)))))

  (add-hook 'post-command-hook #'my/set-powerline-color-according-to-mode)

  (add-to-list 'modalka-excluded-modes 'magit-status-mode))

(provide 'cfg-modalka)

;;; ————————————————————————————————————————————————————————
;;; cfg-modalka.el ends here
