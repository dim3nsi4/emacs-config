;;; Package --- Summary
;;; vimish-fold configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package vimish-fold
  :commands
  (vimish-fold-global-mode)

  :bind
  (("M-RET"   . vimish-fold-toggle)
   ("C-c f"   . nil)
   ("C-c f f" . vimish-fold)
   ("C-c f d" . vimish-fold-delete)
   ("C-c f o" . vimish-fold-unfold-all)
   ("C-c f c" . vimish-fold-refold-all)
   ("C-c f D" . vimish-fold-delete-all)
   ("C-c f b" . my/vimish-fold-next-block))

  :init
  (vimish-fold-global-mode 1)

  :config
  (defun my/vimish-fold-next-block (n)
    "Find the next N custom folding delimiters and create the corresponding fold."
    (interactive "p")

    (catch 'done
      (while (>= (setq n (1- n)) 0)
        (when (eobp) (throw 'done nil))

        (let (p1 p2)
          ;; move to the previous fold delimiter (end or start), in case
          ;; we are inside a region to fold.
          (re-search-backward   "[ ]*\\s<+[ ]*—+[ ]*\\(.*\\)\n" nil t)
          (move-beginning-of-line nil)

          ;; go to the end of the region to fold, mark it.
          (re-search-forward   "[ ]*\\s<+[ ]*—+[ ]*\\(.*\\)\n\\(.*\n\\)*?[ ]*\\s<+[ ]*—+[ ]*\\[end\\][ ]*\\1$" nil t)
          (move-beginning-of-line 2)
          (setq p2 (point))

          ;; go back to the start of the region to fold, mark it.
          (re-search-backward  "[ ]*\\s<+[ ]*—+[ ]*\\(.*\\)\n\\(.*\n\\)*?[ ]*\\s<+[ ]*—+[ ]*\\[end\\][ ]*\\1$" nil t)
          (move-beginning-of-line nil)
          (setq p1 (point))

          ;; fold the region between p1 and p2
          (condition-case nil
              (vimish-fold p1 p2)
            ((debug error) nil))

          ;; go back to the end of the region we just folded.
          (goto-char p2))))))

;; ——

(use-package hydra
  :requires vimish-fold

  :commands
  (hydra--call-interactively-remap-maybe
   hydra-default-pre
   hydra-idle-message
   hydra-keyboard-quit
   hydra-set-transient-map)

  :bind
  (:map global-map
        ("C-c f" . my/hydra-vimish-fold/body))

  :config
  (defhydra my/hydra-vimish-fold (:color teal :hint nil :idle 0.25)
    "
Vimish fold

[_f_] create fold         [_t_] toggle fold    [_b_] fold next block
[_d_] delete fold         [_o_] unfold-all     [_q_] quit
[_D_] delete all folds    [_c_] refold-all
    "
    ("q" nil :exit t)
    ("<escape>" nil :exit t)
    ("RET" vimish-fold-toggle)

    ("t" vimish-fold-toggle)
    ("f" vimish-fold)
    ("d" vimish-fold-delete)
    ("o" vimish-fold-unfold-all)
    ("c" vimish-fold-refold-all)
    ("b" my/vimish-fold-next-block)
    ("D" vimish-fold-delete-all)))

;; ——

(provide 'cfg-vimish-fold)

;;; ————————————————————————————————————————————————————————
;;; cfg-vimish-fold.el ends here
