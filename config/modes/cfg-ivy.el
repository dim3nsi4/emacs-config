;;; Package --- Summary
;;; ivy configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package ivy
  :demand
  :diminish ivy-mode

  :commands
  (ivy-mode)

  :hook
  (minibuffer-setup . my/ivy-disable-trailing-whitespace)

  :bind
  (("C-x C-b" . ivy-switch-buffer)
   ("C-c C-r" . ivy-resume)

   :map ivy-minibuffer-map
   ("<escape>"   . keyboard-escape-quit)
   ("<M-return>" . ivy-call)
   ("<return>"   . ivy-alt-done)
   ("<prior>"    . ivy-scroll-down-command)
   ("<next>"     . ivy-scroll-up-command)
   ("<C-next>"   . ivy-end-of-buffer)
   ("<C-prior>"  . ivy-beginning-of-buffer)
   ("<C-up>"     . ivy-previous-history-element)
   ("<C-down>"   . ivy-next-history-element))

  :config
  (ivy-mode 1)

  (defun my/ivy-disable-trailing-whitespace()
    (setq show-trailing-whitespace nil))

  (setq ivy-height 11
        ivy-fixed-height-minibuffer t
        ivy-use-selectable-prompt nil
        ivy-wrap t
        ivy-action-wrap t
        ivy-use-virtual-buffers t
        ivy-format-function 'ivy-format-function-line
        ivy-count-format ""
        ivy-extra-directories nil
        ivy-ignore-buffers '("\\` " "\\*.+" "^\\:.+$")
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

;; ——

(use-package ivy-rich
  :after ivy

  :config
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-abbreviate-paths t
        ivy-rich-switch-buffer-name-max-length 40
        ivy-rich-switch-buffer-project-max-length 15
        ivy-rich-switch-buffer-mode-max-length 20
        ivy-rich-switch-buffer-align-virtual-buffer t)

  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

;; ——

(use-package ivy-xref
  :after ivy

  :commands
  (ivy-xref-show-xrefs)

  :config
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; ——

(use-package hydra
  :after ivy

  :commands
  (ivy--actionp
   ivy-done)

  :bind
  (:map ivy-minibuffer-map
        ("C-o" . my/hydra-ivy))

  :config
  (defun my/hydra-ivy ()
    "Select one of the available actions and call `ivy-done'."
    (interactive)
    (let* ((actions (ivy-state-action ivy-last)))

      (if (null (ivy--actionp actions))
          (ivy-done)
        (funcall
         (eval
          `(defhydra my/hydra-ivy-read-action (:color teal :hint nil)
             "\nAction %s(ivy-action-name)\n"

             ,@(mapcar (lambda (x) (list (nth 0 x) `(progn (ivy-set-action ',(nth 1 x)) (ivy-done)) (nth 2 x))) (cdr actions))

             ("<left>"     ivy-prev-action         :exit nil)
             ("<right>"    ivy-next-action         :exit nil)
             ("<prior>"    ivy-scroll-down-command :exit nil)
             ("<next>"     ivy-scroll-up-command   :exit nil)
             ("<C-prior>"  ivy-beginning-of-buffer :exit nil)
             ("<C-next>"   ivy-end-of-buffer       :exit nil)
             ("<return>"   ivy-done                :exit t)
             ("<M-return>" ivy-call                :exit nil)
             ("C-j"        ivy-alt-done            :exit nil)
             ("C-M-j"      ivy-immediate-done      :exit nil)
             ("<up>"       ivy-previous-line       :exit nil)
             ("<down>"     ivy-next-line           :exit nil)
             ("C-o"        nil                     :exit t)
             ("<escape>"   nil                     :exit t)
             ("C-g"        nil                     :exit t))))))))

;; ——

(provide 'cfg-ivy)

;;; ————————————————————————————————————————————————————————
;;; cfg-ivy.el ends here
