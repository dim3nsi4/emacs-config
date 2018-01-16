;;; Package --- Summary
;;; Generic keybindings.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

;;; -*- lexical-binding: t -*-

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-.") 'repeat)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x w") 'delete-frame)
(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "<f3>") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "<S-f3>") 'kmacro-end-macro)
(global-set-key (kbd "<f4>") 'eshell)
(global-set-key (kbd "<f5>") 'query-replace)
(global-set-key (kbd "<S-f5>") 'query-replace-regexp)
(global-set-key (kbd "<f6>") 'align-regexp)
(global-set-key (kbd "<S-f6>") 'align-repeat)
(global-set-key (kbd "<f7>") (lambda () (interactive) (dired default-directory)))
(global-set-key (kbd "<S-f7>") 'dired)
(global-set-key (kbd "<f8>") 'align)
(global-set-key (kbd "<f9>") 'imenu)
(global-set-key (kbd "<f12>") 'package-list-packages)
(global-set-key (kbd "<S-f12>") 'package-list-packages-no-fetch)

(global-set-key (kbd "<backtab>") (lambda () (interactive) (switch-to-buffer (other-buffer))))
(global-set-key (kbd "<C-tab>") 'next-buffer)

(global-set-key (kbd "M-SPC") 'dabbrev-expand) ; swapped with just-one-space "M-/"
(global-set-key (kbd "M-/") 'just-one-space)   ; swapped with dabbrev-expand "M-SPC"
(global-set-key (kbd "C-/") 'delete-horizontal-space)

(global-set-key (kbd "<mouse-3>") 'ignore)
(global-set-key (kbd "<S-mouse-3>") 'mouse-appearance-menu)
(global-set-key (kbd "<S-down-mouse-1>") 'ignore) ; switched to right click
(global-set-key (kbd "<S-mouse-1>") 'ignore)

(global-set-key (kbd "C-c") 'nil)

;; switch comment-dwim and xref-pop-marker-stack
(global-set-key (kbd "M-;") 'xref-pop-marker-stack)
(global-set-key (kbd "M-,") 'comment-dwim)

;; Move between windows using meta instead of ctrl
(windmove-default-keybindings 'meta)

;; Commenting
(global-set-key (kbd "<C-down>") 'my/comment-line-and-go-down)
(global-set-key (kbd "<C-up>")   'my/go-up-and-comment-line)

;; Beginning and end of buffer
(global-set-key (kbd "<C-prior>") 'beginning-of-buffer)
(global-set-key (kbd "<C-next>")  'end-of-buffer)

;; Mode specific keybindings
(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
(define-key emacs-lisp-mode-map (kbd "<f8>") 'my/byte-compile-this-file)

(provide 'keys)

;;; ————————————————————————————————————————————————————————
;;; keys.el ends here
