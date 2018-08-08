;;; Package --- Summary
;;; General startup settings.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(menu-bar-mode -1)
(tool-bar-mode -1)
(auto-fill-mode -1)
(cua-selection-mode +1)
(which-function-mode +1)
(global-auto-revert-mode +1)
(minibuffer-depth-indicate-mode +1)

(prefer-coding-system 'utf-8)

(setq-default truncate-lines t
              indent-tabs-mode nil
              global-visual-line-mode t
              show-trailing-whitespace t)

(setq kill-whole-line t
      column-number-mode t
      auto-revert-verbose nil
      mouse-drag-copy-region t
      inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-startup-buffer-menu t
      history-delete-duplicates t
      minibuffer-message-timeout 0.1
      enable-recursive-minibuffers t
      confirm-nonexistent-file-or-buffer t
      text-quoting-style 'grave
      enable-local-variables :safe)

(setq initial-scratch-message ";;; Scratch that.\n\n")

;; Font
;; (add-to-list 'default-frame-alist '(font . "Source Code Pro-9"))
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-9"))

;; Initial frame size and position
;; (add-to-list 'initial-frame-alist '(fullscreen . fullheight))
(add-to-list 'initial-frame-alist '(width  . 130))
(add-to-list 'initial-frame-alist '(height . 70))
(add-to-list 'initial-frame-alist '(top    . 0))
(add-to-list 'initial-frame-alist '(left   . 0))

;; Default frame size and position
;; (add-to-list 'default-frame-alist '(fullscreen . fullheight))
(add-to-list 'default-frame-alist '(width  . 130))
(add-to-list 'default-frame-alist '(height . 70))
(add-to-list 'default-frame-alist '(top    . 0))
(add-to-list 'default-frame-alist '(left   . 0))

;; Set the auto-fill column
(setq-default fill-column 100)

;; To avoid dead circumflex issue
;; (load-library "iso-transl")

;; Change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Ignored extensions during completion
(add-to-list 'completion-ignored-extensions '".bak")
(add-to-list 'completion-ignored-extensions '"~")
(add-to-list 'completion-ignored-extensions '"#")
(add-to-list 'completion-ignored-extensions '".o")
(add-to-list 'completion-ignored-extensions '".obj")
(add-to-list 'completion-ignored-extensions '".lib")
(add-to-list 'completion-ignored-extensions '".elc")
(add-to-list 'completion-ignored-extensions '".exe")

;; Update time-stamp on save
(add-hook 'write-file-hooks 'time-stamp)

;; Activate history saving
(require 'savehist)
(savehist-mode 1)

(provide 'startup)

;;; ————————————————————————————————————————————————————————
;;; startup.el ends here
