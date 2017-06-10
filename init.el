;;; Package --- Summary
;;; Emacs main configuration file
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-06-10 13:10:41 seimandp>
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

;;; —————————————————————————————————————— packages archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu"   . "http://elpa.gnu.org/packages/"))
(package-initialize)
;;; ———————————————————————————————— [end] packages archives

;;; ———————————————————————————————————————————— use-package
(require 'use-package)
;;; —————————————————————————————————————— [end] use-package

;;; ——————————————— utility function: byte-compile-this-file
(defun my/byte-compile-this-file ()
  "Compile the file the buffer is visiting."
  (interactive)
  (byte-compile-file (expand-file-name buffer-file-name)))
;;; ————————— [end] utility function: byte-compile-this-file

;;; ———————————————————— before-save-hook & write-file-hooks
(add-hook 'write-file-hooks 'time-stamp)
;;; —————————————— [end] before-save-hook & write-file-hooks

;;; ———————————————————————————————————————————————— backups
(setq backup-by-copying t     ; copy all files, don't rename them.
      delete-old-versions t   ; don't ask to delete excess backup versions.
      kept-new-versions 10    ; number of newest versions to keep.
      kept-old-versions 0     ; number of oldest versions to keep.
      version-control t       ; use version numbers for backups
      vc-make-backup-files t) ; backup version controlled files

;; Directory for the backup per save
(setq backup-directory-alist '(("." . "~/.backups/per-save")))

(defun my/force-backup-of-buffer ()
  "Make a special \"per session\" backup at the first save of each emacs session."
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.backups/per-session")))
          (kept-new-versions 5))
      (backup-buffer)))

  ;; Make a "per save" backup on each save. The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering of
  ;; per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook 'my/force-backup-of-buffer)

;; Disable lock file creation (#* files)
(setq create-lockfiles nil)
;;; —————————————————————————————————————————— [end] backups

;;; ————————————————————————————————————————————— completion
(add-to-list 'completion-ignored-extensions '".bak")
(add-to-list 'completion-ignored-extensions '"~")
(add-to-list 'completion-ignored-extensions '"#")
(add-to-list 'completion-ignored-extensions '".o")
(add-to-list 'completion-ignored-extensions '".obj")
(add-to-list 'completion-ignored-extensions '".lib")
(add-to-list 'completion-ignored-extensions '".elc")
(add-to-list 'completion-ignored-extensions '".exe")
;;; ——————————————————————————————————————— [end] completion

;;; ——————————————————————————————————————— startup settings
;; Don't show the startup message & screen on launch
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)

;; By default, don't show the menu bar
;; (can be re-enabled with C-c t m)
(menu-bar-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

;; Don't wrap lines
(set-default 'truncate-lines t)

;; Show the column number
(setq column-number-mode t)

;; No tabulations, inserts spaces instead
(setq-default indent-tabs-mode nil)

;; Get some stuff from the CUA mode (mainly, the rectangular selection)
(cua-selection-mode t)

;; C-k kills the whole line (including the EOL marker) if the cursor
;; is at the beginning
(setq kill-whole-line t)

;; No duplicates allowed in the history
(setq history-delete-duplicates t)

;; Auto refresh buffers when files changed on disk
(global-auto-revert-mode t)

;; turn off auto revert messages
(setq auto-revert-verbose nil)

;; Default font
;; (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-8"))
(add-to-list 'default-frame-alist '(font . "Source Code Pro-8"))

;; Initial frame size and position
(add-to-list 'default-frame-alist '(fullscreen . fullheight))
(add-to-list 'default-frame-alist '(width  . 131))
(add-to-list 'default-frame-alist '(height . 81))
(add-to-list 'default-frame-alist '(top    . 0))
(add-to-list 'default-frame-alist '(left   . 0))

;; Load the theme
(load-theme 'clearview-light t)

;; To avoid dead circumflex issue
(load-library "iso-transl")

;; Mouse selection also copy to the system clipboard
(setq mouse-drag-copy-region t)

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Confirmation when asking for a non existing buffer or file
(setq confirm-nonexistent-file-or-buffer t)

;; Enable minibuffer call in minibuffer
(setq enable-recursive-minibuffers t)

;; Disable blinking cursor
(blink-cursor-mode 0)
;;; ————————————————————————————————— [end] startup settings

;;; ———————————————————————————————————— general keybindings
(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x w") 'delete-frame)

(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "<f4>") 'eshell)
(global-set-key (kbd "<f5>") 'query-replace)
(global-set-key (kbd "<S-f5>") 'query-replace-regexp)
(global-set-key (kbd "<f6>") 'align-regexp)
(global-set-key (kbd "<S-f6>") 'align-repeat)
(global-set-key (kbd "<f7>") 'dired)
(global-set-key (kbd "<S-f7>") (lambda () (interactive) (dired default-directory)))
(global-set-key (kbd "<f8>") 'my/byte-compile-this-file)
(global-set-key (kbd "<f9>") 'imenu)
(global-set-key (kbd "<f12>") 'package-list-packages)
(global-set-key (kbd "<S-f12>") 'package-list-packages-no-fetch)

(global-set-key (kbd "<backtab>") (lambda () (interactive) (switch-to-buffer (other-buffer))))
(global-set-key (kbd "<C-tab>") 'next-buffer)

(global-set-key (kbd "M-SPC") 'dabbrev-expand) ; swapped with just-one-space "M-/"
(global-set-key (kbd "M-/") 'just-one-space) ; swapped with dabbrev-expand "M-SPC"

(global-set-key (kbd "<mouse-3>") 'ignore)
(global-set-key (kbd "<S-mouse-3>") 'mouse-appearance-menu)
(global-set-key (kbd "<S-down-mouse-1>") 'ignore) ; switched to right click
(global-set-key (kbd "<S-mouse-1>") 'ignore)

(global-set-key (kbd "C-c") 'nil)
(global-set-key (kbd "C-c C-t C-m") 'menu-bar-mode)
(global-set-key (kbd "C-c C-t C-t") 'tool-bar-mode)

;; switch comment-dwim and xref-pop-marker-stack
(global-set-key (kbd "M-;") 'xref-pop-marker-stack)
(global-set-key (kbd "M-,") 'comment-dwim)

;; Move between windows using meta instead of ctrl
(windmove-default-keybindings 'meta)

;; Set ESC as an escape key in isearch mode
(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
;;; ——————————————————————————————— [end] general keybindings

;;; ———————————————————————————————————————————————— paradox
(use-package paradox
  :demand

  :bind
  (("<f12>" . paradox-list-packages))

  :config
  (paradox-enable)
  (setq paradox-github-token "ed3d6985b01470faf1231c471cfe1ac820c171d6"
        paradox-execute-asynchronously t
        paradox-automatically-star nil))
;;; —————————————————————————————————————————— [end] paradox

;;; ————————————————————————————————————————————————— eshell
;; Allows to completely clear the eshell buffer using C-l
(defun my/eshell-clear-buffer ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook '(lambda() (setq show-trailing-whitespace nil)))
(add-hook 'eshell-mode-hook '(lambda() (local-set-key (kbd "C-l") 'my/eshell-clear-buffer)))
;;; ——————————————————————————————————————————— [end] eshell

;;; —————————————————————————————————————————————————— ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Restore window configuration after ediff
(defvar my/ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my/ediff-bwin-reg ?b
  "*Register to be set up to hold `my/ediff-bwin-config' configuration."
  :group 'ediff)

(defvar my/ediff-awin-config nil "Window configuration after ediff.")
(defcustom my/ediff-awin-reg ?e
  "*Register to be used to hold `my/ediff-awin-config' window configuration."
  :group 'ediff)

(defun my/ediff-bsh ()
  "Function to be called before any buffers or window setup for ediff."
  (setq my/ediff-bwin-config (current-window-configuration))
  (when (characterp my/ediff-bwin-reg)
    (set-register my/ediff-bwin-reg
                  (list my/ediff-bwin-config (point-marker)))))

(defun my/ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq my/ediff-awin-config (current-window-configuration))
  (when (characterp my/ediff-awin-reg)
    (set-register my/ediff-awin-reg
                  (list my/ediff-awin-config (point-marker)))))

(defun my/ediff-qh ()
  "Function to be called when ediff quits."
  (when my/ediff-bwin-config
    (set-window-configuration my/ediff-bwin-config)))

(add-hook 'ediff-before-setup-hook 'my/ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my/ediff-ash 'append)
(add-hook 'ediff-quit-hook 'my/ediff-qh)
;;; ———————————————————————————————————————————— [end] ediff

;;; ——————————————————————————————————————————— align-repeat
(defun align-repeat (start end regexp)
  "Between START and END, repeat alignment with respect to the given regular expression REGEXP."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end (concat "\\(\\s-*\\)" regexp) 1 1 t))
;;; ————————————————————————————————————— [end] align-repeat

;;; —————————————————————————————————————————————— scrolling
(setq scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-preserve-screen-position 1
      scroll-down-aggressively 0.01)

;;scroll window up/down by one line
(global-set-key (kbd "<C-S-up>")   (kbd "C-u 1 M-v"))
(global-set-key (kbd "<C-S-down>") (kbd "C-u 1 C-v"))

(use-package view
  :bind
  (("<next>"  . my/half-page-forward)
   ("<prior>" . my/half-page-backward))

  :config
  (defun my/half-page-forward ()
    (interactive)
    (condition-case nil (View-scroll-half-page-forward) (end-of-buffer (goto-char (point-max)))))

  (defun my/half-page-backward ()
    (interactive)
    (condition-case nil (View-scroll-half-page-backward) (beginning-of-buffer (goto-char (point-min))))))
;;; ———————————————————————————————————————— [end] scrolling

;;; ————————————————————————————————————————————— alarm bell
;; Disable the bell when scrolling to limits
(defun my/bell-function ()
  "Custom alarm bell to avoid unnecessary alarms event."
  (unless (memq this-command
                '(isearch-abort
                  abort-recursive-edit
                  exit-minibuffer
                  keyboard-quit
                  mwheel-scroll
                  down
                  up
                  View-scroll-half-page-forward
                  View-scroll-half-page-backward
                  scroll-down
                  scroll-up
                  next-line
                  previous-line
                  backward-char
                  forward-char))
    (ding)))
(setq ring-bell-function 'my/bell-function)

;; Visual bell
(setq visible-bell 1)
;;; ——————————————————————————————————————— [end] alarm bell

;;; ————————————————————————— kill/copy whole line or region
;; M-w saves the current line if no region is selected
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; C-w deletes and saves the current line if no region is selected
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
;;; ——————————————————— [end] kill/copy whole line or region

;;; ————————————————————————————————————————————— commenting
(defvar my/comment-line-last-col nil)

(defun my/toggle-comment-line (n again)
  "Comment or uncomment the next N line(s).
If AGAIN is true, use the same mode as the last call."
  (if comment-start
      (let*(
            (end    (cond ((or (not comment-end) (equal comment-end "")) "")
                          ((string-match "^ " comment-end) comment-end)
                          (t (concat " " comment-end))))
            (start  (cond ((string-match " $" comment-start) comment-start)
                          ((and (= (length comment-start) 1) (equal end ""))
                           (concat comment-start " "))
                          (t (concat comment-start " "))))

            (end    (concat comment-end))
            (start  (concat comment-start))

            (qstart (regexp-quote start))
            (qend   (regexp-quote end))

            (col    (and again my/comment-line-last-col))
            (mode   (and again (if col 'comment 'uncomment)))
            (direction (if (< 0 n) 1 -1))
            (n  (abs n)))

        (catch 'done
          (beginning-of-line)
          (if (< direction 0) (forward-line -1))

          (while (>= (setq n (1- n)) 0)
            (when (eobp) (throw 'done nil))

            (skip-chars-forward "\t ")
            (unless (eolp)
              (unless mode (setq mode (if (looking-at (concat qstart "\\(.*\\)" qend "$")) 'uncomment 'comment)))

              (let ((cur (current-column)))
                (cond ((and col (< col cur))
                       (move-to-column col t))
                      ((eq mode 'comment)
                       (setq col cur))))

              (cond ((eq mode 'comment)
                     (insert start) (end-of-line) (insert end))

                    ((eq mode 'uncomment)
                     (when (looking-at (concat qstart "\\(.*\\)" qend "$"))
                       (replace-match "\\1" t)))))

            (forward-line direction))

          (if (< direction 0)
              (forward-line 1)))

        (setq my/comment-line-last-col col))
    (message "Comments not available for this mode")))

(defun my/comment-line-and-go-down (n)
  "Toggle a comment on current N line(s) (disable line by line)."
  (interactive "p")
  (my/toggle-comment-line (+ n) (eq last-command 'my/comment-line-and-go-down)))

(defun my/go-up-and-comment-line (n)
  "Toggle a comment on current N line(s) (disable line by line)."
  (interactive "p")
  (my/toggle-comment-line (- n) (eq last-command 'my/go-up-and-comment-line)))

(global-set-key (kbd "<C-down>") 'my/comment-line-and-go-down)
(global-set-key (kbd "<C-up>")   'my/go-up-and-comment-line)
;;; ——————————————————————————————————————— [end] commenting

;;; ———————————————————————————————————————————— winner mode
;; Undo/Redo window configuration changes (C-c left, C-c right)
(use-package winner
  :config
  (winner-mode t))
;;; —————————————————————————————————————— [end] winner mode

;;; ————————————————————————————————————— whitespace cleanup
(setq-default show-trailing-whitespace t)

;; Cleanup whitespace if the file was originally clean
(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode

  :config
  (global-whitespace-cleanup-mode))
;;; ——————————————————————————————— [end] whitespace cleanup

;;; ———————————————————————————————————————————— vimish-fold
(use-package vimish-fold
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
          (goto-char p2) ))) ))
;;; —————————————————————————————————————— [end] vimish-fold

;;; ————————————————————————————————————————————— drag stuff
(use-package drag-stuff
  :bind
  (("C-«" . drag-stuff-left)
   ("C-»" . drag-stuff-right)
   ("C-+" . drag-stuff-up)
   ("C--" . drag-stuff-down)))
;;; ——————————————————————————————————————— [end] drag stuff

;;; —————————————————————————————————————————— expand region
(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C-%" . er/contract-region)))
;;; ———————————————————————————————————— [end] expand region

;;; ———————————————————————————————————————————— smartparens
(use-package smartparens-config
  :diminish smartparens-mode

  :config
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (show-smartparens-global-mode +1))
;;; —————————————————————————————————————— [end] smartparens

;;; ————————————————————————————————————— rainbow delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
;; ———————————————————————————————— [end] rainbow delimiters

;;; ——————————————————————————————————————————————— perspeen
(use-package perspeen
  :demand

  :init
  (setq perspeen-keymap-prefix (kbd "C-c t"))

  :bind
  (:map perspeen-mode-map
        ("<C-tab>" . perspeen-tab-next)
        ("C-c t q" . perspeen-tab-del))

  :config
  (setq perspeen-use-tab t)
  (perspeen-mode))
;;; ————————————————————————————————————————— [end] perspeen

;;; ————————————————————————————————————————————— projectile
(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))

  :config
  (projectile-mode))
;;; ——————————————————————————————————————— [end] projectile

;;; —————————————————————————————————————————————— meghanada
(use-package meghanada
  :demand
  :init
  (setq meghanada-mode-key-prefix (kbd "C-c c"))

  :bind
  (:map meghanada-mode-map
        ("M-," . comment-dwim))

  :config
  (add-hook 'java-mode-hook 'meghanada-mode))
;;; ———————————————————————————————————————— [end] meghanada

;;; ——————————————————————————————————————————————————— helm
;; (use-package helm-config
;;   :demand
;;   :diminish helm-mode

;;   :init
;;   (setq helm-command-prefix-key "C-c h")

;;   :bind
;;   (("M-x" . helm-M-x)
;;    ("C-x C-b" . helm-buffers-list)
;;    ("C-x C-f" . helm-find-files)
;;    ("C-x f"   . helm-recentf)
;;    ("C-S-h"   . helm-apropos)
;;    ("<f9>"    . helm-imenu)
;;    ("<S-f9>"  . helm-imenu-in-all-buffers)
;;    ("<f12>"   . helm-list-elisp-packages)
;;    ("<S-f12>" . helm-list-elisp-packages-no-fetch)

;;    :map helm-command-map
;;    ("<escape>" . helm-keyboard-quit))

;;   :config
;;   (helm-mode 1)
;;   (helm-autoresize-mode 1)

;;   (setq helm-autoresize-max-height 25
;;         helm-autoresize-min-height 25
;;         helm-ff-skip-boring-files nil
;;         helm-M-x-fuzzy-match nil
;;         helm-echo-input-in-header-line nil
;;         helm-full-frame nil
;;         helm-display-header-line nil
;;         helm-split-window-default-side 'below
;;         helm-split-window-in-side-p nil))
;;; ————————————————————————————————————————————— [end] helm

;;; ———————————————————————————————————————————————————— ivy
(use-package ivy
  :demand
  :diminish ivy-mode

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

  (setq ivy-height 11
        ivy-fixed-height-minibuffer t
        ivy-wrap t
        ivy-action-wrap t
        ivy-use-virtual-buffers nil
        ivy-format-function 'ivy-format-function-line
        ivy-count-format ""
        ivy-extra-directories nil
        ;; ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

(use-package ivy
  :after magit
  :demand
  :diminish ivy-mode

  :config
  (setq magit-completing-read-function 'ivy-completing-read))

;; (use-package ivy-rich
;;   :after ivy
;;   :demand

;;   :config
;;   (setq ivy-virtual-abbreviate 'full
;;         ivy-rich-abbreviate-paths t
;;         ivy-rich-switch-buffer-name-max-length 55
;;         ivy-rich-switch-buffer-project-max-length 15
;;         ivy-rich-switch-buffer-mode-max-length 25
;;         ivy-rich-switch-buffer-align-virtual-buffer t)

;;   (add-hook 'minibuffer-setup-hook (lambda () (setq show-trailing-whitespace nil)))
;;   (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))
;;; —————————————————————————————————————————————— [end] ivy

;;; ————————————————————————————————————————————————— swiper
(use-package swiper
  :after ivy
  :after counsel

  :bind
  (("M-i" . counsel-grep-or-swiper)))
;;; ——————————————————————————————————————————— [end] swiper

;;; ———————————————————————————————————————————————— counsel
(use-package counsel
  :after ivy
  :diminish counsel-mode

  :bind
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x f"   . counsel-recentf)
   ("C-c g"   . counsel-git)
   ("C-h C-k ". counsel-descbinds)
   ("C-c j"   . counsel-git-grep)
   ("C-c y"   . counsel-yank-pop)
   ("C-c k"   . counsel-ag)
   ("C-x l"   . counsel-locate)
   ("C-x p"   . counsel-package)
   ("<f1> f"  . counsel-describe-function)
   ("<f1> v"  . counsel-describe-variable)
   ("<f1> l"  . counsel-find-library)
   ("<f2> i"  . counsel-info-lookup-symbol)
   ("<f2> u"  . counsel-unicode-char)
   ("<f9>"    . counsel-imenu)
   :map read-expression-map
   ("C-r" . counsel-expression-history))

  :config
  (counsel-mode 1)
  (setq counsel-find-file-ignore-regexp "^\\.\\|~$\\|^#")
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package counsel-gtags
  :after ivy
  :diminish counsel-gtags-mode

  :bind
  (:map counsel-gtags-mode-map
        ("<f2>" . counsel-gtags-dwim))

  :config
  (add-hook 'c-mode-hook    'counsel-gtags-mode)
  (add-hook 'c++-mode-hook  'counsel-gtags-mode)
  (add-hook 'java-mode-hook 'counsel-gtags-mode)

  (setq counsel-gtags-path-style 'relative
        counsel-gtags-ignore-case t
        counsel-gtags-auto-update t))

(use-package counsel-projectile
  :after ivy
  :after projectile

  :bind
  (:map projectile-mode-map
        ([remap projectile-switch-project]   . counsel-projectile-switch-project)
        ([remap projectile-switch-to-buffer] . counsel-projectile-switch-to-buffer)
        ([remap projectile-find-file]        . counsel-projectile-find-file)
        ([remap projectile-find-dir]         . counsel-projectile-find-dir))

  :config
  (setq projectile-completion-system 'ivy))
;;; —————————————————————————————————————————— [end] counsel

;;; ———————————————————————————————————————————————————— avy
;; Fast go to char/word/line/... in the current view
(use-package avy
  :demand
  :bind
  (("M-s" . avy-goto-char))

  :config
  ;; Keys for bépo keyboard
  (setq avy-keys '(?v ?d ?l ?j ?t ?s ?r ?n ?q ?g ?h ?f)))
;; —————————————————————————————————————————————— [end] avy

;;; ———————————————————————————————————————————— zzz-to-char
(use-package zzz-to-char
  :after avy

  :bind
  (("M-z" . zzz-up-to-char))

  :config
  (setq zzz-to-char-reach 320))
;;; —————————————————————————————————————— [end] zzz-to-char

;;; ———————————————————————————————————————————————— company
(use-package company
  :demand

  :diminish
  company-mode

  :bind
  (:map company-active-map
        ("<tab>"    . company-complete-selection)
        ("<escape>" . company-abort))

  :config

  (setq company-idle-delay 0.           ; delay before displaying auto completion choices
        company-minimum-prefix-length 2 ; number of characters needed before auto completion popup
        company-show-numbers nil)       ; show/hide the quick access numbers

  ;; Activate company globally
  (add-hook 'after-init-hook 'global-company-mode)
  ;; Remove dabbrev from company's backends
  (delete "company-dabbrev" company-backends))

;; --

(use-package company-quickhelp
  :after
  company

  :bind
  (:map company-active-map
        ("M-h" . company-quickhelp-manual-begin))

  :config
  ;; Activate quickhelp
  (company-quickhelp-mode 1)

    ;; delay before displaying the help
  (setq company-quickhelp-delay 0.))

;; --

(use-package company-jedi
  :after
  company

  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook))

;; --

(use-package company-auctex
  :after
  company

  :config
  (company-auctex-init))
;;; —————————————————————————————————————————— [end] company

;;; ————————————————————————————————————————————————— popwin
;; Manage some buffer as popups (less invasive frames)
(use-package popwin
  :init
  (setq popwin:popup-window-height 20)

  :config
  (popwin-mode 1))
;;; ——————————————————————————————————————————— [end] popwin

;;; —————————————————————————————————————————————— yasnippet
(use-package yasnippet
  :diminish
  yas-minor-mode

  :bind
  (("C-c C-y"     . nil)
   ("M-n"         . yas-insert-snippet)
   ("C-c C-y C-y" . yas-insert-snippet)
   ("C-c C-y C-n" . yas-new-snippet)

   :map yas-keymap
   ("M-«" . yas-prev-field)
   ("M-»" . yas-next-field))

  :config
  (yas-global-mode 1))

(use-package company
  :after yasnippet

  :bind
  ("C-c C-y C-u" . company-yasnippet)

  :config
  (defun my/company-yasnippet-or-completion ()
    (interactive)
    (let ((yas-maybe-expand nil))
      (unless (yas-expand)
        (call-interactively #'company-complete-common))))

  (add-hook 'company-mode-hook
            (lambda () (substitute-key-definition 'company-complete-common
                                                  'company-yasnippet-or-completion company-active-map))))
;;; ———————————————————————————————————————— [end] yasnippet

;;; ————————————————————————————————————————————————— ispell
;; ispell
(use-package ispell
  :config
  (setq ispell-program-name "hunspell" ; use hunspell to correct mistakes
        ispell-dictionary   "en_US")   ; default dictionary to use

  ;; Set $DICPATH for Hunspell
  (setenv "DICPATH" "/usr/share/hunspell"()))
;;; ——————————————————————————————————————————— [end] ispell

;;; ——————————————————————————————————————————————— flyspell
(use-package flyspell
  :diminish
  flyspell-mode

  :bind
  (:map flyspell-mode-map
        ("C-c C-v C-b" . flyspell-buffer))
  :config
  ;; Avoid printing messages for every word (it can be very slow)
  (setq flyspell-issue-message-flag nil)

  ;; Flyspell activated for text mode
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))

  ;; Flyspell deactivated for log edit
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))

  (add-hook 'org-mode-hook (lambda () (flyspell-mode)))
  (add-hook 'c++-mode-hook (lambda () (flyspell-prog-mode)))
  (add-hook 'c-mode-hook (lambda () (flyspell-prog-mode)))
  (add-hook 'java-mode-hook (lambda () (flyspell-prog-mode)))
  (add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (flyspell-prog-mode)))
  (add-hook 'latex-mode-hook (lambda () (flyspell-mode))))
;;; ————————————————————————————————————————— [end] flyspell

;;; ——————————————————————————————————————————————— flycheck
(use-package flycheck
  :init
  (setq flycheck-keymap-prefix (kbd "C-c s"))

  :config
  (global-flycheck-mode t))
;;; ————————————————————————————————————————— [end] flycheck

;;; —————————————————————————————————————————————————— latex
;; Changes the default fontification for the given keywords
(eval-after-load "font-latex"
  '(font-latex-add-keywords
    '(("newenvironment" "*{[[")
      ("renewenvironment" "*{[[")
      ("newcommand" "*|{\\[[")
      ("renewcommand" "*|{\\[[")
      ("providecommand" "*|{\\[[")
      ("fbox" "")
      ("mbox" "")
      ("sbox" ""))
    'function))
;;; ———————————————————————————————————————————— [end] latex

;;; —————————————————————————————————————————————— undo tree
(use-package undo-tree
  :bind
  (:map undo-tree-map
        ("C-z" . undo)
        ("M-z" . undo-tree-redo))
  :config
  ;; (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t))
;;; ———————————————————————————————————————— [end] undo tree

;;; ——————————————————————————————————————————————— lua-mode
(use-package lua-mode
  :mode
  ("\\.lua$" . lua-mode)
  :interpreter
  ("lua" . lua-mode)

  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t))
;;; ————————————————————————————————————————— [end] lua-mode

;;; ——————————————————————————————————————————————— markdown
(use-package markdown-mode
  :mode
  ("\\.md$" . markdown-mode)
  ("\\.markdown$" . markdown-mode)
  ("README\\.md$" . gfm-mode)

  :config
  (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files." t)
  (autoload 'gfm-mode      "markdown-mode" "Major mode for editing GitHub Flavored Markdown files" t))
;;; ————————————————————————————————————————— [end] markdown

;;; —————————————————————————————————————————————————— magit
(use-package magit
  :bind
  (("C-x g" . magit-status)))
;;; ———————————————————————————————————————————— [end] magit

;;; ———————————————————————————————————————————————————— sql
(add-to-list 'same-window-buffer-names "*SQL*")

(defun my/sql-save-history-hook ()
  (let ((lval 'sql-input-ring-file-name)
        (rval 'sql-product))
    (if (symbol-value rval)
        (let ((filename
               (concat "~/.emacs.d/sql/"
                       (symbol-name (symbol-value rval))
                       "-history.sql")))
          (set (make-local-variable lval) filename))
      (error
       (format "SQL history will not be saved because %s is nil"
               (symbol-name rval))))))

(add-hook 'sql-interactive-mode-hook 'my/sql-save-history-hook)

(use-package sqlup-mode
  :config
  ;; Capitalize keywords in SQL mode
  (add-hook 'sql-mode-hook 'sqlup-mode)
  ;; Capitalize keywords in an interactive session (e.g. psql)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode))
;;; —————————————————————————————————————————————— [end] sql

;;; ——————————————————————————————————————————————— org-mode
(use-package org
  :config
  (setq org-directory "~/.org-mode.d"
        org-agenda-files (list "calendar-work.org"
                               "calendar-home.org")
        org-default-notes-file "notes.org"
        org-support-shift-select t
        org-log-done t)

  (setq org-emphasis-alist (quote (("*" bold "<b>" "</b>")
                                   ("/" italic "<i>" "</i>")
                                   ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                                   ("=" org-code "<code>" "</code>" verbatim)
                                   ("~" (or )rg-verbatim "<code>" "</code>" verbatim))))

  ;; Activate support for languages for org-babel
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                           (sqlite . t))))
;;; ————————————————————————————————————————— [end] org-mode

;;; —————————————————————————————————————————————————— dired
(use-package dired
  :config
    ;; Human readable size
  (setq dired-listing-switches "-alXGh --group-directories-first")
  ;; allows to edit permissions in dired (when using C-x C-q)
  (setq wdired-allow-to-change-permissions t)
  ;; auto refresh dired when file changes
  (add-hook 'dired-mode-hook 'auto-revert-mode))

(use-package dired-x
  :config
  (setq-default dired-omit-files-p t))
;;; ———————————————————————————————————————————— [end] dired

;;; —————————————————————————————————————————— dired subtree
;; Credits to Mads Hartmann
;; http://mads-hartmann.com/2016/05/12/emacs-tree-view.html
(use-package dired-subtree
  :bind
  (:map dired-mode-map
        ("<enter>"        . my/dwim-toggle-or-open)
        ("<return>"       . my/dwim-toggle-or-open)
        ("<tab>"          . my/dwim-toggle-or-open)
        ("<down-mouse-1>" . my/mouse-dwim-to-toggle-or-open))

  :config
    ;; Function to customize the line prefixes (I simply indent the lines a bit)
  (setq dired-subtree-line-prefix (lambda (depth) (make-string (* 2 depth) ?\s)))
  (setq dired-subtree-use-backgrounds nil)

  (defun my/dwim-toggle-or-open ()
    "Toggle subtree or open the file."
    (interactive)
    (if (file-directory-p (dired-get-file-for-visit))
        (progn
          (dired-subtree-toggle)
          (revert-buffer))
      (dired-find-file)))

  (defun my/mouse-dwim-to-toggle-or-open (event)
    "Toggle subtree or the open file on mouse-click in dired."
    (interactive "e")
    (let* ((window (posn-window (event-end event)))
           (buffer (window-buffer window))
           (pos (posn-point (event-end event))))
      (progn
        (with-current-buffer buffer
          (goto-char pos)
          (my/dwim-toggle-or-open))))))
;;; ———————————————————————————————————— [end] dired subtree

;;; ——————————————————————————————————————————————————— crux
(use-package crux
  :bind
  (([remap kill-whole-line] . crux-kill-whole-line)
   ([remap move-beginning-of-line] . crux-move-beginning-of-line)))
;;; ————————————————————————————————————————————— [end] crux

;;; ———————————————————————————————————— volatile-highlights
(use-package volatile-highlights
  :diminish volatile-highlights-mode

  :config
  (volatile-highlights-mode t))
;;; —————————————————————————————— [end] volatile-highlights

;;; ——————————————————————————————————————— multiple-cursors
(use-package multiple-cursors
  :bind
  (("C-c C-e C-e" . mc/edit-lines)
   ("M-«"         . mc/mark-previous-like-this)
   ("M-»"         . mc/mark-next-like-this)
   ("C-»"         . mc/unmark-previous-like-this)
   ("C-«"         . mc/unmark-next-like-this)
   ("C-c C-e C-p" . mc/mark-all-like-this-dwim)
   ("C-c C-e C-a" . mc/mark-all-like-this)
   ("S-<mouse-1>" . mc/add-cursor-on-click)))
;;; ————————————————————————————————— [end] multiple-cursors

;;; —————————————————————————————————————————— all-the-icons
(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icon
  :after dired

  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
;;; ———————————————————————————————————— [end] all-the-icons

;;; —————————————————————————————————————————————— powerline
;; (use-package powerline
;;   :config
;;   (powerline-default-theme))
;;; ———————————————————————————————————————— [end] powerline

;;; —————————————————————————————————————————————— spaceline
;; (use-package spaceline-config
;;   :config
;;   (spaceline-spacemacs-theme))

(use-package spaceline-all-the-icons
  :after spaceline
  :demand

  :config
  (spaceline-all-the-icons-theme)

  (spaceline-all-the-icons--setup-package-updates)
  (spaceline-all-the-icons--setup-git-ahead)
  (spaceline-all-the-icons--setup-neotree)

  (spaceline-toggle-all-the-icons-git-status-on)
  (spaceline-toggle-all-the-icons-git-ahead-on)
  (spaceline-toggle-all-the-icons-hud-on)
  (spaceline-toggle-all-the-icons-buffer-position-off)
  (spaceline-toggle-all-the-icons-buffer-id-on)
  (spaceline-toggle-all-the-icons-buffer-path-off)
  (spaceline-toggle-all-the-icons-region-info-off)
  (spaceline-toggle-all-the-icons-bookmark-on)
  (spaceline-toggle-all-the-icons-dedicated-on)
  (spaceline-toggle-all-the-icons-process-off)
  (spaceline-toggle-all-the-icons-projectile-on)
  (spaceline-toggle-all-the-icons-time-off)
  (spaceline-toggle-all-the-icons-vc-icon-on)
  (spaceline-toggle-all-the-icons-window-number-off)
  (spaceline-toggle-all-the-icons-separator-right-active-1-off)
  (spaceline-toggle-all-the-icons-separator-right-active-2-off)
  (spaceline-toggle-all-the-icons-separator-right-inactive-off)
  (spaceline-toggle-all-the-icons-separator-left-active-3-off)

  (setq spaceline-all-the-icons-secondary-separator "."
        spaceline-all-the-icons-file-name-highlight t
        spaceline-all-the-icons-hide-long-buffer-path nil
        spaceline-all-the-icons-separator-type 'arrow
        spaceline-all-the-icons-slim-render nil
        spaceline-all-the-icons-flycheck-alternate nil
        spaceline-all-the-icons-icon-set-flycheck-slim 'dots
        spaceline-all-the-icons-window-number-always-visible nil
        ;; spaceline-all-the-icons-icon-set-vc-icon-git 'github
        spaceline-all-the-icons-icon-set-window-numbering 'circle))
;;; ———————————————————————————————————————— [end] spaceline

;;; ———————————————————————————————————————————————— neotree
(use-package neotree
  :bind
  (("<f10>" . neotree-toggle))

  :config
  (setq neo-smart-open t
        neo-autorefresh nil
        neo-window-fixed-size nil
        neo-window-position 'left
        neo-confirm-delete-directory-recursively 'off-p
        neo-confirm-change-root 'off-p
        neo-window-width 35
        neo-confirm-kill-buffers-for-files-in-directory 'off-p
        neo-theme 'icons)

  (use-package hl-anything
    :config
    (add-hook 'neotree-mode-hook 'hl-line-mode))

  (use-package projectile
    :bind
    (("<S-f10>" . neotree-toggle)
     :map projectile-mode-map
     ("<f10>" . my/neotree-project-dir))

    :config
    (defun my/neotree-project-dir ()
      "Open NeoTree using the git root."
      (interactive)
      (let ((project-dir (projectile-project-root))
            (file-name (buffer-file-name)))
        (neotree-toggle)
        (if project-dir
            (if (neo-global--window-exists-p)
                (progn
                  (neotree-dir project-dir)
                  (neotree-find file-name)))
          (message "Could not find git project root."))))))
;;; —————————————————————————————————————————— [end] neotree

;;; ——————————————————————————————————————————————— diminish
(use-package diminish
  :config
  (diminish 'abbrev-mode)
  (diminish 'eldoc-mode))
;;; ————————————————————————————————————————— [end] diminish

;;; —————————————————————————————————————————————— hydra ivy
(use-package hydra
  :after ivy

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
             "\naction %s(ivy-action-name)\n"

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
;;; ——————————————————————————————————————— [end] hydra ivy

;;; ————————————————————————————————————————— hydra perspeen
(use-package hydra
  :after perspeen

  :bind
  (:map perspeen-mode-map
        ("C-c t" . my/hydra-perspeen/body))

  :config
  (defhydra my/hydra-perspeen (:color teal :hint nil :idle 0.25)
    "

Perspeen: [_c_] create-ws   [_p_] previous-ws    [_d_] change-root-dir   [_t_] create-tab
          [_k_] delete-ws   [_n_] next-ws        [_e_] ws-eshell         [_q_] delete-tab
          [_r_] rename-ws   [_'_] goto-last-ws   [_s_] goto-ws

    "
    ("<escape>" nil :exit t)

    ;; Workspaces
    ("c" perspeen-create-ws)
    ("k" perspeen-delete-ws)
    ("r" perspeen-rename-ws)
    ("p" perspeen-previous-ws)
    ("n" perspeen-next-ws)
    ("'" perspeen-goto-last-ws)
    ("d" perspeen-change-root-dir)
    ("e" perspeen-ws-eshell)
    ("s" perspeen-goto-ws)

    ;; Tabs
    ("t" perspeen-tab-create-tab)
    ("q" perspeen-tab-del)
    ("<left>"     perspeen-tab-prev)
    ("<right>"    perspeen-tab-next)
    ("<C-tab>"    perspeen-tab-next)))
;;; ——————————————————————————————————— [end] hydra perspeen

;;; ——————————————————————————————————————— hydra projectile
(use-package hydra
  :after projectile

  :bind
  (:map projectile-mode-map
   ("C-c p" . my/hydra-projectile/body))

  :config
  (defhydra my/hydra-projectile (:color teal :hint nil :idle 0.25)
    "
Projectile

Projects: [_p_]  switch-project            Files: [_d_]  find-dir                  Compilation: [_c_]  compile-project
          [_q_]  switch-open-project              [_f_]  find-file                              [_P_]  test-project
          [_V_]  browse-dirty-project             [_g_]  find-file-dwim                         [_u_]  run-project
          [_y_]  remove from known project        [_T_]  find-test-file                         [_t_]  toggle-between-implementation-and-test
           ^ ^                                    [_e_]  recentf
 Buffers: [_b_]  switch-to-buffer                 [_l_]  file-in-directory                Misc: [_E_]  edit-dir-locals
          [_k_]  kill-buffers                     [_F_]  file-in-known-projects                 [_v_]  version-control
          [_I_]  ibuffer                          [_D_]  dired                                  [_m_]  commander
          [_S_]  save-project-buffers
           ^ ^                             Cache: [_z_]  cache-current-file           Terminal: [_&_]  run-async-shell-command-in-root
  Search: [_o_]  multi-occur                      [_i_]  invalidate-cache                       [_!_]  run-shell-command-in-root
          [_r_]  replace                           ^ ^                                          [_xe_] run-eshell
          [_sg_] grep                       Tags: [_j_]  find-tag                               [_xs_] run-shell
          [_ss_] ag                               [_R_]  regenerate-tags                        [_xt_] run-term

    "
    ("SPC" counsel-projectile)
    ("<escape>" nil :exit t)

    ;; Projects
    ("p"  counsel-projectile-switch-project)
    ("q"  projectile-switch-open-project)
    ("V"  projectile-browse-dirty-projects)
    ("y"  projectile-remove-known-project)


    ;; Files in project(s)
    ("f"  counsel-projectile-find-file)
    ("g"  projectile-find-file-dwim)
    ("l"  projectile-find-file-in-directory)
    ("e"  projectile-recentf)
    ("d"  counsel-projectile-find-dir)
    ("D"  projectile-dired)
    ("F"  projectile-find-file-in-known-projects)
    ("T"  projectile-find-test-file)
    ("a"  projectile-find-other-file)

    ;; Cache
    ("z"  projectile-cache-current-file)
    ("i"  projectile-invalidate-cache)

    ;; Buffers
    ("b"  counsel-projectile-switch-to-buffer)
    ("k"  projectile-kill-buffers)
    ("I"  projectile-ibuffer)
    ("S"  projectile-save-project-buffers)

    ;; Search/Replace
    ("o"  projectile-multi-occur)
    ("r"  projectile-replace)
    ("sg" projectile-grep)
    ("ss" counsel-projectile-ag)

    ;; Tags
    ("j"  projectile-find-tag)
    ("R"  projectile-regenerate-tags)

    ;; Complitation
    ("c"  projectile-compile-project)
    ("P"  projectile-test-project)
    ("u"  projectile-run-project)
    ("t"  projectile-toggle-between-implementation-and-test)

    ;; Terminal
    ("&"  projectile-run-async-shell-command-in-root)
    ("!"  projectile-run-shell-command-in-root)
    ("xe" projectile-run-eshell)
    ("xs" projectile-run-shell)
    ("xt" projectile-run-term)

    ;; Other window
    ("wv"  projectile-display-buffer)
    ("wa"  projectile-find-other-file-other-window)
    ("wb"  projectile-switch-to-buffer-other-window)
    ("wd"  projectile-find-dir-other-window)
    ("wf"  projectile-find-file-other-window)
    ("wg"  projectile-find-file-dwim-other-window)
    ("wt"  projectile-find-implementation-or-test-other-window)

    ;; Misc
    ("E"  projectile-edit-dir-locals)
    ("v"  projectile-vc)
    ("m"  projectile-commander)))
;;; —————————————————————————————————— [end] hydra projectile

;;; —————————————————————————————————————— hydra vimish-fold
(use-package hydra
  :after vimish-fold

  :bind
  (:map global-map
        ("C-c f" . my/hydra-vimish-fold/body))

  :config
  (defhydra my/hydra-vimish-fold (:color teal :hint nil :idle 0.25)
    "

Vimish fold: [_f_] create fold        [_t_] toggle fold   [_b_] fold next block
             [_d_] delete fold        [_o_] unfold-all
             [_D_] delete all folds   [_c_] refold-all

    "
    ("<escape>" nil :exit t)
    ("RET" vimish-fold-toggle)

    ("t" vimish-fold-toggle)
    ("f" vimish-fold)
    ("d" vimish-fold-delete)
    ("o" vimish-fold-unfold-all)
    ("c" vimish-fold-refold-all)
    ("b" my/vimish-fold-next-block)
    ("D" vimish-fold-delete-all)))
;;; ———————————————————————————————— [end] hydra vimish-fold

;;; ———————————————————————————————————————— hydra meghanada
(use-package hydra
  :after meghanada

  :bind
  (:map meghanada-mode-map
        ("C-c C-c" . my/hydra-meghanada/body))

  :config
  (defhydra my/hydra-meghanada (:color teal :hint nil :idle 0.25)
    "

Meghanada: [_c_] compile-file          [_i_] import-all        [_S_] server-start        [_R_] restart
           [_C_] compile-project       [_o_] optimize-import   [_K_] server-kill         [_U_] update-server
           [_m_] run-task              [_b_] code-beautify     [_D_] client-disconnect   [_I_] install-server
           [_t_] run-junit-test-case   [_l_] locate-variable   [_P_] ping                [_?_] version
           [_T_] run-junit-class       [_s_] switch-testcase

    "
    ("<escape>" nil :exit t)

    ("c" meghanada-compile-file)
    ("C" meghanada-compile-project)
    ("m" meghanada-run-task)
    ("t" meghanada-run-junit-test-case)
    ("T" meghanada-run-junit-class)
    ("s" meghanada-switch-testcase)
    ("l" meghanada-local-variable)
    ("b" meghanada-code-beautify)
    ("i" meghanada-import-all)
    ("o" meghanada-optimize-import)
    ("S" meghanada-server-start)
    ("K" meghanada-server-kill)
    ("D" meghanada-client-disconnect)
    ("P" meghanada-ping)
    ("R" meghanada-restart)
    ("U" meghanada-update-server)
    ("I" meghanada-install-server)
    ("?" meghanada-version)))
;;; —————————————————————————————————— [end] hydra meghanada

;;; ————————————————————————————————————————— hydra flycheck
(use-package hydra
  :after flycheck

  :bind
  (:map flycheck-mode-map
        ("C-c s" . my/hydra-flycheck/body))

  :config
  (defhydra my/hydra-flycheck (:color teal :hint nil :idle 0.25)
    "

Flycheck: [_b_] check buffer     [_l_] list-errors              [_s_] select-checker     [_v_] verify-setup
          [_c_] clear buffer     [_h_] display error at point   [_x_] disable checker    [_V_] version
          [_p_] previous error   [_e_] explain error at point   [_?_] describe checker   [_i_] manual
          [_n_] next error

    "
    ("<escape>" nil :exit t)

    ("b" flycheck-buffer)
    ("c" flycheck-clear)
    ("p" flycheck-previous-error)
    ("n" flycheck-next-error)

    ("l" flycheck-list-errors)
    ("e" flycheck-explain-error-at-point)
    ("h" flycheck-display-error-at-point)

    ("s" flycheck-select-checker)
    ("x" flycheck-disable-checker)
    ("?" flycheck-describe-checker)

    ("v" flycheck-verify-setup)
    ("V" flycheck-version)
    ("i" flycheck-manual)))
;;; ——————————————————————————————————— [end] hydra flycheck

;;; ———————————————————————————————————————————— matlab-mode
(use-package matlab-mode
  :mode "\\.m$"

  :config
  (autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
  (setq matlab-indent-function t)
  (setq matlab-shell-command "matlab"))
;;; —————————————————————————————————————— [end] matlab-mode

;;; ————————————————————————————————————————— google-c-style
(use-package google-c-style
  :defer t
  :ensure t

  :commands
  (google-set-c-style)

  :config
  (add-hook 'java-mode-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-set-c-style))
;;; ——————————————————————————————————— [end] google-c-style

;;; ———————————————————————————————————————————————— origami
(use-package origami
  :bind
  (("M-p"     . origami-recursively-toggle-node)
   ("C-c o t" . origami-toggle-node)
   ("C-c o o" . origami-open-node)
   ("C-c o O" . origami-open-node-recursively)
   ("C-c o c" . origami-close-node)
   ("C-c o C" . origami-close-node-recursively)
   ("C-c o t" . origami-toggle-node)
   ("C-c o T" . origami-forward-toggle-node)
   ("C-c o n" . origami-forward-fold)
   ("C-c o p" . origami-previous-fold)
   ("C-c o R" . origami-reset))

  :config
  (add-hook 'java-mode-hook 'origami-mode)
  (add-hook 'python-mode-hook 'origami-mode)
  (add-hook 'c-mode-common-hook 'origami-mode)
  (add-hook 'emacs-lisp-mode-hook 'origami-mode))
;;; —————————————————————————————————————————— [end] origami


;;; ********************************************************

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (paradox spaceline-all-the-icons spaceline
     all-the-icons-dired all-the-icons origami google-c-style
     zzz-to-char matlab-mode ivy-hydra counsel-gtags hydra
     use-package ivy-rich smex flx counsel-projectile counsel ivy
     neotree dired-subtree diminish perspeen multiple-cursors
     hl-anything volatile-highlights crux whitespace-cleanup-mode
     vimish-fold undo-tree systemd sqlup-mode smartparens
     rainbow-mode popwin meghanada markdown-mode magithub
     lua-mode java-snippets expand-region drag-stuff
     company-quickhelp company-jedi company-bibtex company-auctex
     avy))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
