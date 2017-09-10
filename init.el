;;; Package --- Summary
;;; Emacs main configuration file.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

;;; -*- no-byte-compile: t -*-

(defvar emacs-d user-emacs-directory)

;; (defvar emacs-d
;;   (file-name-directory
;;    (file-chase-links load-file-name))
;;   "The giant turtle on which the world rests.")

(setq package-user-dir
      (expand-file-name "elpa" emacs-d))

(add-to-list 'load-path (expand-file-name "lisp/" emacs-d))
(add-to-list 'load-path (expand-file-name "config/" emacs-d))
(add-to-list 'load-path (expand-file-name "config/modes/" emacs-d))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu"   . "http://elpa.gnu.org/packages/"))
(package-initialize)

(require 'packages)
(require 'cfg-use-package)
(setq load-prefer-newer t)
(require 'cfg-auto-compile)

(require 'functions)
(require 'keys)
(require 'startup)
(require 'backups)
(require 'cursor)
(require 'scrolling)
(require 'bell)

(require 'cfg-which-key)
(require 'cfg-paradox)
(require 'cfg-diminish)
(require 'cfg-crux)
(require 'cfg-eshell)
(require 'cfg-ediff)
(require 'cfg-whitespace-cleanup)
(require 'cfg-vimish-fold)
(require 'cfg-drag-stuff)
(require 'cfg-expand-region)
(require 'cfg-smartparens)
(require 'cfg-projectile)
(require 'cfg-meghanada)
(require 'cfg-ivy)
(require 'cfg-counsel)
(require 'cfg-swiper)
(require 'cfg-avy)
(require 'cfg-helm)
(require 'cfg-company)
(require 'cfg-popwin)
(require 'cfg-flyspell)
(require 'cfg-flycheck)
(require 'cfg-modalka)
(require 'cfg-beacon)
(require 'cfg-anzu)
(require 'cfg-dired)
(require 'cfg-origami)
(require 'cfg-image+)
(require 'cfg-eyebrowse)
(require 'cfg-neotree)
(require 'cfg-org)
(require 'cfg-markdown)
(require 'cfg-lua-mode)
(require 'cfg-matlab-mode)
(require 'cfg-magit)
(require 'cfg-multiple-cursors)
(require 'cfg-undo-tree)
(require 'cfg-doc-view)
(require 'cfg-volatile-highlights)
(require 'cfg-sql)
(require 'cfg-latex)
(require 'cfg-all-the-icons)
(require 'cfg-spaceline-all-the-icons)

(use-package help-fns+)

;; (require 'cfg-hideshowvis)
;; (use-package dokuwiki)
;; (require 'cfg-google-c-style)
;; (require 'cfg-perspeen)

(req-package-finish)

(setq custom-file  (expand-file-name "config/customs.el" emacs-d))
(load custom-file)

(provide 'init)

;;; ————————————————————————————————————————————————————————
;;; init.el ends here
