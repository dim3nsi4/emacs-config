;;; Package --- Summary
;;; Emacs main configuration file.
;;;
;;; Commentary:
;;; Copyright (c) 2016 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Time-stamp: <2017-09-09 20:22:48 seimandp>
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

;;; ********************************************************

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-bullets which-key define-word dired-narrow dired-collapse dired-sidebar beacon use-package-chords modalka flyspell-correct-ivy org helm dokuwiki-mode dokuwiki mmm-mode all-the-icons anzu avy cdlatex company-auctex company-bibtex company-jedi company-quickhelp counsel counsel-gtags counsel-projectile crux diff-hl diminish dired-subtree drag-stuff expand-region eyebrowse flx gnuplot gnuplot-mode google-c-style help-fns+ hl-anything hydra image+ ivy ivy-hydra ivy-rich java-snippets lua-mode magithub markdown-mode matlab-mode meghanada multiple-cursors neotree org-ref origami paradox pdf-tools perspeen popwin rainbow-mode req-package smartparens smex spaceline all-the-icons-dired sqlup-mode undo-tree use-package vimish-fold volatile-highlights whitespace-cleanup-mode zzz-to-char))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)

;;; ————————————————————————————————————————————————————————
;;; init.el ends here
