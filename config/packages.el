;;; Package --- Summary
;;; packages configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(defconst my/packages
  '(all-the-icons
    auto-compile
    avy
    beacon
    cdlatex
    company
    company-bibtex
    company-jedi
    company-quickhelp
    counsel
    counsel-gtags
    counsel-projectile
    crux
    define-word
    diminish
    dired-collapse
    dired-narrow
    dired-sidebar
    dired-subtree
    dired-x
    drag-stuff
    expand-region
    eyebrowse
    flx
    flycheck
    flyspell
    help-fns+
    hl-anything
    hydra
    ivy
    ivy-rich
    magit
    markdown-mode
    meghanada
    multiple-cursors
    org
    org-bullets
    org-ref
    origami
    paradox
    pdf-tools
    popwin
    projectile
    req-package
    smartparens
    smex
    spaceline-all-the-icons
    spaceline-all-the-icons-dired
    swiper
    use-package
    use-package-chords
    vimish-fold
    volatile-highlights
    which-key
    zzz-to-char)
  "List of packages that will be automatically installed if missing.")

;; (package-refresh-contents)

;; install required
(dolist (package my/packages)
  (unless (package-installed-p package)
    (ignore-errors
      (package-install package))))

;; ;; upgrade installed
;; (save-window-excursion
;;   (package-list-packages t)
;;   (package-menu-mark-upgrades)
;;   (condition-case nil
;;       (package-menu-execute t)
;;     (error
;;      (package-menu-execute))))

;; ——

(provide 'packages)

;;; ————————————————————————————————————————————————————————
;;; packages.el ends here
