;;; Package --- Summary
;;; spaceline-all-the-icons configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package spaceline-all-the-icons
  :demand

  :commands
  (spaceline-all-the-icons--setup-anzu
   spaceline-all-the-icons--setup-git-ahead
   spaceline-all-the-icons--setup-paradox
   spaceline-all-the-icons-theme)

  :config
  (spaceline-all-the-icons-theme)

  (spaceline-all-the-icons--setup-git-ahead)
  (spaceline-all-the-icons--setup-paradox)
  (spaceline-all-the-icons--setup-anzu)

  (spaceline-toggle-all-the-icons-bookmark-on)
  (spaceline-toggle-all-the-icons-buffer-id-on)
  (spaceline-toggle-all-the-icons-buffer-path-off)
  (spaceline-toggle-all-the-icons-buffer-position-off)
  (spaceline-toggle-all-the-icons-dedicated-on)
  (spaceline-toggle-all-the-icons-eyebrowse-workspace-on)
  (spaceline-toggle-all-the-icons-git-ahead-on)
  (spaceline-toggle-all-the-icons-git-status-on)
  (spaceline-toggle-all-the-icons-hud-off)
  (spaceline-toggle-all-the-icons-minor-modes-off)
  (spaceline-toggle-all-the-icons-package-updates-on)
  (spaceline-toggle-all-the-icons-paradox-filter-on)
  (spaceline-toggle-all-the-icons-paradox-line-count-on)
  (spaceline-toggle-all-the-icons-paradox-status-installed-on)
  (spaceline-toggle-all-the-icons-paradox-status-new-on)
  (spaceline-toggle-all-the-icons-paradox-status-upgrade-on)
  (spaceline-toggle-all-the-icons-paradox-total-on)
  (spaceline-toggle-all-the-icons-process-on)
  (spaceline-toggle-all-the-icons-projectile-on)
  (spaceline-toggle-all-the-icons-region-info-on)
  (spaceline-toggle-all-the-icons-separator-left-active-3-off)
  (spaceline-toggle-all-the-icons-separator-left-inactive-on)
  (spaceline-toggle-all-the-icons-separator-right-active-1-off)
  (spaceline-toggle-all-the-icons-separator-right-active-2-off)
  (spaceline-toggle-all-the-icons-separator-right-inactive-on)
  (spaceline-toggle-all-the-icons-text-scale-on)
  (spaceline-toggle-all-the-icons-time-on)
  (spaceline-toggle-all-the-icons-vc-icon-on)
  (spaceline-toggle-all-the-icons-which-function-on)
  (spaceline-toggle-all-the-icons-window-number-on)

  (setq spaceline-all-the-icons-primary-separator "|"
        spaceline-all-the-icons-secondary-separator ""
        spaceline-all-the-icons-file-name-highlight t
        spaceline-all-the-icons-hide-long-buffer-path nil
        spaceline-all-the-icons-separator-type 'slant
        spaceline-all-the-icons-icon-set-eyebrowse-slot 'solid
        spaceline-all-the-icons-separators-invert-direction t
        spaceline-all-the-icons-slim-render nil
        spaceline-all-the-icons-flycheck-alternate t
        spaceline-all-the-icons-icon-set-flycheck-slim 'outline
        spaceline-all-the-icons-window-number-always-visible t
        spaceline-all-the-icons-icon-set-modified 'toggle
        spaceline-all-the-icons-icon-set-vc-icon-git 'gitlab
        spaceline-all-the-icons-icon-set-eyebrowse-slot 'solid
        spaceline-all-the-icons-icon-set-git-ahead 'commit
        spaceline-all-the-icons-icon-set-window-numbering 'circle))

(provide 'cfg-spaceline-all-the-icons)

;;; ————————————————————————————————————————————————————————
;;; cfg-spaceline-all-the-icons.el ends here
