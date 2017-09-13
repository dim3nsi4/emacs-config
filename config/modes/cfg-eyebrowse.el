;;; Package --- Summary
;;; eyebrowse configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package eyebrowse
  :defer t
  :diminish

  :bind
  (("<C-tab>"   . eyebrowse-next-window-config)
   ("<backtab>" . eyebrowse-prev-window-config)
   ("C-c t t"   . eyebrowse-create-window-config)
   ("C-c t c"   . eyebrowse-close-window-config)
   ("C-c t n"   . eyebrowse-next-window-config)
   ("C-c t p"   . eyebrowse-prev-window-config))

  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c t"))

  :config
  (setq eyebrowse-wrap-around t
        eyebrowse-switch-back-and-forth t
        eyebrowse-mode-line-style 'always)
  (eyebrowse-mode t))

;; ——

(req-package eyebrowse
  :defer t
  :diminish
  :require neotree

  :init
  ;; Fix for neotree/eyebrowse compatibility
  (add-hook 'eyebrowse-post-window-switch-hook 'neo-global--attach))

;; ——

(provide 'cfg-eyebrowse)

;;; ————————————————————————————————————————————————————————
;;; cfg-eyebrowse.el ends here