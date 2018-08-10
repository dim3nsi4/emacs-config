(deftheme clearview-light
  "Created 2017-10-12.")

(custom-theme-set-faces
 'clearview-light
 '(Info-quoted ((t (:inherit fixed-pitch-serif))))
 '(all-the-icons-dired-dir-face ((((background dark)) (:foreground "white")) (((background light)) (:foreground "black"))))
 '(anzu-match-1 ((t (:background "aquamarine" :foreground "black"))))
 '(anzu-match-2 ((t (:background "springgreen" :foreground "black"))))
 '(anzu-match-3 ((t (:background "yellow" :foreground "black"))))
 '(anzu-mode-line ((t (:inherit mode-line-emphasis))))
 '(anzu-mode-line-no-match ((t (:inherit anzu-mode-line :foreground "OrangeRed2"))))
 '(anzu-replace-highlight ((t (:inherit query-replace))))
 '(anzu-replace-to ((((class color) (background light)) (:foreground "red")) (((class color) (background dark)) (:foreground "yellow"))))
 '(avy-background-face ((t (:foreground "gray40"))))
 '(avy-goto-char-timer-face ((t (:inherit (highlight)))))
 '(avy-lead-face ((t (:background "gray25" :foreground "white" :weight bold))))
 '(avy-lead-face-0 ((t (:background "DodgerBlue3" :foreground "white" :weight bold))))
 '(avy-lead-face-1 ((t (:background "PaleGreen4" :foreground "white" :weight bold))))
 '(avy-lead-face-2 ((t (:background "DeepPink3" :foreground "white" :weight bold))))
 '(company-echo ((t nil)))
 '(company-echo-common ((t (:background "firebrick3"))))
 '(company-preview ((t (:inherit (company-tooltip-selection company-tooltip)))))
 '(company-preview-common ((t (:inherit company-tooltip-common-selection))))
 '(company-preview-search ((t (:inherit company-tooltip-common-selection))))
 '(company-scrollbar-bg ((t (:background "gray90"))))
 '(company-scrollbar-fg ((t (:background "black"))))
 '(company-template-field ((t (:background "orange" :foreground "black"))))
 '(company-tooltip ((t (:background "floral white" :foreground "black"))))
 '(company-tooltip-annotation ((t (:foreground "saddle brown" :slant oblique))))
 '(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation))))
 '(company-tooltip-common ((t (:foreground "black" :weight bold))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-common))))
 '(company-tooltip-mouse ((t (:inherit highlight))))
 '(company-tooltip-search ((t (:inherit highlight))))
 '(company-tooltip-search-selection ((t (:inherit highlight))))
 '(company-tooltip-selection ((t (:background "moccasin"))))
 '(cua-rectangle ((t (:background "gray20" :foreground "white"))))
 '(custom-button ((t (:box (:line-width 2 :style released-button) :foreground "black" :background "gray95"))))
 '(custom-button-mouse ((t (:background "DodgerBlue3" :foreground "white" :box (:line-width 2 :style released-button)))))
 '(custom-button-pressed ((t (:box (:line-width 2 :style pressed-button) :inherit custom-button-mouse))))
 '(custom-group-tag ((t (:weight bold :foreground "white smoke" :background "gray25" :inherit variable-pitch))))
 '(custom-modified ((t (:background "DodgerBlue3" :foreground "white"))))
 '(custom-variable-tag ((t (:weight bold))))
 '(dired-directory ((t (:inherit (font-lock-function-name-face)))))
 '(dired-header ((t (:inherit (font-lock-type-face)))))
 '(dired-symlink ((t (:inherit (font-lock-keyword-face)))))
 '(eyebrowse-mode-line-active ((t (:inherit mode-line-emphasis))))
 '(eyebrowse-mode-line-inactive ((t (:inherit mode-line-inactive))))
 '(fixed-pitch-serif ((t (:family "DejaVu Serif Mono"))))
 '(flycheck-error ((t (:foreground "red" :underline (:color "firebrick" :style wave)))))
 '(flycheck-error-list-column-number ((t (:inherit default))))
 '(flycheck-error-list-error ((t (:inherit flycheck-fringe-error))))
 '(flycheck-error-list-info ((t (:inherit flycheck-fringe-info))))
 '(flycheck-error-list-line-number ((t (:weight bold))))
 '(flycheck-error-list-warning ((t (:inherit flycheck-fringe-warning))))
 '(flycheck-fringe-error ((t (:foreground "red" :weight bold))))
 '(flycheck-fringe-info ((t (:foreground "forest green" :weight bold))))
 '(flycheck-fringe-warning ((t (:foreground "DodgerBlue2" :weight bold))))
 '(flycheck-info ((t (:underline (:color "forest green" :style wave)))))
 '(flycheck-warning ((t (:underline (:color "gray30" :style wave)))))
 '(font-lock-builtin-face ((t (:foreground "black" :underline t))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "gray60" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "DeepPink3"))))
 '(font-lock-doc-face ((t (:foreground "gray60"))))
 '(font-lock-function-name-face ((t (:foreground "DodgerBlue3" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "black" :weight bold))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit font-lock-regexp-grouping-construct))))
 '(font-lock-regexp-grouping-construct ((t nil)))
 '(font-lock-string-face ((t (:foreground "DarkOrange4"))))
 '(font-lock-type-face ((t (:foreground "grey43" :weight semi-bold))))
 '(font-lock-variable-name-face ((t (:foreground "black"))))
 '(fringe ((t (:background "grey95"))))
 '(helm-M-x-key ((t (:foreground "purple3" :underline t))))
 '(helm-action ((t nil)))
 '(helm-buffer-directory ((t (:foreground "black" :weight bold))))
 '(helm-buffer-file ((t (:inherit default))))
 '(helm-buffer-not-saved ((t (:slant italic))))
 '(helm-buffer-process ((t (:foreground "gray50"))))
 '(helm-buffer-saved-out ((t (:background "black" :foreground "white smoke"))))
 '(helm-ff-directory ((t (:foreground "black" :weight bold))))
 '(helm-ff-dotted-directory ((t (:inherit helm-ff-directory :underline t))))
 '(helm-ff-executable ((t (:foreground "forest green"))))
 '(helm-ff-file ((t (:inherit default))))
 '(helm-ff-invalid-symlink ((t (:foreground "red"))))
 '(helm-ff-symlink ((t (:foreground "DodgerBlue3"))))
 '(helm-grep-cmd-line ((t (:inherit (diff-added)))))
 '(helm-grep-file ((t (:foreground "black" :underline t))))
 '(helm-grep-finish ((t (:foreground "forest green"))))
 '(helm-grep-lineno ((t (:foreground "gray50"))))
 '(helm-match ((t (:foreground "VioletRed3"))))
 '(helm-moccur-buffer ((t (:foreground "DodgerBlue3" :underline t))))
 '(helm-selection ((t (:background "DodgerBlue3" :foreground "white smoke" :weight bold))))
 '(helm-separator ((t (:foreground "gray60"))))
 '(helm-source-header ((t (:background "gray20" :foreground "white smoke" :weight bold :height 1.25 :family "Sans Serif"))))
 '(highlight ((t (:background "LightSteelBlue3" :foreground "black"))))
 '(hydra-face-amaranth ((t (:foreground "DarkOrchid3" :weight bold))))
 '(hydra-face-blue ((t (:foreground "DodgerBlue3" :weight bold))))
 '(hydra-face-pink ((t (:foreground "DeepPink3" :weight bold))))
 '(hydra-face-red ((t (:foreground "red2" :weight bold))))
 '(hydra-face-teal ((t (:foreground "turquoise4" :weight bold))))
 '(isearch ((t (:background "VioletRed3" :foreground "white smoke" :weight bold))))
 '(isearch-fail ((t (:background "DodgerBlue3" :foreground "white smoke" :weight bold))))
 '(ivy-action ((t (:inherit (font-lock-builtin-face)))))
 '(ivy-confirm-face ((t (:foreground "ForestGreen" :inherit (minibuffer-prompt)))))
 '(ivy-current-match ((t (:background "DodgerBlue3" :foreground "white" :weight bold))))
 '(ivy-cursor ((t (:background "gray25" :foreground "white"))))
 '(ivy-highlight-face ((t (:inherit (highlight)))))
 '(ivy-match-required-face ((t (:foreground "red" :inherit (minibuffer-prompt)))))
 '(ivy-minibuffer-match-face-1 ((t (:foreground "VioletRed3"))))
 '(ivy-minibuffer-match-face-2 ((t (:foreground "VioletRed3"))))
 '(ivy-minibuffer-match-face-3 ((t (:foreground "VioletRed3"))))
 '(ivy-minibuffer-match-face-4 ((t (:foreground "VioletRed3"))))
 '(ivy-modified-buffer ((t (:inherit (default)))))
 '(ivy-remote ((((class color) (background light)) (:foreground "#110099")) (((class color) (background dark)) (:foreground "#7B6BFF"))))
 '(ivy-subdir ((t (:foreground "black" :weight bold))))
 '(ivy-virtual ((t (:foreground "gray50"))))
 '(lazy-highlight ((t (:background "gray25" :foreground "white" :weight bold))))
 '(link ((t (:foreground "DodgerBlue3" :underline "black"))))
 '(link-visited ((t (:inherit link :foreground "black"))))
 '(minibuffer-prompt ((t (:foreground "black" :underline nil :weight bold))))
 '(mode-line ((t (:weight normal :height 0.95 :background "gray20" :foreground "gray95" :distant-foreground "gray95" :box (:line-width 2 :color "gray20")))))
 '(mode-line-inactive ((t (:inherit mode-line :weight normal :background "gray20" :foreground "gray50" :distant-foreground "gray50"))))
 '(mode-line-buffer-id ((t ( :inherit mode-line :weight bold))))
 '(mode-line-buffer-id-inactive ((t (:inherit mode-line-inactive))))
 '(mode-line-emphasis ((t (:inherit mode-line :foreground "SeaGreen2" :weight bold))))
 '(mode-line-highlight ((t (:box (:line-width 1 :color "grey95")))))
 '(neo-banner-face ((((background dark)) (:weight bold :foreground "lightblue")) (t (:foreground "DarkMagenta"))))
 '(neo-dir-link-face ((t (:foreground "DodgerBlue3" :weight bold))))
 '(neo-expand-btn-face ((t (:foreground "DeepPink3" :weight bold))))
 '(neo-file-link-face ((((background dark)) (:foreground "White")) (t (:foreground "Black"))))
 '(neo-header-face ((t (:foreground "gray25"))))
 '(neo-root-dir-face ((t (:foreground "gray25" :weight bold))))
 '(org-block ((t (:inherit (shadow)))))
 '(org-block-background ((t nil)))
 '(org-block-begin-line ((t (:inherit (org-meta-line)))))
 '(org-block-end-line ((t (:inherit (org-meta-line)))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "OrangeRed3" :weight bold))))
 '(org-table ((t (:foreground "gray20"))))
 '(org-warning ((t (:inherit font-lock-warning-face :underline t :weight normal))))
 '(paradox-mode-line-face ((t (:inherit mode-line-buffer-id))))
 '(paradox-star-face ((t (:inherit (font-lock-string-face)))))
 '(paradox-starred-face ((t (:inherit font-lock-constant-face :weight bold))))
 '(perspeen-selected-face ((t (:inherit mode-line :background "gray25" :foreground "PaleGreen2" :weight bold))))
 '(perspeen-tab--header-line-active ((t (:weight bold :inherit mode-line))))
 '(perspeen-tab--header-line-inactive ((t (:inherit mode-line :weight normal :background "gray25" :foreground "white"))))
 '(perspeen-tab--powerline-inactive1 ((t (:inherit mode-line :background "gray25" :foreground "white" :weight normal))))
 '(powerline-active1 ((t (:inherit mode-line :background "gray25" :foreground "white" :weight normal))))
 '(powerline-active2 ((t (:inherit mode-line :background "gray25" :foreground "white" :weight normal))))
 '(powerline-inactive1 ((t (:inherit mode-line :background "gray25" :foreground "gray60"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "grey25"))))
 '(region ((t (:background "gray93" :distant-foreground "black"))))
 '(secondary-selection ((t (:inherit highlight))))
 '(show-paren-match ((t (:background "dodger blue" :foreground "white smoke" :weight bold))))
 '(show-paren-mismatch ((t (:background "orange red" :foreground "white smoke" :weight bold))))
 '(spaceline-highlight-face ((t (:inherit mode-line :foreground "Black" :background "Gray95" :weight bold))))
 '(tooltip ((t (:inherit variable-pitch :background "gray98" :foreground "black"))))
 '(trailing-whitespace ((t (:underline (:color "tomato" :style wave)))))
 '(vimish-fold-fringe ((t (:inherit fringe :foreground "black" :weight bold))))
 '(vimish-fold-mouse-face ((t (:background "old lace" :inherit vimish-fold-overlay))))
 '(vimish-fold-overlay ((t (:background "floral white" :foreground "black" :box (:line-width 1 :color "moccasin")))))
 '(warning ((t (:foreground "tomato" :weight bold))))
 '(which-func ((t (:inherit mode-line :foreground "DeepSkyBlue1"))))
 '(whitespace-big-indent ((t (:background "orange red" :foreground "white"))))
 '(whitespace-empty ((t (:background "orange red" :foreground "white"))))
 '(whitespace-hspace ((t (:background "orange red" :foreground "white"))))
 '(whitespace-indentation ((t (:background "DodgerBlue3" :foreground "white"))))
 '(whitespace-line ((t (:foreground "orange red"))))
 '(whitespace-newline ((t (:foreground "DodgerBlue4" :weight bold))))
 '(whitespace-space ((t (:foreground "DodgerBlue4" :weight bold))))
 '(whitespace-space-after-tab ((t (:background "DodgerBlue3" :foreground "white" :weight bold))))
 '(whitespace-space-before-tab ((t (:background "DodgerBlue3" :foreground "white" :weight bold))))
 '(whitespace-tab ((t (:background "floral white" :foreground "black"))))
 '(ztreep-arrow-face ((((background dark)) (:foreground "#7f7f7f")) (t (:foreground "#8d8d8d"))))
 '(ztreep-diff-header-face ((t (:inherit font-lock-function-name-face))))
 '(ztreep-diff-header-small-face ((t (:inherit font-lock-keyword-face :foreground "grey25"))))
 '(ztreep-diff-model-add-face ((t (:inherit normal))))
 '(ztreep-diff-model-diff-face ((t (:weight normal :inherit font-lock-warning-face))))
 '(ztreep-diff-model-ignored-face ((t (:slant normal :strike-through t :inherit font-lock-comment-face))))
 '(ztreep-diff-model-normal-face ((t (:slant normal :inherit font-lock-comment-face))))
 '(ztreep-expand-sign-face ((((background dark)) (:foreground "#7f7fff")) (t (:foreground "#8d8d8d"))))
 '(ztreep-header-face ((t (:inherit font-lock-function-name-face))))
 '(ztreep-leaf-face ((t (:inherit default))))
 '(ztreep-node-face ((t (:inherit dired-directory)))))

(provide-theme 'clearview-light)
