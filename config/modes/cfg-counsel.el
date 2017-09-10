;;; Package --- Summary
;;; counsel configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2017 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(req-package counsel
  :defer t
  :after ivy
  :require ivy
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
  (setq counsel-find-file-ignore-regexp "^\\.\\|~$\\|^#\\|\\.elc\\|\\.pyc")
  (setq magit-completing-read-function 'ivy-completing-read))

;; ——

(use-package counsel-gtags
  :after ivy
  :defer t
  :diminish counsel-gtags-mode

  :init
  (add-hook 'c-mode-hook    #'counsel-gtags-mode)
  (add-hook 'c++-mode-hook  #'counsel-gtags-mode)
  (add-hook 'java-mode-hook #'counsel-gtags-mode)

  :bind
  (:map counsel-gtags-mode-map
        ("<f2>" . counsel-gtags-dwim))

  :config
  (setq counsel-gtags-path-style 'relative
        counsel-gtags-ignore-case t
        counsel-gtags-auto-update t))

;; ——

(use-package counsel-projectile
  :after (ivy projectile)
  :defer t

  :bind
  (:map projectile-mode-map
        ([remap projectile-switch-project]   . counsel-projectile-switch-project)
        ([remap projectile-switch-to-buffer] . counsel-projectile-switch-to-buffer)
        ([remap projectile-find-file]        . counsel-projectile-find-file)
        ([remap projectile-find-dir]         . counsel-projectile-find-dir))

  :config
  (setq projectile-completion-system 'ivy))

;; ——

(provide 'cfg-counsel)

;;; ————————————————————————————————————————————————————————
;;; cfg-counsel.el ends here
