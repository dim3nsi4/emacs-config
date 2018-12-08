;;; Package --- Summary
;;; flyspell configuration.
;;;
;;; Commentary:
;;; Copyright (c) 2016-2018 Pierre Seimandi
;;; Under GPL License v3.0 and after.
;;;
;;; Code:
;;; ————————————————————————————————————————————————————————

(use-package ispell
  :bind
  (("C-c C-s s" . my/cycle-ispell-languages))

  :config
  (cond
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell"))
   ((executable-find "aspell")
    (setq ispell-program-name "aspell")))

  (setq ispell-dictionary "en_US")
  (setq ispell-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
          ("fr_FR" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "fr_FR") nil utf-8)))

  ;; Cycle between a dictionary list session-wide
  (let ((langs '("fr_FR" "en_US")))
    (setq lang-ring (make-ring (length langs)))
    (dolist (elem langs) (ring-insert lang-ring elem)))

  (defun my/cycle-ispell-languages ()
    (interactive)
    (let ((lang (ring-ref lang-ring -1)))
      (ring-insert lang-ring lang)
      (ispell-change-dictionary lang)
      (setq ispell-dictionary lang))))

;; ——

(use-package flyspell
  :diminish flyspell-mode

  :bind
  (:map flyspell-mode-map
        ("C-." . repeat)
        ("C-c C-s" . nil)
        ("C-c C-s b" . flyspell-buffer))

  :hook
  ((org-mode        . flyspell-mode)
   (c++-mode        . flyspell-prog-mode)
   (c-mode          . flyspell-prog-mode)
   (java-mode       . flyspell-prog-mode)
   (python-mode     . flyspell-prog-mode)
   (emacs-lisp-mode . flyspell-prog-mode)
   (latex-mode      . flyspell-mode))

  :config
  ;; Avoid printing messages for every word (it can be very slow)
  (setq flyspell-issue-message-flag nil)

  ;; Flyspell activated for text mode
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))

  ;; Flyspell deactivated for log edit
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1)))))

;; ——

(use-package flyspell-correct-ivy
  :after flyspell

  :bind
  (:map flyspell-mode-map
        ("C-x x" . flyspell-correct-previous-word-generic)))

;; ——

(use-package hydra
  :after flyspell

  :commands
  (flyspell-mode-off
   hydra--call-interactively-remap-maybe
   hydra-default-pre
   hydra-idle-message
   hydra-keyboard-quit
   hydra-set-transient-map
   ispell-change-dictionary
   ispell-check-version)

  :bind
  (:map flyspell-mode-map
        ("C-c C-s" . my/hydra-flyspell/body))

  :config
  (defhydra my/hydra-flyspell (:color teal :hint nil :idle 0.25)
    "
Flyspell

[_b_] check buffer            [_c_] cycle dictionaries     [_v_] version
[_p_] correct previous error  [_d_] change dictionary      [_x_] disable flyspell
[_n_] correct next error
    "
    ("<escape>" nil :exit t)
    ("C-o"      nil :exit t)
    ("C-g"      nil :exit t)
    ("q"        nil :exit t)

    ("b" flyspell-buffer)
    ("p" flyspell-correct-previous-word-generic)
    ("n" flyspell-correct-next-word-generic)
    ("c" my/cycle-ispell-languages)
    ("d" ispell-change-dictionary)
    ("v" ispell-check-version)
    ("x" flyspell-mode-off)))

;; ——

(provide 'cfg-flyspell)

;;; ————————————————————————————————————————————————————————
;;; cfg-flyspell.el ends here
