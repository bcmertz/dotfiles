(use-package general :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

    ;; simple command
    "SPC" '(avy-goto-word-or-subword-1  :which-key "go to char")
    "p"   (general-simulate-key "C-c p" :which-key "projectile")

    ;; Applications
    "a" '(:ignore t :which-key "Applications")
    "am" 'magit-status
    "ad" 'dired))

