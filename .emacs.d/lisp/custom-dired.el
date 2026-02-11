;;; custom-dired.el --- dired configuration -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

(use-package ls-lisp
  :straight (:type built-in)
  :config
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-insert-directory-program nil))

(use-package dired
  :straight (:type built-in)
  :bind (:map dired-mode-map
              ("M-s" . consult-ripgrep)))

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("C-c C-t" . wdired-change-to-wdired-mode)
              ("/ /" . dired-narrow)
              ("/ f" . dired-narrow-fuzzy)
              ("/ r" . dired-narrow-regexp)))

(use-package dired-subtree
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map ("<tab>" . dired-subtree-toggle)))

(use-package dired-ranger
  :after dired
  :bind (:map dired-mode-map
              ("C" . dired-ranger-copy)
              ("P" . dired-ranger-paste)
              ("M" . dired-ranger-move)))

(use-package all-the-icons-dired
  :diminish
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  ;; from centaur-emacs https://github.com/seagle0128/.emacs.d/blob/334d9afaedd67bc10f207ca72d6daff5ac6469cf/lisp/init-dired.el
  ;; BROKEN - work in progress
  ;; (with-no-warnings
  ;;    (advice-add 'dired :around #'all-the-icons-dired--refresh-advice))

  (with-no-warnings
    (defun my-all-the-icons-dired--refresh ()
      "Display the icons of files in a dired buffer."
      (all-the-icons-dired--remove-all-overlays)
      ;; NOTE: don't display icons in remote folders or the folder has too many items
      (if (and (not (file-remote-p default-directory))
               ;; (not (string-match-p "sudo\:root\@localhost" (buffer-file-name)))
               (string-match "localhost" (buffer-file-name))
               (<= (count-lines (point-min) (point-max)) 300))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (dired-move-to-filename nil)
                (let ((file (dired-get-filename 'relative 'noerror)))
                  (when file
                    (let ((icon (if (file-directory-p file)
                                    (all-the-icons-icon-for-dir file
                                                                :face 'all-the-icons-dired-dir-face
                                                                :height 0.9
                                                                :v-adjust all-the-icons-dired-v-adjust)
                                  (all-the-icons-icon-for-file file :height 0.9 :v-adjust all-the-icons-dired-v-adjust))))
                      (if (member file '("." ".."))
                          (all-the-icons-dired--add-overlay (point) "  \t")
                        (all-the-icons-dired--add-overlay (point) (concat icon "\t")))))))
              (forward-line 1)))
        (message "Remote folder or too many items.")))
    (advice-add #'all-the-icons-dired--refresh :override #'my-all-the-icons-dired--refresh)))

(provide 'custom-dired)
;;; custom-dired.el ends here
