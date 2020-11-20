;;; custom-exwm-config.el --- EXWM configuration
;;; Commentary:
;;;
;;; Personalized configuration for EXWM
;;; Copied line for line from @clarete
;;;
;;; Code:

(require 'exwm)
(require 'exwm-config)
(exwm-config-example)
(require 'exwm-systemtray)
(exwm-systemtray-enable)

;; 's-&': Launch application
(exwm-input-set-key (kbd "s-&")
                    (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command command nil command)))


;;   ;; Bind keys for resizing windows
;;   (exwm-input-set-key (kbd "C-{") #'shrink-window-horizontally)
;;   (exwm-input-set-key (kbd "C-}") #'enlarge-window-horizontally)
;;   (exwm-input-set-key (kbd "C-M-}") 'shrink-window)
;;   (exwm-input-set-key (kbd "C-M-{") 'enlarge-window)

;;   ;; 's-r': Reset
;;   (exwm-input-set-key (kbd "s-r") #'exwm-reset)
;;   (define-key exwm-mode-map (kbd "C-c") nil)

;;   ;; 's-w': Switch workspace
;;   (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)

;;   ;; 's-N': Switch to certain workspace
;;   (dotimes (i exwm-workspace-number)
;;     (exwm-input-set-key (kbd (format "s-%d" (1+ i)))
;;                         `(lambda ()
;;                            (interactive)
;;                            (exwm-workspace-switch-create ,i))))
;; )


;; (defun custom-exwm-config-editing-keys ()
;;   "Configure Line-editing shortcuts for X11 apps."
;;   (exwm-input-set-simulation-keys
;;    '(([?\C-b] . left)
;;      ([?\C-f] . right)
;;      ([?\C-p] . up)
;;      ([?\C-n] . down)
;;      ([?\C-a] . home)
;;      ([?\C-e] . end)
;;      ([?\M-v] . prior)
;;      ([?\C-v] . next)
;;      ([?\C-d] . delete)
;;      ([?\C-y] . 22)                     ; Paste
;;      ([?\M-w] . 3)                      ; Copy
;;      ([?\C-k] . (S-end 24)))))          ; Select til the end & Cut


;;   ;; Set the initial workspace number.
;;   (setq exwm-workspace-number 5))

;; (defun custom-exwm-randr ()
;;   "Setup xrandr defaults."
;;   (setq exwm-randr-workspace-output-plist '(0 "eDP" 1 "DisplayPort-0"))
;;   (add-hook 'exwm-randr-screen-change-hook
;;             (lambda ()
;;               (start-process-shell-command
;;                "xrandr" nil "xrandr --output eDP --left-of DisplayPort-0 --auto")))
;;   (exwm-randr-enable))

;;; custom-exwm-config ends here
