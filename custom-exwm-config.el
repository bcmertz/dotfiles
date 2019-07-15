;;; custom-exwm-config.el --- EXWM configuration
;;; Commentary:
;;
;; Personalized configuration for EXWM
;;
;;; Code:

(require 'exwm)
(require 'exwm-randr)
(require 'exwm-systemtray)

(defun custom-exwm-config-global-keys ()
  "Configure Keybindings for exwm."

  ;; Bind window cycling to Meta-Tab & Meta-Shift-Tab
  (exwm-input-set-key (kbd "<M-tab>") #'other-window)
  (exwm-input-set-key
   (kbd "<M-iso-lefttab>")
   #'(lambda () (interactive) (other-window -1)))

  ;; Unbind <M-tab> when in magit
  (with-eval-after-load 'magit-mode
    (define-key magit-mode-map (kbd "<M-tab>") nil))
  ;; Unbind <M-tab> in eshell
  (add-hook 'eshell-mode-hook
            '(lambda () (define-key eshell-mode-map (kbd "<M-tab>") nil)))

  ;; Bind keys for resizing windows
  (exwm-input-set-key (kbd "C-{") #'shrink-window-horizontally)
  (exwm-input-set-key (kbd "C-}") #'enlarge-window-horizontally)
  (exwm-input-set-key (kbd "C-M-}") 'shrink-window)
  (exwm-input-set-key (kbd "C-M-{") 'enlarge-window)

  ;; 's-r': Reset
  (exwm-input-set-key (kbd "s-r") #'exwm-reset)

  ;; 's-w': Switch workspace
  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)

  ;; 's-N': Switch to certain workspace
  (dotimes (i exwm-workspace-number)
    (exwm-input-set-key (kbd (format "s-%d" (1+ i)))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))

  ;; 's-&': Launch application
  (exwm-input-set-key (kbd "s-&")
                      (lambda (command)
                        (interactive (list (read-shell-command "$ ")))
                        (start-process-shell-command command nil command))))


(defun custom-exwm-config-editing-keys ()
  "Configure Line-editing shortcuts for X11 apps."
  (exwm-input-set-simulation-keys
   '(([?\C-b] . left)
     ([?\C-f] . right)
     ([?\C-p] . up)
     ([?\C-n] . down)
     ([?\C-a] . home)
     ([?\C-e] . end)
     ([?\M-v] . prior)
     ([?\C-v] . next)
     ([?\C-d] . delete)
     ([?\C-y] . 22)                     ; Paste
     ([?\M-w] . 3)                      ; Copy
     ([?\C-k] . (S-end 24)))))          ; Select til the end & Cut


(defun custom-exwm-config-shell-command ()
  "Configure Async Shell Command buffer options."

  ;; Don't ask if a new buffer should be created. Just do it!
  (setq async-shell-command-buffer 'new-buffer)

  ;; Don't show buffer with output for asynchronous commands
  (add-to-list
   'display-buffer-alist
   '(".*Async Shell Command.*" (display-buffer-no-window))))


(defun custom-exwm-config-buffer-name ()
  "Rename buffer according to WM_CLASS and WM_NAME (or _NET_WM_NAME)."
  (exwm-workspace-rename-buffer (concat exwm-class-name " - " exwm-title)))


(defun custom-exwm-config-wm-options ()
  "Set misc Window Manager options."

  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook #'custom-exwm-config-buffer-name)
  (add-hook 'exwm-update-title-hook #'custom-exwm-config-buffer-name)

  ;; Set the initial workspace number.
  (setq exwm-workspace-number 5))


(defun custom-exwm-x11-helpers ()
  "Setup Desktop Background & X11 Key mapping."
  (shell-command "xsetroot -default && xmodmap ~/.Xmodmap")
  (shell-command "xbindkeys"))

(defun custom-exwm-randr ()
  "Setup xrandr defaults."
  (setq exwm-randr-workspace-output-plist '(0 "eDP" 1 "DisplayPort-0"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output eDP --left-of DisplayPort-0 --auto")))
  (exwm-randr-enable))

(defun custom-exwm-config ()
  "Custom configuration of EXWM."
  (when (not (eq system-type 'darwin))
    (custom-exwm-x11-helpers)
    (custom-exwm-config-wm-options)
    (custom-exwm-config-shell-command)
    (custom-exwm-config-global-keys)
    (custom-exwm-config-editing-keys)
    (custom-exwm-randr)
    (exwm-systemtray-enable)
    (exwm-enable)))


(provide 'custom-exwm-config)
;;; custom-exwm-config ends here
