;;; custom-arduino.el --- arduino configuratiob
;;;
;;; Commentary:
;;;
;;;
;;; Code:

(use-package arduino-mode
  :mode "\\.ino\\'"
  :defer t
  )

;; make useful keys work in serial terminal
(eval-after-load 'term
  '(define-key term-raw-map (kbd "M-x") #'execute-extended-command))
(eval-after-load 'term
  '(define-key term-raw-map (kbd "C-x") nil))
(eval-after-load 'term
  '(define-key term-raw-map (kbd "C-x k") #'persp-kill-buffer*))

(defun arduino-open-example ()
  "Open Ardiuno example sketch."
  (interactive "")
  (setq dir "/usr/share/arduino/examples")
  (save-excursion
    (setq file (counsel-find-file dir))
    )
  (save-excursion
    (let ((buffer (generate-new-buffer "scratch.ino")))
      (switch-to-buffer buffer)
      (arduino-mode)
      (insert-file-contents file)
      (setq filename (file-name-sans-extension (file-name-nondirectory file)))
      (setq directory (concat "~/coding/arduino/" filename))
      (mkdir directory)
      (write-file (concat (concat directory "/") (file-name-nondirectory file)))
      ))
  (kill-buffer (file-name-nondirectory file)))

;; BOARD_TAG - MONITOR_PORT
;; uno - /dev/ttyACM0
;; nano - /dev/ttyUSB0
;;
(defun arduino-makefile ()
  "Insert Arduino Makefile."
  (interactive)
  (setq nano-info "BOARD_TAG    = nano
MONITOR_PORT = /dev/ttyUSB0
include /usr/share/arduino/Arduino.mk
")
  (setq uno-info "BOARD_TAG    = uno
MONITOR_PORT = /dev/ttyACM0
include /usr/share/arduino/Arduino.mk")
  (setq board (completing-read "Board: " '("nano" "uno")))
  (if (string= board "nano")
      (insert-makefile-info nano-info)
    (if (string= board "uno")
        (insert-makefile-info uno-info))))


(defun insert-makefile-info (board-info)
  "Helper to insert BOARD-INFO specific Makefile info."
  (save-excursion
    (let ((buffer (generate-new-buffer "Makefile")))
      (switch-to-buffer buffer)
      (insert board-info)
      (write-file "Makefile")
      ))
)


;;; custom-arduino.el ends here
