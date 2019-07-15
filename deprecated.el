;; Failed attempts at various things, for future reference

;; (defun return-indent ()
;;  (move-end-of-line)
;;  (<RET>))
;;
;; (global-set-key (kbd "M-j") 'return-indent)
;;
;; autocompletion with company mode ;; removed because slower (?) maybe not its probabyl actually better
;;(require 'company)
;;(setq company-tooltip-limit 20)                      ; bigger popup window
;;(setq company-idle-delay .5)                         ; decrease delay before autocompletion popup shows
;;(setq company-echo-delay 0)                          ; remove annoying blinking
;;(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
;;(require 'company-go) ;; if switching back to company mode, put lines 2-3 in go-mode-setup
;; (set (make-local-variable 'company-backends) '(company-go))
;; (company-mode)

;; add in flycheck, broken rn for go bc of staticcheck fuckery
;; (add-hook 'after-init-hook #'global-flycheck-mode)


;i dont need relative line numbering with avy / cant figure out the default goto with it
;(display-line-numbers-mode)   ; give display numbers
;(setq display-line-numbers-type "relative")
; i think to use this i'd have to bind M-g M-g to a custom function which subtracts line number entered by the current display-line-number line number or smthing like that

; Deprecated line numbering
;(global-nlinum-relative-mode)                 ; add line numbers on side
;(setq nlinum-relative-redisplay-delay 1)      ; make snappy
;(setq nlinum-relative-current-symbol "->")    ; make pretty
;(setq nlinum-format "%d  ")                   ; make spacious 
;(setq nlinum-relative-toggle)


; indent-rigidly ????? [WIP]
    ;(global-set-key (kbd "[?\C-\t]") 'indent-rigidly-right-to-tab-stop)
    ;(global-set-key (kbd "C-<BACKTAB>") 'indent-rigidly-left-to-tab-stop)
; [?\C-\t]


;;(server-start) taken care of on startup of linux by $  --daemon ;;RUN SERVER / CLIENT SETUP
