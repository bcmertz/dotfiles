;;; custom-music.el --- music player config  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; music player config
;;;
;;; Code:

(require 'emms-setup)
(emms-all)
(emms-default-players)

(setq emms-source-file-default-directory "~/media/music/")

(provide 'custom-music)
;;; custom-music.el ends here
