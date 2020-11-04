;;; helm-prf-theme.el --- Helm bindings to prf-theme

;; Copyright (C) 2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: theme
;; URL: https://github.com/p3r7/prf-theme
;; Package-Requires: ((helm "1.9.4"))
;;
;; Permission is hereby granted to use and distribute this code, with or
;; without modifications, provided that this copyright notice is copied with
;; it. Like anything else that's free, lusty-explorer.el is provided *as is*
;; and comes with no warranty of any kind, either expressed or implied. In no
;; event will the copyright holder be liable for any damages resulting from
;; the use of this software.

;;; Commentary:
;;  -----------
;;
;; For detailed instructions, please look at the README.md


;;; Code:



;; DEPS

(require 'helm)
(require 'helm-source)

(require 'prf-theme)



;; HELM COMMAND

(setq prf-theme--helm-source
      (helm-build-sync-source "Change theme to"
        :candidates (lambda () prf/theme/theme-list)
        :fuzzy-match t
        :action #'prf/theme/set-theme-from-list))

(defun helm-prf-theme-choose ()
  (interactive)
  (let ((helm-candidate-number-limit 10000))
    (helm :sources '(prf-theme--helm-source)
          :buffer "*helm Choose Theme*")))




(provide 'helm-prf-theme)

;;; helm-prf-theme.el ends here.
