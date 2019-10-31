;;; prf-theme.el --- Util to cycle themes -*- mode: emacs-lisp -*-

;; Copyright (C) 2015 Worney Renth <contact.perf@gmail.com>
;;
;; Version: 20130407.1256
;; X-Original-Version: 2.5
;; Created: July 27, 2010
;; Keywords: convenience, files, matching
;; Compatibility: GNU Emacs 22, 23, and 24
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
;; To install, copy this file somewhere in your load-path and add this line to
;; your .emacs:
;;
;;    (require 'prf-theme)


;;; Code:

;; NOTE: somebody did the same thing, for deftheme only, less clean as disabling whole list everytime: https://github.com/myTerminal/theme-looper/blob/master/theme-looper.el

;; TODO: test prf/theme/theme-list bound
;; TODO: prf/theme/theme-list change detection (via a clone) -> to prevent having to cycle whole list before reloading

;; ------------------------------------------------------------------------

(require 'dash)

;; ------------------------------------------------------------------------
;; INTERNAL VARS

(defvar prf/theme/current-theme nil)
(defvar prf/theme/current-theme-list nil) ;; more like a cursor
(defvar prf/theme/theme-list nil)
(defvar prf/theme/theme-list-change-detection nil)


;; ------------------------------------------------------------------------
;; FUNCTIONS: DEFTHEME

(defun prf/deftheme/apply-theme (theme)
  (setq prf/theme/current-theme theme)
  (load-theme theme t))


(defun prf/deftheme/revert-theme (theme)
  (disable-theme theme))


;; ------------------------------------------------------------------------
;; FUNCTIONS: COLOR-THEME (legacy)

(defun prf/color-theme/apply-theme (theme)
  (setq prf/theme/current-theme theme)
  (funcall theme))


(defun prf/color-theme/revert-theme (theme)
  nil)


;; ------------------------------------------------------------------------
;; DICHOTOMY

(if (>= emacs-major-version 24) ;; deftheme
    (progn
      (defalias 'prf/theme/apply-theme #'prf/deftheme/apply-theme)
      (defalias 'prf/theme/revert-theme #'prf/deftheme/revert-theme))
  (defalias 'prf/theme/apply-theme #'prf/color-theme/apply-theme)
  (defalias 'prf/theme/revert-theme #'prf/color-theme/revert-theme)
  (require 'color-theme)
  (with-eval-after-load "color-theme"
    (color-theme-initialize))
  (setq color-theme-is-global   t
        color-theme-is-cumulative t))


;; ------------------------------------------------------------------------
;; FUNCTIONS: theme list

(defun prf/theme-list/changed-p ()
  (not (eq prf/theme/theme-list-change-detection prf/theme/theme-list)))

(defun prf/theme-list/get-current ()
  (car prf/theme/current-theme-list))


(defun prf/theme-list/move-to-next ()
  (setq prf/theme/current-theme-list (cdr prf/theme/current-theme-list)))


(defun prf/theme-list/end-p ()
  (null prf/theme/current-theme-list))


;; ------------------------------------------------------------------------
;; FUNCTIONS: INTERACTIVE

(defun prf/theme/set-default-theme (&optional apply)
  (interactive)

  (if (null prf/theme/theme-list-change-detection)
      (setq prf/theme/theme-list-change-detection prf/theme/theme-list) )

  (if (not (null prf/theme/current-theme-list))
      (prf/theme/revert-theme prf/theme/current-theme) )
  (setq prf/theme/current-theme-list prf/theme/theme-list)
  (when apply
    (prf/theme/apply-theme (car prf/theme/current-theme-list))))


(defun prf/theme/cycle-theme ()
  (interactive)
  (prf/theme/revert-theme prf/theme/current-theme)

  (if (prf/theme-list/changed-p)
      (progn
	(setq prf/theme/theme-list-change-detection nil)
	(prf/theme/set-default-theme))
    (prf/theme-list/move-to-next)
    (when (prf/theme-list/end-p)
      (prf/theme/set-default-theme)))

  (prf/theme/apply-theme (prf/theme-list/get-current)))


(defun prf/theme/set-theme-from-list (theme)
  (when (stringp theme)
    (setq theme (intern theme)))

  (when (not (member theme prf/theme/theme-list))
    (error "Theme `%s' not in `prf/theme/theme-list' %S" theme prf/theme/theme-list))

  (prf/theme/revert-theme prf/theme/current-theme)
  (setq prf/theme/current-theme-list (cons theme
                                           (car (cdr (-split-on theme prf/theme/theme-list)))))
  (prf/theme/apply-theme (prf/theme-list/get-current)))


(defun prf/theme/initialize ()
  (interactive)
  (prf/theme/set-default-theme t))


;; ------------------------------------------------------------------------
;; HELM INTEGRATION

(with-eval-after-load "helm"

  (setq prf-theme--helm-source
        (helm-build-sync-source "Change theme to"
          :candidates prf/theme/theme-list
          :fuzzy-match t
          :action #'prf/theme/set-theme-from-list))

  (defun helm-prf-theme-choose ()
    (interactive)
    (let ((helm-candidate-number-limit 10000))
      (helm :sources '(prf-theme--helm-source)
            :buffer "*helm Choose Theme*"))))

;; ------------------------------------------------------------------------

(provide 'prf-theme)

;;; prf-theme.el ends here.
