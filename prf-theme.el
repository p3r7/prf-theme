;;; prf-theme.el --- Util to cycle themes

;; Copyright (C) 2010-2020 Jordan Besly
;;
;; Version: 0.1.0
;; Created: July 27, 2010
;; Keywords: theme
;; URL: https://github.com/p3r7/prf-theme
;; Package-Requires: ((dash "2.16.0"))
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

(require 'dash)

;; tame compiler warnings
(unless (featurep 'color-theme)
  (defvar color-theme-is-global nil)
  (defvar color-theme-is-cumulative nil)
  (defun color-theme-initialize ()))



;; CONFIG

(defvar prf/theme/theme-list nil)



;; INTERNAL VARS

(defvar prf/theme/theme-list-change-detection nil)

(defvar prf/theme/current-theme nil)
(defvar prf/theme/current-theme-list nil) ;; more like a cursor



;; FUNCTIONS: DEFTHEME

(defun prf/deftheme/apply-theme (theme)
  (setq prf/theme/current-theme theme)
  (unless (eq theme 'default)
    (load-theme theme t)))


(defun prf/deftheme/revert-theme (theme)
  (unless (eq theme 'default)
    (disable-theme theme)))



;; FUNCTIONS: COLOR-THEME (legacy)

(defun prf/color-theme/apply-theme (theme)
  (setq prf/theme/current-theme theme)
  (unless (eq theme 'default)
    (funcall theme)))


(defun prf/color-theme/revert-theme (theme)
  nil)



;; THEME IMPLEM DICHOTOMY

(if (>= emacs-major-version 24)
    (progn
      (defalias 'prf/theme/apply-theme #'prf/deftheme/apply-theme)
      (defalias 'prf/theme/revert-theme #'prf/deftheme/revert-theme))
  (defalias 'prf/theme/apply-theme #'prf/color-theme/apply-theme)
  (defalias 'prf/theme/revert-theme #'prf/color-theme/revert-theme)
  (require 'color-theme)
  (with-eval-after-load "color-theme"
    (color-theme-initialize))
  (setq color-theme-is-global     t
        color-theme-is-cumulative t))



;; FUNCTIONS: THEME LIST

(defun prf/theme-list/changed-p ()
  (not (eq prf/theme/theme-list-change-detection prf/theme/theme-list)))

(defun prf/theme-list/get-current ()
  (car prf/theme/current-theme-list))

(defun prf/theme-list/move-to-next ()
  (setq prf/theme/current-theme-list (cdr prf/theme/current-theme-list)))

(defun prf/theme-list/end-p ()
  (null prf/theme/current-theme-list))



;; COMMANDS

(defun prf/theme/set-default-theme (&optional apply)
  (interactive)

  (unless prf/theme/theme-list
    (error "Empty list `prf/theme/theme-list'"))

  (if (null prf/theme/theme-list-change-detection)
      (setq prf/theme/theme-list-change-detection prf/theme/theme-list) )

  (if (not (null prf/theme/current-theme-list))
      (prf/theme/revert-theme prf/theme/current-theme))
  (setq prf/theme/current-theme-list prf/theme/theme-list)
  (when apply
    (prf/theme/apply-theme (car prf/theme/current-theme-list))))


(defun prf/theme/cycle-theme ()
  (interactive)

  (unless prf/theme/theme-list
    (error "Empty list `prf/theme/theme-list'"))

  (prf/theme/revert-theme prf/theme/current-theme)

  (if (prf/theme-list/changed-p)
      (progn
	(setq prf/theme/theme-list-change-detection nil)
	(prf/theme/set-default-theme))
    (prf/theme-list/move-to-next)
    (when (prf/theme-list/end-p)
      (prf/theme/set-default-theme)))

  (prf/theme/apply-theme (prf/theme-list/get-current))

  (prf/theme/font-lock-refresh-special-buffers))


(defun prf/theme/set-theme-from-list (theme)
  (when (stringp theme)
    (setq theme (intern theme)))

  (when (not (member theme prf/theme/theme-list))
    (error "Theme `%s' not in `prf/theme/theme-list' %S" theme prf/theme/theme-list))

  (prf/theme/revert-theme prf/theme/current-theme)
  (setq prf/theme/current-theme-list (cons theme
                                           (car (cdr (-split-on theme prf/theme/theme-list)))))
  (prf/theme/apply-theme (prf/theme-list/get-current))

  (prf/theme/font-lock-refresh-special-buffers))


(defun prf/theme/initialize ()
  (interactive)
  (prf/theme/set-default-theme t))



;; PRIVATE HELPERS: FONT LOCK

(defun prf/theme/font-lock-refresh ()
  ;; NB: necessary for stuff such as polymode buffers
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))


(defun prf/theme/font-lock-refresh-special-buffers ()
  ;; polymode
  (save-window-excursion
    (--map
     (when (buffer-live-p it)
       (switch-to-buffer it)
       (when (and (boundp 'polymode-mode)
                  polymode-mode
                  (string-match-p "^.*\\[.*\\]$" (buffer-name)))
         (prf/theme/font-lock-refresh)))
     (buffer-list))))




(provide 'prf-theme)

;;; prf-theme.el ends here.
