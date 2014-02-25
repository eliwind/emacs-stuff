;;; w32-cursor.el --- Windows-like cursor behavior

;; Copyright 2000 Eli Daniel.

;; Author: Eli Daniel <elidaniel@mediaone.net>.  Based in part on code
;;         by Kyle E. Jones <kyle_jones@wonderworks.com>.
;; Maintainer: Eli Daniel <elidaniel@mediaone.net>
;; Version: 0.1, 12/20/2000

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, send e-mail to
;; this program's maintainer or write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file is intended for use with GNU Emacs on a Win32 platform. It
;; defines a mode that make the Emacs cursor act similarly to the usual
;; cursor in Windows applications.  This means that it appears as a
;; solid box when Emacs is in `overwrite-mode', and a blinking bar
;; otherwise.

;;; Installation:

;; Put this file somewhere in your `load-path' and byte-compile it.  Then
;; add this to your .emacs:
;;   (require 'w32-cursor)
;;   ;; set configuration variables here
;;   (w32-cursor-mode 1)

;;; Code:

(require 'timer)

;; (defvar w32-cursor-color (frame-parameter (selected-frame) 'cursor-color)
;;   "*Foreground cursor color")

;;(defvar w32-cursor-blink-rate .5
;;  "*Cursor blink rate (in seconds)")

;;(defvar w32-cursor-idle-time 1
;;  "*Time emacs must be idle before blinking starts (in seconds).")

(defvar w32-cursor-bar-width 2
  "*Width of cursor when it's in bar mode (in pixels)")

;; (defvar w32-cursor-color-index 0
;;   "Index of current cursor color in list of cursor colors.
;; Internal variable; do not change.")

;;(defvar w32-cursor-busy t
;;  "Non-nil means `w32-cursor-toggle-cursor-color' should change the cursor color.
;;Internal variable; do not change.")

(defvar w32-cursor-mode nil
  "Non-nil value means w32-cursor-mode is active.
Internal variable; do not change.")

(defvar w32-cursor-orig-cursor-type (frame-parameter (selected-frame) 'cursor-type)
  "Original cursor type (to be reverted to if w32-cursor-mode is turned off).
Internal variable; do not change.")

;;(defun w32-cursor-close-color (color)
;;  "Get a color very close to the specified one"
;;  (let* ((values (mapcar '(lambda (x) (/ x 257)) (x-color-values color)))
;;          (newvalues (mapcar '(lambda (x) (max (1- x) 0)) values))
;;          (newcolor (concat "almost " color)))
;;    (apply 'w32-define-rgb-color (append newvalues `(,newcolor)))
;;    newcolor))

;;(defun w32-cursor-blink ()
;;  "Toggle the cursor color unless we're in overwrite mode or `w32-cursor-busy' is non-nil."
;;  (unless (or overwrite-mode w32-cursor-busy)
;;    (setq w32-cursor-color-index (mod (1+ w32-cursor-color-index) 2))
;;    (let* ((bcolor (w32-cursor-close-color (frame-parameter (selected-frame) 'background-color)))
;;           (current-ccolor (frame-parameter (selected-frame) 'cursor-color))
;;           (ccolor w32-cursor-color))
;;      (when (not (equal current-ccolor bcolor))
;;        (setq w32-cursor-color current-ccolor)
;;        (setq ccolor current-ccolor))
;;      (set-cursor-color (nth w32-cursor-color-index (list ccolor bcolor))))))

(defun w32-cursor-mode (&optional arg)
  "Toggle w32-cursor-mode. With arg, turn w32-cursor-mode on iff arg is positive.
When w32-cursor-mode is enabled, the cursor appears as a box in overwrite mode and
a bar in insert mode.  When the cursor is a bar, it blinks when emacs is idle."
  (interactive "P")
  (setq w32-cursor-mode (or (and arg (> (prefix-numeric-value arg) 0))
				 (and (null arg) (null w32-cursor-mode))))
  (message "w32-cursor-mode: %s" w32-cursor-mode)
  (w32-blink-cursor)
  (cond ((null w32-cursor-mode)
         (modify-frame-parameters (selected-frame) `((cursor-type . ,w32-cursor-orig-cursor-type))))
	(t
         (modify-frame-parameters (selected-frame) `((cursor-type . ,(w32-cursor-get-cursor-type)))))))

(defmacro w32-cursor-get-cursor-type ()
  "Returns what should be the current cursor type, based on whether emacs is in insert mode."
  '(if overwrite-mode 'box
     (cons 'bar w32-cursor-bar-width)))

(defun w32-blink-cursor ()
  "Makes cursor blink, or not, depending on whether we're in insert or overwrite mode"
  (if overwrite-mode (blink-cursor-mode nil)
    (blink-cursor-mode t)))
  
(defadvice overwrite-mode (after w32-cursor-toggle-cursor-shape activate)
  "Toggle cursor between bar and box in the current frame."
  (w32-blink-cursor)
  (modify-frame-parameters
   (selected-frame)
   `((cursor-type . ,(w32-cursor-get-cursor-type)))))

(provide 'w32-cursor)
