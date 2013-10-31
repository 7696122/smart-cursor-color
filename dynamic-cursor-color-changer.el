;;; dynamic-cursor-color-changer.el --- change cursor-color dynamically.
;; 
;; Filename: dynamic-cursor-color-changer.el
;; Description: 
;; Author: 7696122
;; Maintainer: 
;; Created: Thu Oct 31 21:33:34 2013 (+0900)
;; Version: 
;; Package-Requires: ()
;; Last-Updated: Thu Oct 31 21:56:48 2013 (+0900)
;;           By: 7696122
;;     Update #: 14
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Quickstart
;; 
;;       (require 'dynamic-cursor-color-changer)
;;       (dynamic-cursor-color-mode-on)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'eyedropper)

(defvar current-cursor-color nil
  "previous cursor color.")

(defun set-dynamic-cursor-color ()
  "Set cursor color dynamically."
  (let ((fcolor (eyedrop-foreground-at-point)))
    (if fcolor
        (unless (eq current-cursor-color fcolor)
          (let ((bcolor (eyedrop-background-at-point)))
            (unless (eq fcolor bcolor)
              (setq current-cursor-color fcolor)
              (set-cursor-color fcolor)))))))

;;;###autoload
(defun dynamic-cursor-color-mode-on ()
  "Turn on dynamic-cursor-color-mode."
  (interactive)
  (set-face-attribute 'cursor nil
                      :foreground nil
                      :background nil
                      :inverse-video t)
  (add-hook 'post-command-hook 'set-dynamic-cursor-color))

;;;###autoload
(defun dynamic-cursor-color-mode-off ()
  "Turn off dynamic-cursor-color-mode."
  (interactive)
  (remove-hook 'post-command-hook 'set-dynamic-cursor-color))


(provide 'dynamic-cursor-color-changer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dynamic-cursor-color-changer.el ends here
