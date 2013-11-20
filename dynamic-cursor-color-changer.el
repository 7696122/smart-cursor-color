;;; dynamic-cursor-color-changer.el --- Change cursor color dynamically.
;; 
;; Filename: dynamic-cursor-color-changer.el
;; Description: Change cursor color dynamically at cursor or pointer.
;; Author: 7696122
;; Maintainer: 7696122
;; Created: Thu Oct 31 21:33:34 2013 (+0900)
;; Version: 0.0.1
;; Package-Requires: ()
;; Last-Updated: Wed Nov 20 22:33:52 2013 (+0900)
;;           By: 7696122
;;     Update #: 261
;; URL: https://github.com/7696122/dynamic-cursor-color-changer
;; Doc URL: 
;; Keywords: cursor, color, face
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x
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

(defvar cursor-color (face-foreground 'cursor) "cursor face's foreground color.")
(defvar current-cursor-color cursor-color "current cursor color.")

(defun set-dynamic-cursor-color ()
  "Set cursor color dynamically."
  (let ((c (foreground-color-at-point)))
    (unless (eq c current-cursor-color)
      (if c
          (progn
            (setq current-cursor-color c)
            (set-cursor-color current-cursor-color))
        (progn
          (unless cursor-color
            (setq cursor-color (face-foreground 'cursor)))
          (unless (eq cursor-color current-cursor-color)
            (setq current-cursor-color cursor-color)
            (set-cursor-color cursor-color)))))))

;;;###autoload
(defun dynamic-cursor-color-mode-on ()
  "Turn on dynamic-cursor-color-mode."
  (interactive)
  (add-hook 'post-command-hook 'set-dynamic-cursor-color))

;;;###autoload
(defun dynamic-cursor-color-mode-off ()
  "Turn off dynamic-cursor-color-mode."
  (interactive)
  (remove-hook 'post-command-hook 'set-dynamic-cursor-color))

(provide 'dynamic-cursor-color-changer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dynamic-cursor-color-changer.el ends here
