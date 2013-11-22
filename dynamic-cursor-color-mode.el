;;; dynamic-cursor-color-changer.el --- Change cursor color dynamically.
;;
;; Filename: dynamic-cursor-color-changer.el
;; Description: Change cursor color dynamically at cursor or pointer.
;; Author: 7696122
;; Maintainer: 7696122
;; Created: Thu Oct 31 21:33:34 2013 (+0900)
;; Version: 0.0.1
;; Package-Requires: ()
;; Last-Updated: Fri Nov 22 13:51:11 2013 (+0900)
;;           By: 7696122
;;     Update #: 309
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

(defvar cursor-color (face-background 'default) "Cursor face's foreground color.")
(defvar current-cursor-color cursor-color "Current cursor color.")

(defcustom dynamic-cursor-color-mode nil
  "Non-nil means change cursor color dynamically."
  :group 'cursor
  :type 'boolean)

(defun set-dynamic-cursor-color ()
  "Set cursor color dynamically."
  (let ((picked-color (foreground-color-at-point)))
    (unless (eq picked-color current-cursor-color)
      (if picked-color
          (progn
            (setq current-cursor-color picked-color)
            (set-cursor-color current-cursor-color))
        (unless (eq cursor-color current-cursor-color)
          (setq current-cursor-color cursor-color)
          (set-cursor-color cursor-color))))))

(if dynamic-cursor-color-mode
    (add-hook 'after-init-hook
              (lambda ()
                (add-hook 'post-command-hook 'set-dynamic-cursor-color))))

(provide 'dynamic-cursor-color-changer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dynamic-cursor-color-changer.el ends here
