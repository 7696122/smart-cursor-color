;;; smart-cursor-color-mode.el --- Change cursor color dynamically.
;;
;; Filename: smart-cursor-color-mode.el
;; Description: Change cursor color dynamically at cursor or pointer.
;; Author: 7696122
;; Maintainer: 7696122
;; Created: Thu Oct 31 21:33:34 2013 (+0900)
;; Version: 0.0.1
;; Package-Requires: ()
;; Last-Updated: Tue Apr 22 23:32:16 2014 (+0900)
;;           By: 7696122
;;     Update #: 337
;; URL: https://github.com/7696122/smart-cursor-color-mode
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
;;       (require 'smart-cursor-color-mode)
;;       (setq smart-cursor-color-mode +1)
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

(defvar current-cursor-color nil "Current cursor color.")

(defun smart-cursor-color ()
  "Change cursor color dynamically."
  (let ((picked-color (foreground-color-at-point))
        (foreground-color (face-foreground 'default)))
    (if picked-color
        (unless (eq picked-color current-cursor-color)
          (setq current-cursor-color picked-color)
          (set-cursor-color current-cursor-color))
      (unless (eq foreground-color current-cursor-color)
        (setq current-cursor-color foreground-color)
        (set-cursor-color foreground-color)))))

(define-minor-mode smart-cursor-color-mode
  "Dynamically changed cursor color at point's color."
  :global t
  :group 'cursor
  (if smart-cursor-color-mode
      (progn
        (add-hook 'pre-command-hook #'smart-cursor-color)
        (add-hook 'post-command-hook #'smart-cursor-color))
    (remove-hook 'pre-command-hook #'smart-cursor-color)
    (remove-hook 'post-command-hook #'smart-cursor-color)))

(provide 'smart-cursor-color-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; smart-cursor-color-mode.el ends here
