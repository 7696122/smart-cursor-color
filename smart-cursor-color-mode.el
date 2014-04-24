;;; smart-cursor-color-mode.el --- Change cursor color dynamically. -*- lexical-binding: t -*-
;;
;; Filename: smart-cursor-color-mode.el
;; Description: Change cursor color dynamically at cursor or pointer.
;; Author: 7696122
;; Maintainer: 7696122
;; Created: Thu Oct 31 21:33:34 2013 (+0900)
;; Version: 0.0.3
;; Package-Requires: ()
;; Last-Updated: Thu Apr 24 11:03:57 2014 (+0900)
;;           By: 7696122
;;     Update #: 366
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

(defvar sccm--last-cursor-color nil "Current cursor color.")

(defvar sccm--default-cursor-color (face-foreground 'default)
  "Default cursor color.")

(defvar sccm--saved-cursor-color (face-background 'cursor)
  "Saved cursor color.")

(defun sccm--set-cursor-color ()
  "Change cursor color dynamically."
  (let ((picked-color (foreground-color-at-point)))
    (if picked-color
        (unless (eq picked-color sccm--last-cursor-color)
          (setq sccm--last-cursor-color picked-color)
          (set-cursor-color sccm--last-cursor-color))
      (unless (eq sccm--default-cursor-color sccm--last-cursor-color)
        (setq sccm--last-cursor-color sccm--default-cursor-color)
        (set-cursor-color sccm--default-cursor-color)))))

;;;###autoload
(define-minor-mode smart-cursor-color-mode
  "Dynamically changed cursor color at point's color."
  :global t
  :group 'cursor
  (if smart-cursor-color-mode
      (progn
        (setq sccm--saved-cursor-color (face-background 'cursor))
        (add-hook 'post-command-hook #'sccm--set-cursor-color))
    (set-cursor-color sccm--saved-cursor-color)
    (remove-hook 'post-command-hook #'sccm--set-cursor-color)))

(provide 'smart-cursor-color-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; smart-cursor-color-mode.el ends here
