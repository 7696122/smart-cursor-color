;;; smart-cursor-color.el --- Change cursor color dynamically. -*- lexical-binding: t -*-
;;
;; Filename: smart-cursor-color.el
;; Description: Change cursor color dynamically at cursor or pointer.
;; Author: 7696122
;; Maintainer: 7696122
;; Created: Thu Oct 31 21:33:34 2013 (+0900)
;; Version: 0.0.4
;; Package-Requires: ()
;; Last-Updated: Fri Apr 25 22:30:58 2014 (+0900)
;;           By: 7696122
;;     Update #: 388
;; URL: https://github.com/7696122/smart-cursor-color
;; Doc URL:
;; Keywords: cursor, color, face
;; Compatibility: GNU Emacs: 24.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Quickstart
;;
;; To make the mode enabled every time Emacs starts, add the following
;; to Emacs initialisation file (~/.emacs or ~/.emacs.d/init.el):
;;
;; If installed from melpa.
;;       (smart-cursor-color-mode +1)
;;
;; If installed manually,
;;       (add-to-list 'load-path "path-to-installed-directory")
;;       (require 'smart-cursor-color)
;;       (smart-cursor-color-mode +1)
;;
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

(defvar scc--last-cursor-color nil
  "Current cursor color for smart-cursor-color-mode.")

(defvar scc--default-cursor-color (face-foreground 'default)
  "Default cursor color. When picked foreground color is nil, use this.")

(defvar scc--saved-cursor-color (face-background 'cursor)
  "Saved cursor color. When turn off smart-cursor-color-mode, restore origin cursor color.")

(defun scc--set-cursor-color ()
  "Change cursor color dynamically."
  (let ((picked-color (foreground-color-at-point)))
    (if picked-color
        (unless (eq picked-color scc--last-cursor-color)
          (setq scc--last-cursor-color picked-color)
          (set-cursor-color scc--last-cursor-color))
      (unless (eq scc--default-cursor-color scc--last-cursor-color)
        (setq scc--last-cursor-color scc--default-cursor-color)
        (set-cursor-color scc--default-cursor-color)))))

;;;###autoload
(define-minor-mode smart-cursor-color-mode
  "Dynamically changed cursor color at point's color."
  :lighter " scc" :global t :group 'cursor :require 'smart-cursor-color
  (if smart-cursor-color-mode
      (progn
        (setq scc--saved-cursor-color (face-background 'cursor))
        (add-hook 'post-command-hook #'scc--set-cursor-color))
    (remove-hook 'post-command-hook #'scc--set-cursor-color)
    (set-cursor-color scc--saved-cursor-color)))

;;;###autoload
(defun turn-on-smart-cursor-color ()
  "Unconditionally turn on `smart-cursor-color-mode'."
  (interactive)
  (smart-cursor-color-mode +1))

;;;###autoload
(defun turn-off-smart-cursor-color ()
  "Unconditionally turn off `smart-cursor-color-mode'."
  (interactive)
  (smart-cursor-color-mode -1))

(provide 'smart-cursor-color)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; smart-cursor-color.el ends here
