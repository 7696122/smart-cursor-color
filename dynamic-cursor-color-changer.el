;;; dynamic-cursor-color-changer.el --- change cursor-color dynamically.
;; 
;; Filename: dynamic-cursor-color-changer.el
;; Description: 
;; Author: 7696122
;; Maintainer: 
;; Created: Thu Oct 31 21:33:34 2013 (+0900)
;; Version: 0.0.1
;; Package-Requires: ()
;; Last-Updated: Thu Nov  7 23:12:14 2013 (+0900)
;;           By: 7696122
;;     Update #: 91
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

(defun set-dynamic-cursor-color ()
  "Set cursor color dynamically."
  (unless (eq eyedrop-last-picked-color (pick-foreground-color))
    (set-cursor-color eyedrop-last-picked-color)))

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
