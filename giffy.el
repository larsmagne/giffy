;;; giffy.el --- creating GIF animations
;; Copyright (C) 2017 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: fun, images

;; This file is not part of GNU Emacs.

;; giffy.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; giffy.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'cl)

(defvar giffy-animation-delay 40)
(defvar giffy-file-list nil)
(defvar giffy-start nil)
(defvar giffy-end nil)
(defvar giffy-reverse-back nil)
(defvar giffy-animation nil)
(defvar giffy-index 0)
(defvar giffy-direction 'forward)

(defun giffy (directory match)
  (interactive "DSource directory: \nsMatching files (regexp): ")
  (pop-to-buffer (generate-new-buffer match))
  (giffy-mode)
  (setq default-directory directory)
  (setq giffy-file-list (directory-files directory nil match)
	giffy-start 0
	giffy-end (1- (length giffy-file-list))
	giffy-timestamp (float-time))
  (giffy-display))

(defvar giffy-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "z" 'giffy-adjust-start-earlier)
    (define-key map "x" 'giffy-adjust-start-later)
    (define-key map "n" 'giffy-adjust-end-earlier)
    (define-key map "m" 'giffy-adjust-end-later)
    (define-key map "<" 'giffy-decrease-skip)
    (define-key map ">" 'giffy-increase-skip)
    (define-key map "(" 'giffy-decrease-rate)
    (define-key map ")" 'giffy-increase-rate)
    (define-key map "w" 'giffy-write-gif)
    map))

(define-derived-mode giffy-mode special-mode "Giffy"
  "Major mode for creating animated GIF images.

\\{giffy-mode-map}"
  (setq buffer-read-only t)
  (setq-local giffy-animation-delay 40)
  (setq-local giffy-file-list nil)
  (setq-local giffy-start nil)
  (setq-local giffy-end nil)
  (setq-local giffy-animation nil)
  (setq-local giffy-timestamp 0)
  (setq-local giffy-index 0)
  (setq-local giffy-direction 'forward)
  (setq-local giffy-reverse-back nil))

(provide 'giffy)

;;; giffy.el ends here
