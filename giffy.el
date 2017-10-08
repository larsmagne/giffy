;;; giffy.el --- creating GIF animations -*- lexical-binding: t -*-
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

;; (giffy "/var/tmp/screenshots/Witness for the Prosecution/" "anim02")

(require 'cl)

(defvar giffy-animation-delay 40)
(defvar giffy-file-list nil)
(defvar giffy-start nil)
(defvar giffy-end nil)
(defvar giffy-reverse-back nil)
(defvar giffy-skip 0)
(defvar giffy-animation nil)
(defvar giffy-index 0)
(defvar giffy-direction 'forward)
(defvar giffy-paused nil)
(defvar giffy-timer nil)

(defun giffy (directory match)
  (interactive "DSource directory: \nsMatching files (regexp): ")
  (pop-to-buffer (generate-new-buffer match))
  (giffy-mode)
  (setq default-directory directory)
  (setq giffy-file-list (directory-files directory nil match)
	giffy-start 0
	giffy-end (1- (length giffy-file-list))
	giffy-timestamp (float-time))
  (giffy-display (current-buffer)))

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
    (define-key map "m" 'giffy-toggle-mode)
    (define-key map "p" 'giffy-pause)
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
  (setq-local giffy-skip 0)
  (setq-local giffy-paused nil)
  (setq-local giffy-timer nil)
  (setq-local giffy-direction 'forward)
  (setq-local giffy-reverse-back nil))

(defun giffy-display (buffer)
  (setq giffy-timer nil)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
	     (delay (- (float-time) giffy-timestamp))
	     (at-time (max 0.001 (- (/ giffy-animation-delay 1000.0) delay))))
	(erase-buffer)
	(insert-image (create-image (expand-file-name
				     (elt giffy-file-list giffy-index))))
	(insert "\n\n")
	(insert (format
		 "Start: %d  Stop: -%d  Skip: %d  Delay: %d  Mode: %s\nLength: %d  Index: %d  Real-Delay: %.5f\n"
		 giffy-start
		 (- (1- (length giffy-file-list)) giffy-end)
		 giffy-skip
		 giffy-animation-delay
		 (if giffy-reverse-back
		     'fold
		   'restart)
		 (length giffy-file-list)
		 giffy-index
		 at-time))
	;; Put the point at a nice unobtrusive place.
	(goto-char (point-min))
	(forward-line 1)
	(unless giffy-paused
	  (if (eq giffy-direction 'forward)
	      (progn
		(incf giffy-index (1+ giffy-skip))
		(when (> giffy-index giffy-end)
		  (if giffy-reverse-back
		      (setq giffy-direction 'backward
			    giffy-index (1- giffy-end))
		    (setq giffy-index giffy-start))))
	    (decf giffy-index (1+ giffy-skip))
	    (when (< giffy-index giffy-start)
	      (setq giffy-direction 'forward
		    giffy-index (1+ giffy-start))))
	  (setq giffy-timestamp (float-time))
	  (setq giffy-timer
		(run-at-time
		 at-time
		 nil
		 (lambda ()
		   (giffy-display buffer)))))))))

(defun giffy-pause ()
  "Toggle whether the animation is paused."
  (interactive)
  (setq giffy-paused (not giffy-paused))
  (when (and (not giffy-paused)
	     (not giffy-timer))
    (giffy-display (current-buffer))))

(defun giffy-toggle-mode ()
  "Toggle whether the animation loops back or restarts."
  (interactive)
  (setq giffy-reverse-back (not giffy-reverse-back)))

(defun giffy-adjust-start-earlier ()
  "Move the start point earlier."
  (interactive)
  (decf giffy-start)
  (when (< giffy-start 0)
    (setq giffy-start 0)))

(defun giffy-adjust-start-later ()
  "Move the start point later."
  (interactive)
  (incf giffy-start)
  (when (>= giffy-start giffy-end)
    (setq giffy-start (1- giffy-end))))

(defun giffy-adjust-end-earlier ()
  "Move the end point earlier."
  (interactive)
  (decf giffy-end)
  (when (<= giffy-end giffy-start)
    (setq giffy-end (1+ giffy-start))))

(defun giffy-adjust-end-later ()
  "Move the end point later."
  (interactive)
  (incf giffy-end)
  (when (>= giffy-end (length giffy-file-list))
    (setq giffy-end (1- (length giffy-file-list)))))

(defun giffy-decrease-skip ()
  "Decrease the number of frames are being skipped."
  (interactive)
  (decf giffy-skip)
  (when (< giffy-skip 0)
    (setq giffy-skip 0)))

(defun giffy-increase-skip ()
  "Increase the number of frames are being skipped."
  (interactive)
  (incf giffy-skip))

(defun giffy-decrease-rate ()
  "Decrease the number of frames are being rateped."
  (interactive)
  (decf giffy-animation-delay)
  (when (< giffy-animation-delay 0)
    (setq giffy-animation-delay 0)))

(defun giffy-increase-rate ()
  "Decrease the number of frames are being rateped."
  (interactive)
  (incf giffy-animation-delay))

(provide 'giffy)

;;; giffy.el ends here
