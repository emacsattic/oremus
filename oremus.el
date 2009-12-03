;;;; Read the office
;;; Time-stamp: <2005-01-18 19:04:55 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'oremus)
(require 'cl)
(require 'w3)
(require 'autocue)

(defvar oremus-both-readings t
  "*Whether to have both readings in Oremus.")

(defvar oremus-buffer nil
  "The buffer holding the prepared Oremus reading.")

;;;###autoload
(defun prepare-oremus ()
  "Read the office."
  (interactive)
  (w3-fetch "http://www.oremus.org/oremus.cgi")
  (setq oremus-buffer (current-buffer))
  (w3-handle-replacements
   ;; each entry is:
   ;;   pattern for thing to replace
   ;;   condition for doing this replacement
   ;;   start for trimming replacement text
   ;;   end for trimming replacement text
   ;;   start for deleting the replaced text
   ;;   end for deleting the replaced text
   ;;   preamble to insert before replacement text
   ;;   postamble to insert after replacement text
   '(
     ;; ("opening canticle" t nil nil "An" "may be sung" "\nOpening Canticle\n" nil)
     ("Venite" (is-morning) 0 "This is material from"
      "A hymn appropriate to the time of day may be sung."
      "In the evening, Phos hilaron may be used."
      "Venite\n" "The Psalmody\n")
     ("Phos hilaron" (is-evening) "O gladsome Light" "Robert Bridges"
      "A hymn appropriate to the time of day may be sung."
      "In the evening, Phos hilaron may be used."
      "Phos hilaron\n" "The Psalmody\n")
     ("For another Biblical reading, \\([A-Z 1-2]+ [0-9]+:[0-9]*\\)" oremus-both-readings
      0 "-----------"
      nil nil
      (format "Second reading: %s" matched) ""
      )
     ("Benedictus" (is-morning) 0 "Glory . . ."
      "The Benedictus (Morning)" "(Night) may follow."
      "" "\n Glory to the Father, and to the Son,*\n  and to the Holy Spirit:\n as it was in the beginning, is now,*\n  and will be for ever. Amen.")
     ("Magnificat" (or (is-afternoon) (is-evening)) 0 "37 B"
      "The Benedictus (Morning)" "(Night) may follow.")
     ("Nunc dimittis" (is-night) 0 "back"
      "The Benedictus (Morning)" "(Night) may follow.")
     "Lord's Prayer"))
  (goto-char (point-min))
  (when (boundp 'oremus-pending)
    ;; interaction with type-break-setup: tell it there is a new Oremus to read
    (setq oremus-pending t)))

(defun show-oremus ()
  "Show the latest Oremus service.
This is separable from fetching the service, so that you can do all your
fetching of favourite daily pages in one go, at startup, and not have to
reconnect to the net (if using a modem)."
  (interactive)
  (unless oremus-buffer (prepare-oremus))
  (autocue oremus-buffer 1.5 1 "Oremus for"))

(defun is-morning ()
  "Return whether it appears to be the morning."
  (let ((hour (third (decode-time (current-time)))))
    (< hour 12)))

(defun is-afternoon ()
  "Return whether it appears to be the afternoon."
  (let ((hour (third (decode-time (current-time)))))
    (and (> hour 12)
	 (< hour 18))))

(defun is-evening ()
  "Return whether it appears to be the evening."
  (let ((hour (third (decode-time (current-time)))))
    (and (>= hour 18)
	 (<= hour 20))))

(defun is-night ()
  "Return whether it appears to be the night."
  (let ((hour (third (decode-time (current-time)))))
    (<= hour 21)))

(defun buffer-portion (buffer &optional start end)
  "Return part of BUFFER.
If optional START and END are given, return just that part of the buffer.
If they are numbers, those are character positions; if strings, search patterns."
  (save-window-excursion
    (message "buffer-portion on %s %S %S" buffer start end)
    (set-buffer buffer)
    (if start
	(if (numberp start)
	    (goto-char start)
	  (goto-char (point-min))
	  (re-search-forward start))
      (goto-char (point-min)))
    (let ((start-found (point)))
      (if end
	  (if (stringp end)
	      (progn
		(re-search-forward end)
		(goto-char (match-beginning 0)))
	    (goto-char end))
	(goto-char (point-max)))
      (buffer-substring start-found (point)))))

(defun w3-insert-linked-content-replacing-paragraph (&rest portion)
  "Find the page linked at point, and insert its text replacing the current paragraph.
If arguments are given, the first two specify the portion of the buffer to insert,
as given to buffer-portion.
Third and fourth arguments, if given, are used to find the section of the original
text to delete before inserting its replacement.
Fifth and sixth arguments, if given, are inserted before and after the insertion."
  (interactive)
  (message "Looking for replacement portion: %S" portion)
  (let* ((properties (cdr (get-char-property (point) (quote button))))
	 (href (plist-get properties :href)))
    (when href
      (message "Replacing link with contents from %s" href)
      (save-window-excursion
	(w3-fetch href))
      (message "Will try to retrieve text from buffer %s" (url-buffer-visiting href))
      (let* ((inhibit-read-only t)
	     (replacement-portion (if (<= (length portion) 2)
				      portion
				    (subseq portion 0 2)))
	     (replaced-portion-start (if (<= (length portion) 2)
					 nil
				       (nth 2 portion)))
	     (replaced-portion-end (if (<= (length portion) 3)
				       nil
				     (nth 3 portion)))
	     (leader (if (<= (length portion) 4)
			 nil
		       (nth 4 portion)))
	     (trailer (if (<= (length portion) 5)
			  nil
			(nth 5 portion)))
	     (par-start nil)
	     (new-text  (apply 'buffer-portion
			       (url-buffer-visiting href)
			       replacement-portion)))
	(message "Replacement portion given as: %S" replacement-portion)
	(message "Replaced portion given as: %S %S" replaced-portion-start replaced-portion-end)
	(if replaced-portion-start
	    (unless (re-search-backward replaced-portion-start (point-min) t)
	      (backward-paragraph))
	  (backward-paragraph))
	(setq par-start (point))
	(if replaced-portion-end
	    (unless (re-search-forward replaced-portion-end (point-max) t)
	      (forward-paragraph))
	  (forward-paragraph))
	(message "Old text is %s...%s"
		 (buffer-substring par-start (+ par-start 10))
		 (buffer-substring (- (point) 10) (point)))
	(message "New text is %s...%s"
		 (substring new-text 0 10)
		 (substring new-text -10))
	(delete-region par-start (point))
 	(when leader (insert (eval leader)))
	(insert new-text)
	(when trailer (insert (eval trailer)))))))

(defun w3-handle-replacements (replacements)
  "Handle REPLACEMENTS.
Each of these may be a regexp, which should match the text of an anchor,
or a list of a regexp, a condition form to decide whether to do this one,
and further arguments to w3-insert-linked-content-replacing-paragraph,
which see.
If the regexp has a parenthesized subexpression, use the start of that
as the point from which to find the URL, otherwise take the character
just before the end of the match."
  (goto-char (point-min))
  (dolist (replacement replacements)
    (let ((pattern (if (stringp replacement)
		       replacement
		     (car replacement)))
	  (condition (if (consp replacement)
			 (car (cdr replacement))
		       t))
	  (portion (if (and (consp replacement)
			    (consp (cdr replacement)))
		       (cdr (cdr replacement))
		     nil)))
      (message "On condition %S, handling possible insertion %s" condition pattern)
      (if (eval condition)
	  (progn
	    (message "Looking for %s to insert" pattern)
	    (if (re-search-forward pattern (point-max) t)
		(progn
		  (goto-char
		   (if (match-beginning 1)
		       (match-beginning 1)
		     (1- (point))))
		  (message "Moved onto %s.%s in buffer %s to do replacement"
			   (buffer-substring (- (point) 10) (- (point) 1))
			   (buffer-substring (point) (+ (point) 10))
			   (buffer-name))
		  (let* ((how-matched (if (match-beginning 1) 1 0))
			 (matched (buffer-substring (match-beginning how-matched)
						    (match-end how-matched))))
		    (apply 'w3-insert-linked-content-replacing-paragraph portion)))
	      (message "Could not find %s" pattern)))
	(message "Failed condition, will not look for %s" pattern)))))
