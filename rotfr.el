;;; rotfr.el --- Rotate phrases -*- lexical-binding: t -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;; Author: Herbert Jones <jones.herbert@gmail.com>
;; Version: 0.0.0
;; Package-Requires: ((emacs "24.0") (cl-lib "0.5"))
;; Keywords: rotation phrases

;;; Commentary:
;; Rotate among sets of phrases.

;;; Code:
(require 'dash)
(require 'cl-lib)

(defvar rotfr-rotation-sets
  '((:rot ("yes" "no" "maybe so"))
    (:mode php-mode :rot ("public" "protected" "private"
                          "public static" "protected static" "private static"))
    (:mode php-mode :rot ("@param[in]" "@param[out]" "@param[in/out]"))
    (:mode php-mode :rot ("class" "abstract class" "interface"))
    (:mode php-mode :rot ("namespace" "use"))
    (:mode php-mode :rot ("==" "===" "!=" "!==" "<" "<=" ">" ">="))
    (:mode php-mode :rot ("self::" "$this->"))
    (:mode emacs-lisp-mode :rot ("cl-labels" "cl-flet"))
    (:mode emacs-lisp-mode :rot ("let" "let*"))
    (:mode emacs-lisp-mode :rot ("buffer-substring" "buffer-substring-no-properties"))
    (:mode emacs-lisp-mode :rot ("substring" "substring-no-properties"))
    (:mode emacs-lisp-mode :rot ("string=" "string-equal"))
    (:mode emacs-lisp-mode :rot ("string<" "string-lessp"))
    (:mode emacs-lisp-mode :rot ("string>" "string-greaterp"))
    (:mode prog-mode :rot ("&&" "||"))
    (:mode text-mode :rot ("and" "or" "either")))
  "Rotate phrase phrase rotations.

List of plists where each plist must have:
:rot  List of words or phrases to cycle through.

Plists may have the keys:
:mode  symbol or list of symbols that must match the major mode of the
       buffer.
:bounds  ensures phrase has boundries that match thing.  By default
         thing is 'same-syntax.")

(defun rotfr--rotate-match-mode-p (group)
  "Determine if the mode of the current buffer is a match for the GROUP."
  (cl-labels ((matchp (mode)
                      (cl-typecase mode
                        (cons (-any #'matchp mode))
                        (symbol (derived-mode-p mode))
                        (t (error "Unknown group mode: %S" (type-of mode))))))
    (let ((mode (plist-get group :mode)))
      (if mode
          (matchp (plist-get group :mode))
        t))))

(defun rotfr--phrase-match-point-p (search-point phrase)
  "Determine if there is a match of the PHRASE around the area at SEARCH-POINT.

Returns start and end position as a list."
  (let* ((len (length phrase))
         (start (max (point-min) (- search-point (1- len))))
         (end (min (+ search-point len) (point-max)))
         (ss (buffer-substring-no-properties start end))
         (pos (cl-search phrase ss)))
    (when pos
      (list (+ start pos) (+ start pos len)))))

(defun rotfr--bounds-match-thing (start end thing)
  "Ensure bounds of phrase match are bound by THING.

Check that the phrase captured between START and END are also
bound at the endpoints according to THING."
  (let ((start-bounds (save-excursion (goto-char start) (bounds-of-thing-at-point thing)))
        (end-bounds (save-excursion (goto-char (1- end)) (bounds-of-thing-at-point thing))))
    (and (equal start (car start-bounds))
         (equal end (cdr end-bounds)))))

(defun rotfr--find-match-for-group (group &optional reversed)
  "Attempt to match a GROUP around the current point.

Will rotate REVERSED when non nil.

On success, returns as a list the phrase start point, end point,
and replacement phrase."
  (when (rotfr--rotate-match-mode-p group)
    (let* ((search-point (point))
           (rot (plist-get group :rot))
           (bounds-full (plist-member group :bounds))
           (bounds (if bounds-full (cadr bounds-full) 'same-syntax))
           best-start best-end best-length best-next-word)
      (when reversed
        (setq rot (reverse rot)))
      (cl-labels
          ((possible-better-match (word)
                                  ;; Can only match if no existing match or this
                                  ;; match is longer
                                  (or (not best-next-word)
                                      (< best-length (length word))))
           (update-matches (word next-word)
                           (when-let (((possible-better-match word))
                                      (found (rotfr--phrase-match-point-p search-point word))
                                      ((or (not bounds)
                                           (rotfr--bounds-match-thing
                                            (elt found 0) (elt found 1) bounds))))
                             (setq best-start (elt found 0)
                                   best-end (elt found 1)
                                   best-length (length word)
                                   best-next-word next-word))))
        (map nil #'update-matches rot (cdr (-cycle rot))))
      (when best-next-word
        (list best-start best-end best-next-word)))))

(setq rotfr--last-used nil)

(defun rotfr--maintain-relative-point (start-point end-point new-phrase-len)
  "Determine what the new point should be after replacement.

Maintains the old positioning so that repeated calls do not move
point in a random fashion.  New position is based off the
position of point in comparison with its original relative position.

START-POINT and END-POINT being the points where the replacement
will be done.  NEW-PHRASE-LEN is the length of the new
replacement.  Returns where point should be moved to."
  (let (from-end relative-len)
    (if (and (eql start-point (elt rotfr--last-used 0))
             (eql (point) (elt rotfr--last-used 1)))
        (setq relative-len (elt rotfr--last-used 2)
              from-end (elt rotfr--last-used 3))
      (let* ((old-len (- end-point start-point))
             (len-from-start (- (point) start-point))
             (len-from-end (- end-point (point) 1)))
        (if (<= len-from-start len-from-end)
            (setq relative-len (/ len-from-start (float old-len)))
          (setq from-end t
                relative-len (/ len-from-end (float old-len))))))
    (let* ((new-relative-pos (min (1- new-phrase-len)
                                  (floor (* (abs relative-len) new-phrase-len))))
           (new-point (if from-end
                          (- (+ start-point new-phrase-len) new-relative-pos 1)
                        (+ start-point new-relative-pos))))
      (setq rotfr--last-used (list start-point new-point relative-len from-end))
      new-point)))

;;;###autoload
(defun rotfr-rotate-this (&optional reversed)
  "Rotate the current word among selections.

Will rotate REVERSED when non nil."
  (interactive)
  (cl-flet ((frepl (start end new-word)
                   ;; Try to keep point around relative position
                   (let* ((new-point
                           (rotfr--maintain-relative-point
                            start end (length new-word))))
                     (delete-region start end)
                     (goto-char start)
                     (insert new-word)
                     (goto-char new-point))))
    (cl-loop for group in rotfr-rotation-sets
             for match = (rotfr--find-match-for-group group reversed)
             if match do (apply #'frepl match) and return t)))

;;;###autoload
(defun rotfr-rotate-this-reversed ()
  "Rotate the current word among selections in reverse order."
  (interactive)
  (rotfr-rotate-this t))

(provide 'rotfr)

;;; rotfr.el ends here
