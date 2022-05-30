;;; segment.el --- Regexes for sentence segmentation rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Marty Hiatt <martianhiatus AT riseup.net>
;; Author: Marty Hiatt <martianhiatus AT riseup.net>
;; Version: 0.1
;; URL: https://codeberg.org/martianh/segment
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages, convenience, translation, sentences, text

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package uses breaking and non-breaking sentence-ending rules ported
;; from OmegaT and Okapi Framework.

;; It's very green. for now only some English rules are ported, and it only
;; provides `segment-forward-sentence' and `segment-backward-sentence'
;; commands for movement that respects the rules.

;;; Code:

(require 'segment-regexes)
(require 'segment-convert)
;; (require 'sentence-navigation)

(require 'xml)
(require 'dom)

(defgroup segment nil
  "Sentence segmentation regexes."
  :group 'editing)

(defcustom segment-ruleset-framework 'omegat
  "Framework to use for break/non-break rules.
Can be either 'omegat, 'okapi, 'okapi-icu4j, or 'all.
Used by `segment-regexes--construct-en-list'."
  :type 'symbol
  :group 'segment)

;; roll our own movement cmds:

;;;###autoload
(defun segment-forward-sentence (&optional arg)
  "Call `forward-sentence' ARG number of times.
Check if we are after any entries in `segment-regexes-en-list',
and if we are, run `forward-sentence' again and check again."
  (interactive "p")
  (dotimes (count (or arg 1))
    (forward-sentence)
    (while
        (segment--looking-back-forward-map
         (segment-regexes--construct-en-list))
      (forward-sentence))))

;;;###autoload
(defun segment-backward-sentence (&optional arg)
  "Call `backward-sentence' ARG number of times.
Check if we are after any entries in `segment-regexes-en-list',
and if we are, run `backward-sentence' again and check again."
  (interactive "p")
  (dotimes (count (or arg 1))
    (backward-sentence)
    (while
        (segment--looking-back-forward-map
         (segment-regexes--construct-en-list)
         :moving-backward)
      (backward-sentence))))

(defun segment-kill-sentence (&optional arg)
  "Kill forwards from point to end of sentence.
With ARG, kill that many more sentences."
  (interactive "p")
  (kill-region (point) (progn (segment-forward-sentence arg) (point))))

(defun segment--looking-back-forward-map (regex-alist &optional moving-backward)
  "Return non-nil if we are at any of the segment rules in REGEX-ALIST.
MOVING-BACKWARD modifies the check for when we have moved backwards."
  (let ((case-fold-search nil))
    (cl-dolist (reg-pair regex-alist)
      (when (and (looking-back
                  ;; before-break regex
                  (if moving-backward
                      ;; we are after any whitespace
                      (concat (car reg-pair)
                              "[[:blank:]]*")
                    (car reg-pair))
                  ;; limit arg:
                  (save-excursion (backward-word 2)
                                  (point)))
                 ;; after-break regex:
                 (if moving-backward
                     ;; we are after any whitespace:
                     (save-excursion
                       (forward-whitespace -1) ; back over whitespace
                       (looking-at (cadr reg-pair)))
                   (looking-at (cadr reg-pair))))
        (cl-return reg-pair)))))

;; modify sentence-nav functions:
(defun segment--get-before-break-rules-for-sentence-nav (regex-list)
  "Get all before break rules from REGEX-LIST."
  (mapcar (lambda (x)
            (segment--strip-trailing-period
             (car x)))
          regex-list))

(defun segment--strip-trailing-period (regex)
  "Strip trailing slashes and period from string REGEX."
  (save-match-data
    (when (string-match ".\\(?2:\\\\\\.\\)$" regex)
      (replace-match "" t nil regex 2))))

;;;###autoload
(defun segment-sentence-nav-forward (&optional arg)
  "Move to the start of the next sentence ARG times.
Add `segment.el' rules to `sentence-nav-abbreviation-list' beforehand."
  (interactive "p")
  (let ((sentence-nav-abbreviation-list
         (append (segment--get-before-break-rules-for-sentence-nav
                  (segment-regexes--construct-en-list))
                 sentence-nav-abbreviation-list)))
    (sentence-nav-forward arg)))

;;;###autoload
(defun segment-sentence-nav-backward (&optional arg)
  "Move to the start of the previous sentence ARG times.
Add `segment.el' rules to `sentence-nav-abbreviation-list' beforehand."
  (interactive "p")
  (let ((sentence-nav-abbreviation-list
         (append (segment--get-before-break-rules-for-sentence-nav
                  (segment-regexes--construct-en-list))
                 sentence-nav-abbreviation-list)))
    (sentence-nav-backward arg)))

;;;###autoload
(defun segment-sentence-nav-forward-end (&optional arg)
  "Move to the end of the next sentence end ARG times.
Add `segment.el' rules to `sentence-nav-abbreviation-list' beforehand."
  (interactive "p")
  (let ((sentence-nav-abbreviation-list
         (append (segment--get-before-break-rules-for-sentence-nav
                  (segment-regexes--construct-en-list))
                 sentence-nav-abbreviation-list)))
    (sentence-nav-forward-end arg)))

;;;###autoload
(defun segment-sentence-nav-backward-end (&optional arg)
  "Move to the end of the previous sentence end ARG times.
Add `segment.el' rules to `sentence-nav-abbreviation-list' beforehand."
  (interactive "p")
  (let ((sentence-nav-abbreviation-list
         (append (segment--get-before-break-rules-for-sentence-nav
                  (segment-regexes--construct-en-list))
                 sentence-nav-abbreviation-list)))
    (sentence-nav-backward-end arg)))

(provide 'segment)
;;; segment.el ends here
