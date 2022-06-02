;;; segment.el --- Regexes for sentence segmentation rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Marty Hiatt <martianhiatus AT riseup.net>
;; Author: Marty Hiatt <martianhiatus AT riseup.net>
;; Version: 0.1
;; URL: https://codeberg.org/martianh/segment
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages, convenience, translation, sentences, text, wp

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

;; It provides `segment-forward-sentence', `segment-backward-sentence', and
;; `segment-kill-sentence'.

;;; Code:

(require 'segment-convert)
(require 'segment-regexes)
(require 'xml)
(require 'dom)

(defgroup segment nil
  "Sentence segmentation regexes."
  :group 'editing)

(defcustom segment-ruleset-framework 'icu4j
  "Framework to use for break/non-break rules.
Can be either 'omegat, 'okapi-alt, 'icu4j, or 'all.
Used by `segment--build-rule-list'."
  :type 'symbol
  :group 'segment)

(defcustom segment-current-language "English"
  "The language the text is in."
  :group 'segment
  :type 'string)

;;; Converted files:
(defvar segment-directory "~/code/elisp/segment/")
(defvar segment-icu4j-file
  (concat segment-directory "segment-icu4j-rules-converted.el"))
(defvar segment-omegat-file
  (concat segment-directory "segment-omegat-rules-converted.el"))
(defvar segment-okapi-alt-file
  (concat segment-directory "segment-okapi-alt-rules-converted.el"))

(defun segment--get-lang-ruleset-from-file (language file)
  "Return ruleset for LANGUAGE from converted rulesets FILE.
Language is a string, like \"English\"."
  (cadr
   (segment-convert--get-ruleset-by-lang
    language
    (with-temp-buffer
      (insert-file-contents file)
      (read (current-buffer))))))

(defun segment--build-rule-list (&optional language)
  ""
  (append
   (cond ((equal segment-ruleset-framework 'omegat)
          (segment--get-lang-ruleset-from-file
           (or language segment-current-language)
           segment-omegat-file))
         ((equal segment-ruleset-framework 'icu4j)
          (segment--get-lang-ruleset-from-file
           (or language segment-current-language)
           segment-icu4j-file))
         ((equal segment-ruleset-framework 'okapi-alt)
          (segment--get-lang-ruleset-from-file
           (or language segment-current-language)
           segment-okapi-alt-file)))))
;; TODO: single point to fetch our own additional rules
;; (segment--get-additional-rules language)))

;; roll our own movement cmds:
;;;###autoload
(defun segment-forward-sentence (&optional arg)
  "Call `forward-sentence' ARG number of times.
Check if point matches any break rules for `segment-current-language',
and if it does, run `forward-sentence' again and check again."
  (interactive "p")
  (dotimes (count (or arg 1))
    (forward-sentence)
    (while
        (segment--looking-back-forward-map segment-current-language))
    (forward-sentence)))

;;;###autoload
(defun segment-backward-sentence (&optional arg)
  "Call `backward-sentence' ARG number of times.
Check if point matches any break rules for `segment-current-language',
and if it does, run `backward-sentence' again and check again."
  (interactive "p")
  (dotimes (count (or arg 1))
    (backward-sentence)
    (while
        (segment--looking-back-forward-map segment-current-language)
      :moving-backward)
    (backward-sentence)))

;;;###autoload
(defun segment-kill-sentence (&optional arg)
  "Kill forwards from point to end of sentence.
With ARG, kill that many more sentences."
  (interactive "p")
  (kill-region (point) (progn (segment-forward-sentence arg) (point))))

(defun segment--looking-back-forward-map (language &optional moving-backward)
  "Return non-nil if we are at a non-break rule for LANGUAGE.
MOVING-BACKWARD modifies the check for when we have moved backwards."
  (let ((case-fold-search nil)
        (regex-alist (segment--build-rule-list language)))
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
                   (looking-at (cadr reg-pair)))
                 ;; only when we hit nil break rules
                 (not (plist-get reg-pair :break)))
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
