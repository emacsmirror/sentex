;;; segment.el --- regexes for sentence segmentation rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  marty hiatt

;; Author: marty hiatt <martianhiatus [A T] riseup [D o t] net>
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

;; This code just parses the srx rule files.
;; ported from OmegaT and Okapi Framework

;;; Code:

(require 'segment-regexes)
(require 'sentence-navigation)

(require 'xml)
(require 'dom)

(defgroup segment nil
  "Sentence segmentation regexes."
  :group 'editing)

(cl-defstruct (segment-rules-english (:constructor segment-rules-english-create))
  language-rule-name rules)

(cl-defstruct (segment-rule-english (:constructor segment-rule-english-create))
  break before-break after-break)

;;; parsing SRX files
(defun segment-parse-xml-file (file)
  "Parse an XML FILE."
  ;; to use this we need to adapt our extraction functions:
  ;; (nxml-parse-file transcat-project-tmx-file))
  (with-temp-buffer
    (insert-file-contents file)
    (xml-parse-region (point-min) (point-max))))

(defun segment-get-rules-en (srx-file)
  ""
  (let ((en-parsed (segment-parse-xml-file srx-file)))
    (segment-rules-english-create
     :language-rule-name (dom-attr en-parsed 'languagerulename)
     :rules (segment-map-rules en-parsed))))

(defun segment-map-rules (dom)
  ""
  (mapcar (lambda (x)
            (segment-rule-english-create
             :break (dom-attr (dom-by-tag x 'rule) 'break)
             :before-break (dom-text (dom-by-tag x 'beforebreak))
             :after-break (dom-text (dom-by-tag x 'afterbreak))))
          (segment-get-children dom)))

;; TODO: fix this shit
(defun segment-get-children (dom)
  ""
  (remove "\n"
          (remove "\n    "
                  (remove "\n  "
                          (dom-children dom)))))

(defun segment-get-before-break-rules-for-sentence-nav (regex-list)
  "Get all before break rules from REGEX-LIST."
  (mapcar (lambda (x)
            (segment-strip-trailing-period
             (car x)))
          regex-list))

(defun segment-strip-trailing-period (regex)
  "Strip trailing slashes and period from string REGEX."
  (save-match-data
    (when (string-match ".\\(?2:\\\\\\.\\)$" regex)
      (replace-match "" t nil regex 2))))

;; modify sentence-nav functions:

;;;###autoload
(defun segment-sentence-nav-forward (&optional arg)
  "Move to the start of the next sentence ARG times.
Add `segment.el' rules to `sentence-nav-abbreviation-list' beforehand."
  (interactive "p")
  (let ((sentence-nav-abbreviation-list
         (append (segment-get-before-break-rules-for-sentence-nav
                  segment-regexes-en-alist)
                 sentence-nav-abbreviation-list)))
    (sentence-nav-forward arg)))

;;;###autoload
(defun segment-sentence-nav-backward (&optional arg)
  "Move to the start of the previous sentence ARG times.
Add `segment.el' rules to `sentence-nav-abbreviation-list' beforehand."
  (interactive "p")
  (let ((sentence-nav-abbreviation-list
         (append (segment-get-before-break-rules-for-sentence-nav
                  segment-regexes-en-alist)
                 sentence-nav-abbreviation-list)))
    (sentence-nav-backward arg)))

;;;###autoload
(defun segment-sentence-nav-forward-end (&optional arg)
  "Move to the end of the next sentence end ARG times.
Add `segment.el' rules to `sentence-nav-abbreviation-list' beforehand."
  (interactive "p")
  (let ((sentence-nav-abbreviation-list
         (append (segment-get-before-break-rules-for-sentence-nav
                  segment-regexes-en-alist)
                 sentence-nav-abbreviation-list)))
    (sentence-nav-forward-end arg)))

;;;###autoload
(defun sentence-nav-backward-end (&optional arg)
  "Move to the end of the previous sentence end ARG times.
Add `segment.el' rules to `sentence-nav-abbreviation-list' beforehand."
  (interactive "p")
  (let ((sentence-nav-abbreviation-list
         (append (segment-get-before-break-rules-for-sentence-nav
                  segment-regexes-en-alist)
                 sentence-nav-abbreviation-list)))
    (sentence-nav-backward-end arg)))

(provide 'segment)
;;; segment.el ends here
