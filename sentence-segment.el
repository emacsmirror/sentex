;;; sentence-segment.el --- regexes for sentence segmentation rules  -*- lexical-binding: t; -*-

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

(require 'xml)
(require 'dom)

(cl-defstruct (sentence-segment-rules-english (:constructor sentence-segment-rules-english-create))
  language-rule-name rules)

(cl-defstruct (sentence-segment-rule-english (:constructor sentence-segment-rule-english-create))
  break before-break after-break)

;;; parsing files
(defun sentence-segment-parse-xml-file (file)
  "Parse an XML FILE."
  ;; to use this we need to adapt our extraction functions:
  ;; (nxml-parse-file transcat-project-tmx-file))
  (with-temp-buffer
    (insert-file-contents file)
    (xml-parse-region (point-min) (point-max))))

(setq sent-seg-xml-rules
      (sentence-segment-parse-xml-file
       "/home/mouse/code/elisp/sentence-segmenter/OmegaT-defaults/en-only.srx"))

(defun sentence-segment-get-rules-en ()
  ""
  (let ((en-parsed
         (sentence-segment-parse-xml-file
          "/home/mouse/code/elisp/sentence-segmenter/OmegaT-defaults/en-only.srx")))
    (sentence-segment-rules-english-create
     :language-rule-name (dom-attr en-parsed 'languagerulename)
     :rules (sentence-segment-map-rules en-parsed))))

(defun sentence-segment-map-rules (dom)
  ""
  (mapcar (lambda (x)
            (sentence-segment-rule-english-create
             :break (dom-attr (dom-by-tag x 'rule) 'break)
             :before-break (dom-text (dom-by-tag x 'beforebreak))
             :after-break (dom-text (dom-by-tag x 'afterbreak))))
          (sentence-segment-get-children dom)))

(defun sentence-segment-get-children (dom)
  ""
  (remove "\n"
          (remove "\n    "
                  (remove "\n  "
                          (dom-children dom)))))

(sentence-segment-get-rules-en)

(dom-attr sent-seg-xml-rules 'languagerulename)
(dom-attr sent-seg-xml-rules 'languagerulename)

(sentence-segment-map-rules (dom-children sent-seg-xml-rules))

(dom-attr (dom-by-tag sent-seg-xml-rules 'rule) 'break)
(sentence-segment-get-rules-en)

(provide 'sentence-segment)
;;; sentence-segment.el ends here
