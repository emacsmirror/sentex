;;; segment-convert.el --- Convert srx ICU regexes to elisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Marty Hiatt <martianhiatus AT riseup.net>
;; Author:Marty Hiatt <martianhiatus AT riseup.net>
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

;; Rough and ready conversion of ICU regexes into elisp.
;; The rules are ad-hoc, and only added as I come across them.

;;; Code:

(require 'dom)
(require 'xml)
(require 'segment)

;; conversion from ICU to emacs regex:
(defvar segment-convert-icu-regex-conversion-alist
  '(
    ;; ("\\." "\\\\.")
    ;; ("\\b" "\\\\b")
    ;; ("\b" "\\b")
    ("(" "\\(")
    ("\\s" "[[:space:]]")
    (")" "\\)")
    ("|" "\\|")
    ("\\d" "[[:digit:]]")
    ;; capitals are negations \p{Lu} is upper \P{Lu} is not upper
    ("\\p{Lu}" "[[:upper:]]")
    ("\\p{Ll}" "[[:lower:]]")
    ("\\P{Ll}" "[^[:lower:]]")
    ("\\P{Lu}" "[^[:upper:]]")
    ;; ("\\P{Lu}" "[[:lower:]]") ; wrong
    ;; (?i) == (case-fold-search t) ; FIXME how to do case fold search
    ("\\p{Ps}" "[[({]") ; any opening bracket
    ("\\p{pe}" "[])}]") ; any closing bracket
    ("\\p{L}" "[[:alpha:]]") ; any letter in any language
    ("\\p{N}" "[[:digit:]]")
    ;; FIXME: \p{Po}: any kind of punctuation character that is not a dash, bracket, quote or connector.
    ("\\p{Po}" "[^][(){}\"_-]")
    ;; ("{X,Y}" "\\{X,Y\\}") ; need match groups to convert this? or just:
    ("{" "\\{")
    ("}" "\\}")
    )
  "An (in-progress) alist of ICU regexe elements and their elisp equivalents.")

;; (defvar segment-convert-icu-regex-list nil)

(cl-defstruct (segment-convert-ruleset (:constructor segment-convert-ruleset-create))
  language-rule-name rules)

(cl-defstruct (segment-convert-rule (:constructor segment-convert-rule-create))
  break before-break after-break)

;;; parsing SRX files into cl-structs
(defun segment-convert--parse-xml-file (file)
  "Parse an XML FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (xml-parse-region (point-min) (point-max))))

(defun segment-convert--get-rulesets-from-file (srx-file)
  "Collect a list of rulesets from SRX-FILE."
  (let* ((parsed (segment-convert--parse-xml-file srx-file))
         (language-rules-tag
          (dom-by-tag (car parsed) 'languagerules))
         (language-rules-list
          (dom-by-tag (car language-rules-tag) 'languagerule)))
    (mapcar (lambda (x)
              (segment-convert--make-ruleset x))
            language-rules-list)))

(defun segment-convert--make-ruleset (ruleset)
  "Create a struct for RULESET."
  (segment-convert-ruleset-create
   :language-rule-name
   (dom-attr ruleset 'languagerulename)
   :rules (segment-convert--map-rules ruleset)))

(defun segment-convert--map-rules (dom)
  "Collect break rule, before-break and after-break regexes from DOM children."
  (mapcar (lambda (x)
            (segment-convert-rule-create
             :break (dom-attr (dom-by-tag x 'rule) 'break)
             :before-break (dom-text (dom-by-tag x 'beforebreak))
             :after-break (dom-text (dom-by-tag x 'afterbreak))))
          (dom-by-tag dom 'rule)))

;;; converting the regexes in our structs from ICU to elisp
(defun segment-convert--convert-srx-file-to-elisp (srx-file)
  "Convert SRX-FILE of segmentation rules to elisp regexes.
Conversion is done against `segment-convert-icu-regex-conversion-alist'."
  (mapcar (lambda (x)
            (segment-convert--convert-icu-ruleset-to-elisp x))
          (segment-convert--get-rulesets-from-file srx-file)))

(defun segment-convert--convert-icu-ruleset-to-elisp (ruleset)
  "Convert a single language RULESET to elisp regexes."
  (let ((rules (segment-convert-ruleset-rules ruleset)))
    (mapc (lambda (x)
            (setf (segment-convert-rule-before-break x)
                  (segment-convert--convert-icu-rule x))
            (setf (segment-convert-rule-after-break x)
                  (segment-convert--convert-icu-rule x :after)))
          rules)))

(defun segment-convert--convert-icu-rule (rule &optional after)
  "Convert single segmentation RULE to elisp regex.
By default, it is a before rule, with arg AFTER, it's an after one."
  (let ((rule-string (if after
                         (segment-convert-rule-after-break rule)
                       (segment-convert-rule-before-break rule))))
    (unless (equal "" rule-string)
      (with-temp-buffer
        ;; (with-current-buffer (get-buffer-create "test")
        ;; (switch-to-buffer (current-buffer))
        (erase-buffer)
        (insert rule-string)
        (mapc (lambda (x)
                (segment-convert--replace-icu-regex-in-string x))
              segment-convert-icu-regex-conversion-alist)
        (buffer-string)))))

(defun segment-convert--replace-icu-regex-in-string (regex-pair)
  "Replace a matching CAR from REGEX-PAIR with its CADR."
  (goto-char (point-min))
  (while (search-forward (car regex-pair) nil t)
    (replace-match (cadr regex-pair) nil t)))

(provide 'segment-convert)
;;; segment-convert.el ends here
