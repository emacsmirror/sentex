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

;; instead of structs, just build a keyword plist like:
;; '(:break nil :before-break x :after-break y)

(defvar segment-icu-omegat-regex-list
  "~/code/elisp/segment/OmegaT-defaults/defaultRules.srx")

(defvar segment-icu-okapi-regex-list
  "~/code/elisp/segment/Okapi-defaults/defaultSegmentation.srx")

(defvar segment-icu-okapi-alt-regex-list
  "~/code/elisp/segment/Okapi-defaults/alternate-default.srx")

(defvar segment-icu-icu4j-regex-list
  "~/code/elisp/segment/Okapi-defaults/okapi_default_icu4j.srx")

;; ICU regexes support look-aheads, Elisp doesn't:
;; look out for (?= ...), (?! ...), (?<= ...), (?<! ...)
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
    ;; ("(?i)" "") ; (case-fold-search t) ; elisp regexes ignore case by default
    ("\\(?i\\)" "") ; (case-fold-search t) ; elisp regexes ignore case by default
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
  "An (in-progress) alist of ICU regex elements and their elisp equivalents.")

(defvar segment-convert-icu-regex-conversion-alist-unicode-only
  '(
    ;; capitals are negations \p{Lu} is upper \P{Lu} is not upper
    ("\\p{Lu}" "[[:upper:]]")
    ("\\p{Ll}" "[[:lower:]]")
    ("\\P{Ll}" "[^[:lower:]]")
    ("\\P{Lu}" "[^[:upper:]]")
    ;; ("\\P{Lu}" "[[:lower:]]") ; wrong
    ;; ("(?i)" "") ; (case-fold-search t) ; elisp regexes ignore case by default
    ("\\(?i\\)" "") ; (case-fold-search t) ; elisp regexes ignore case by default
    ("\\p{Ps}" "[[({]") ; any opening bracket
    ("\\p{pe}" "[])}]") ; any closing bracket
    ("\\p{L}" "[[:alpha:]]") ; any letter in any language
    ("\\p{N}" "[[:digit:]]")
    ;; FIXME: \p{Po}: any kind of punctuation character that is not a dash, bracket, quote or connector.
    ("\\p{Po}" "[^][(){}\"_-]")
    ("\\p{Nd}" "[[:digit:]]")
    ;; manually handle space also:
    ("\\s" "[[:space:]]")))

(defvar segment-convert-converted-rulesets-file nil)

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
    (setq segment-convert-converted-rulesets-file
          (mapcar (lambda (x)
                    (segment-convert--make-ruleset x))
                  language-rules-list))))

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

(defun segment-convert--get-ruleset-languages (srx-file)
  "Return a list of the languages of the rulesets in SRX-FILE."
  (let ((segment-omegat-rulesets
         (segment-convert--get-rulesets-from-file srx-file)))
    (mapcar (lambda (ruleset)
              (segment-convert-ruleset-language-rule-name ruleset))
            segment-omegat-rulesets)))

;;; converting the regexes in our structs from ICU to elisp
(defun segment-convert--convert-srx-file-to-elisp (srx-file)
  "Convert SRX-FILE of segmentation rules to elisp regexes.
Conversion is done against `segment-convert-icu-regex-conversion-alist'."
  (mapcar (lambda (x)
            (segment-convert--convert-icu-ruleset-to-elisp x))
          (segment-convert--get-rulesets-from-file srx-file)))

(defun segment-convert--convert-icu-ruleset-to-elisp (ruleset)
  "Convert a single language RULESET to elisp regexes.
Updates the structs with the converted regex strings."
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

;; try to use `pcre2el' to convert (fails):
;;; converting the regexes in our structs from ICU to elisp
(defun segment-convert--convert-srx-file-to-elisp-pcre2el (srx-file)
  "Convert SRX-FILE of segmentation rules to elisp regexes."
  (mapcar (lambda (x)
            (list
             (segment-convert-ruleset-language-rule-name x)
             (segment-convert--convert-icu-ruleset-to-elisp-pcre2el x)))
          (segment-convert--get-rulesets-from-file srx-file)))

(defun segment-convert--convert-icu-ruleset-to-elisp-pcre2el (ruleset)
  "Convert a single language RULESET to elisp regexes.
Returns a nested list of the rules."
  (let ((rules (segment-convert-ruleset-rules ruleset)))
    (mapcar (lambda (x)
              (list
               (segment-convert--convert-icu-rule-pcre2el x)
               (segment-convert--convert-icu-rule-pcre2el x :after)
               :break
               (if (equal (segment-convert-rule-break x) "no")
                   nil
                 t)))
            rules)))

(defun segment-convert--convert-icu-rule-pcre2el (rule &optional after)
  "Convert single segmentation RULE to elisp regex.
By default, it is a before rule, with arg AFTER, it's an after one."
  (let ((rule-string (if after
                         (segment-convert-rule-after-break rule)
                       (segment-convert-rule-before-break rule))))
    (unless (equal "" rule-string)
      ;; first try our incomplete unicode properties conversion
      ;; which pcre2el can't handle:
      (with-temp-buffer
        ;; (with-current-buffer (get-buffer-create "test")
        ;; (switch-to-buffer (current-buffer))
        (erase-buffer)
        (insert rule-string)
        (mapc (lambda (x)
                (segment-convert--replace-icu-regex-in-string x))
              segment-convert-icu-regex-conversion-alist-unicode-only)
        (pcre-to-elisp
         (buffer-string))))))

(defun segment-convert--replace-icu-regex-in-string (regex-pair)
  "Replace a matching CAR from REGEX-PAIR with its CADR."
  (goto-char (point-min))
  (while (search-forward (car regex-pair) nil t)
    (replace-match (cadr regex-pair) nil t)))

;; build lists from conv struct:
;; map over all langs in segment-convert-converted-full-file-set
(defun segment-convert--build-regex-list-from-ruleset-struct (ruleset)
  "Return a rule list for each rule in RULESET.
A rule list consists in a before-break regex, an after-break
regex, and a :break flag of either nil or t."
  (mapcar (lambda (rule)
            (list (segment-convert-rule-before-break rule)
                  (segment-convert-rule-after-break rule)
                  :break (if (equal (segment-convert-rule-break rule) "no")
                             nil
                           t)))
          ruleset))

(defun segment-convert--get-ruleset-by-lang (language converted-file-set)
  "Get ruleset for LANGUAGE from CONVERTED-FILE-SET."
  (dolist (x converted-file-set)
    (when (equal language
                 (car x))
      (cl-return x))))


(provide 'segment-convert)
;;; segment-convert.el ends here
