;;; segment.el --- Regexes for sentence segmentation rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Marty Hiatt <martianhiatus AT riseup.net>
;; Author: Marty Hiatt <martianhiatus AT riseup.net>
;; Version: 0.2
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

;; Customize `segment-ruleset-framework' to select which framework to use.
;; Set `segment-current-language' to choose what language's rules to use.

;;; Code:

(require 'segment-convert)
(require 'segment-regexes)
(require 'xml)
(require 'dom)

(defgroup segment nil
  "Sentence segmentation regexes."
  :group 'editing)

(defcustom segment-ruleset-framework 'icu4j
  "Framework to use for break/non-break rulesets."
  :type '(choice (const :tag "ICU4J" icu4j)
                 (const :tag "OmegaT" omegat)
                 (const :tag "Okapi alternative" okapi-alt)))

(defcustom segment-current-language "English"
  "The language for which the segmentation rules are to be used.
This can be changed on a per-buffer basis by calling
`segment-set-language-for-buffer'. Note that different frameworks
support different languages. Run `segment-get-valid-langs' to see
what languages the current framework supports."
  :group 'segment
  :type 'string)

(defcustom segment-custom-rules-list
  '(("English"
    ;;; segment.el additions

     ;; Sr. / Jr. can end a sentence
     ;; a single title addition from okapi above (otherwise we are using omegat)
     (("[JS]r\\."
       "[[:space:]][[:lower:]]" :break nil)

      ;; this is wrong, but how to differentiate U.S. ending a sentence, and
      ;; U.S. followed by a proper noun?
      ;; omegat's U.K. rule only mandates a space, which is also imperfect
      ("U\\.S\\."
       "[[:space:]][[:lower:]]" :break nil)

      ;; chars + period + closing bracket followed by space + lower char
      ("[[:lower:]]+\\.[])}]"
       "[[:space:]][[:lower:]]" :break nil)

      ;; Name initials can be preceded by opening quotation mark
      ;; and space should be an after break
      ;; fix for okapi regex above
      ("[^\\.][[:space:]][\"“]?[A-Z]\\."
       "[[:space:]]" :break nil)

      ;; opus abbrev:
      ("[Oo]p\\."
       "[[:space:]][[:digit:]]" :break nil))))
  "Custom regexes of before break / after break rules.
These additional rules are added to the converted rulesets.
\nCustom rules are grouped by language, and take the format
\"(\"Language\" ((\"before-break-re\" \"after-break-re\" :break
BREAK-BOOLEAN))\"."
  :group 'segment-regexes
  :type 'alist)

;;; Converted files:
(defvar segment-directory
  (file-name-directory (locate-library "segment")))
(defvar segment-icu4j-file
  (concat segment-directory "segment-icu4j-rules-converted.el"))
(defvar segment-omegat-file
  (concat segment-directory "segment-omegat-rules-converted.el"))
(defvar segment-okapi-alt-file
  (concat segment-directory "segment-okapi-alt-rules-converted.el"))

(defun segment-get-valid-langs ()
  "Return the list of languages supported by `segment-current-language'."
  (interactive)
  (segment--get-langs-from-file (segment--current-framework-file)))

(defun segment--current-framework-file ()
  "Return the current ruleset file given `segment-ruleset-framework'."
  (cond ((equal segment-ruleset-framework 'omegat)
         segment-omegat-file)
        ((equal segment-ruleset-framework 'icu4j)
         segment-icu4j-file)
        ((equal segment-ruleset-framework 'okapi-alt)
         segment-okapi-alt-file)))

(defun segment-set-language-for-buffer ()
  "Set the language ruleset to use for current buffer, using completion.
Note that different frameworks support different languages, so if
your desired language does not appear, customize
`segment-ruleset-framework' and try again."
  (interactive)
  (let* ((langs
          (segment--get-langs-from-file (segment--current-framework-file)))
         (lang-choice
          (completing-read
           (format "Set segment.el language for current buffer (%s): "
                   segment-ruleset-framework)
           langs
           nil t
           segment-current-language)))
    (setq-local segment-current-language lang-choice)
    (message "Using %s rules for current buffer." lang-choice)))

(defun segment--read-file (file)
  "Read FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun segment--get-langs-from-file (file)
  "Return the languages with rulesets in FILE."
  (let ((rulesets (segment--read-file file)))
    (mapcar (lambda (x)
              (car x))
            rulesets)))

(defun segment--get-ruleset-by-lang (language converted-file-set)
  "Get ruleset for LANGUAGE from CONVERTED-FILE-SET."
  (dolist (x converted-file-set)
    (when (equal language
                 (car x))
      (cl-return x))))

(defun segment--get-lang-ruleset-from-file (language file)
  "Return ruleset for LANGUAGE from converted rulesets FILE.
Language is a string, like \"English\"."
  (cadr
   (segment--get-ruleset-by-lang
    language
    (segment--read-file file))))

(defun segment--get-custom-rules (language)
  "Return the set of custom rules for LANGUAGE."
  (cadr
   (segment--get-ruleset-by-lang language segment-custom-rules-list)))

(defun segment--build-rule-list (&optional language)
  "Build ruleset list for LANGUAGE.
Add any additional rules to the converted rulesets."
  (append
   (segment--get-lang-ruleset-from-file
    (or language segment-current-language)
    (segment--current-framework-file))
   (segment--get-custom-rules language)))

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
        (segment--looking-back-forward-map segment-current-language)
      (forward-sentence))))

;;;###autoload
(defun segment-backward-sentence (&optional arg)
  "Call `backward-sentence' ARG number of times.
Check if point matches any break rules for `segment-current-language',
and if it does, run `backward-sentence' again and check again."
  (interactive "p")
  (dotimes (count (or arg 1))
    (backward-sentence)
    (while
        (segment--looking-back-forward-map segment-current-language
                                           :moving-backward)
      (backward-sentence))))

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


(provide 'segment)
;;; segment.el ends here
