;;; sentex.el --- Regex-based sentence navigation rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Marty Hiatt <martianhiatus AT riseup.net>
;; Author: Marty Hiatt <martianhiatus AT riseup.net>
;; Version: 0.2
;; URL: https://codeberg.org/martianh/sentex
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

;; It provides `sentex-forward-sentence', `sentex-backward-sentence', and
;; `sentex-kill-sentence'. They aim to act like the built-in functions, but to
;; intelligently ignore things like "e.g.", "i.e.", or "Mr." as ends of sentences.

;; Customize `sentex-ruleset-framework' to select which framework to use.
;; Call `sentex-set-language-for-buffer', or set `sentex-current-language'
;; to choose what language's rules to use. Different frameworks support
;; different languages, so if your language doesn't appear in the options, try
;; using a different one.

;;; Code:
(require 'xml)
(require 'dom)

(defgroup sentex nil
  "Sentence sentexation regexes."
  :group 'editing)

(defcustom sentex-ruleset-framework 'icu4j
  "Framework to use for break/non-break rulesets."
  :type '(choice (const :tag "ICU4J" icu4j)
                 (const :tag "OmegaT" omegat)
                 (const :tag "Okapi alternative" okapi-alt)))

(add-variable-watcher 'sentex-ruleset-framework 'sentex--update-current-ruleset)

(defcustom sentex-custom-rules-regex-list
  '(("English"
     ;; Sr. / Jr. can end a sentence
     ;; a single title addition from okapi above (otherwise we are using omegat)
     (("[JS]r\\."
       "[[:space:]][[:lower:]]" :break nil)

      ;; FIXME: this is wrong, but how to differentiate U.S. ending a sentence, and
      ;; U.S. followed by a proper noun ("the U.S. Army")?
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
\"(\"LANGUAGE\" ((\"BEFORE-BREAK-RE\" \"AFTER-BREAK-RE\" :break
BREAK-BOOLEAN))\". This is to match the converted rule lists, so
they can be easily combined."
  :group 'sentex
  :type '(alist :key-type (string :tag "Language")
                :value-type (list (alist :key-type
                                         (string :tag "before-break")
                                         :value-type
                                         (group
                                          (string :tag "after-break")
                                          (const :break)
                                          (boolean :tag "break-value"))))))

(defvar sentex-current-language "English"
  "The language for which the sentexation rules are to be used.
\nThis variable should be set by running
`sentex-set-current-language'. Note that different frameworks
support different languages.")

(add-variable-watcher 'sentex-current-language 'sentex--update-current-ruleset)

(defvar sentex-current-ruleset nil
  "The current ruleset to use.
\nA ruleset is a set of rules for a given language as specified
by a framework.
\nThis is created by `sentex--build-rule-list'.")

(defvar sentex-current-break-rules nil)

;;; Converted files:
(defvar sentex-directory
  (file-name-directory (locate-library "sentex")))
(defvar sentex-icu4j-file
  (concat sentex-directory "sentex-icu4j-rules-converted.eld"))
(defvar sentex-omegat-file
  (concat sentex-directory "sentex-omegat-rules-converted.eld"))
(defvar sentex-okapi-alt-file
  (concat sentex-directory "sentex-okapi-alt-rules-converted.eld"))

;;; getting and setting language:
(defun sentex--read-file (file)
  "Read FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun sentex--get-langs-from-file (file)
  "Return the languages with rulesets in FILE."
  (let ((rulesets (sentex--read-file file)))
    (mapcar (lambda (x)
              (car x))
            rulesets)))

(defun sentex--current-framework-file ()
  "Return the current ruleset file given `sentex-ruleset-framework'."
  (cond ((equal sentex-ruleset-framework 'omegat)
         sentex-omegat-file)
        ((equal sentex-ruleset-framework 'icu4j)
         sentex-icu4j-file)
        ((equal sentex-ruleset-framework 'okapi-alt)
         sentex-okapi-alt-file)))

(defun sentex-get-valid-langs ()
  "Return the list of languages supported by `sentex-ruleset-framework'."
  (interactive)
  (sentex--get-langs-from-file (sentex--current-framework-file)))

(defun sentex-set-current-language (&optional local)
  "Set the language ruleset to use, using completion.
With arg LOCAL, make the setting buffer-local.
\nNote that different frameworks support different languages, so if
your desired language does not appear, customize
`sentex-ruleset-framework' and try again.
\nRuns `sentex--build-rule-list' which sets `sentex-current-ruleset'."
  (interactive)
  (let* ((langs (sentex-get-valid-langs))
         (lang-choice
          (completing-read
           (format "Set sentex.el language for current buffer (%s): "
                   sentex-ruleset-framework)
           langs
           nil t)))
    (if local
        (setq-local sentex-current-language lang-choice)
      (setq sentex-current-language lang-choice))
    (sentex--build-rule-list)
    (message "Using %s %s rules for current buffer."
             sentex-ruleset-framework
             lang-choice)))

;;; building current ruleset:
(defun sentex--get-ruleset-by-lang (language converted-file-set)
  "Get ruleset for LANGUAGE from CONVERTED-FILE-SET."
  (cl-dolist (x converted-file-set)
    (when (equal language
                 (car x))
      (cl-return x))))

(defun sentex--get-lang-ruleset-from-file (language file)
  "Return ruleset for LANGUAGE from converted rulesets FILE.
Language is a string, like \"English\"."
  (cadr
   (sentex--get-ruleset-by-lang
    language
    (sentex--read-file file))))

(defun sentex--get-custom-rules (language)
  "Return the set of custom rules for LANGUAGE."
  (cadr
   (sentex--get-ruleset-by-lang language sentex-custom-rules-regex-list)))

(defun sentex--build-rule-list (&optional language)
  "Build ruleset list for LANGUAGE.
Add any additional rules to the converted rulesets."
  (setq sentex-current-ruleset
        (append
         (reverse (sentex--get-lang-ruleset-from-file
                   (or language sentex-current-language)
                   (sentex--current-framework-file)))
         (sentex--get-custom-rules (or language sentex-current-language))))
  (setq sentex-current-break-rules
        (sentex--get-breaking-rules sentex-current-ruleset)))

(defun sentex--update-current-ruleset (symbol newval _operation _where)
  "Update `sentex-current-ruleset' as needed.
\nThis is a variable watcher function for
`sentex-ruleset-framework'.
Args SYMBOL NEWVAL OPERATION and WHERE are required by it."
  (set-default symbol newval)
  (sentex--build-rule-list))

;;; roll our own movement cmds:
;;;###autoload
(defun sentex-forward-sentence (&optional arg)
  "Call `forward-sentence' ARG number of times.
Check if point matches any break rules for `sentex-current-language',
and if it does, run `forward-sentence' again and check again."
  (interactive "p")
  (dotimes (_count (or arg 1))
    (forward-sentence)
    (while
        (sentex--looking-back-forward-map sentex-current-language)
      (forward-sentence))))

;;;###autoload
(defun sentex-backward-sentence (&optional arg)
  "Call `backward-sentence' ARG number of times.
Check if point matches any break rules for `sentex-current-language',
and if it does, run `backward-sentence' again and check again."
  (interactive "p")
  (dotimes (_count (or arg 1))
    (backward-sentence)
    (while
        (sentex--looking-back-forward-map sentex-current-language
                                          :moving-backward)
      (backward-sentence))))

;;;###autoload
(defun sentex-kill-sentence (&optional arg)
  "Kill forwards from point to end of sentence.
With ARG, kill that many more sentences."
  (interactive "p")
  (kill-region (point) (progn (sentex-forward-sentence arg) (point))))

;; FIXME: language arg redundant? break-rules redundant? everything redundant?
(defun sentex--looking-back-forward-map (&optional _language moving-backward)
  "Return non-nil if we are at a non-break rule for LANGUAGE.
MOVING-BACKWARD modifies the check for when we have moved backwards."
  (let* ((case-fold-search nil)
         (regex-alist sentex-current-ruleset))
    ;; (break-rules sentex-current-break-rules))
    ;; break rules first?:
    ;; only check for non-break rules if no break rule matched:
    ;; (unless
    ;; (sentex--test-rule-pairs break-rules moving-backward)
    (sentex--test-rule-pairs regex-alist moving-backward)))
;; :non-break)))

(defun sentex--get-breaking-rules (regex-alist)
  "Collect the rules that mandate sentence breaks in REGEX-ALIST."
  (remove nil
          (mapcar (lambda (x)
                    (when (plist-get x :break)
                      x))
                  regex-alist)))

(defun sentex--test-rule-pairs (regex-alist &optional moving-backward)
  "Return non-nil when when point is surrounded by an element in REGEX-ALIST.
MOVING-BACKWARD makes adjustments based on where
`backward-sentence' places point."
  (cl-dolist (reg-pair regex-alist)
    (when (and
           ;; FIXME: performance of this is terrible
           (looking-back
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
      (cl-return reg-pair))))

(defvar sentex-mode-map
  (let ((keymap (make-keymap)))
    (define-key keymap [remap forward-sentence] #'sentex-forward-sentence)
    (define-key keymap [remap backward-sentence] #'sentex-backward-sentence)
    (define-key keymap [remap kill-sentence] #'sentex-kill-sentence)
    keymap)
  "Keymap for `sentex-minor-mode'.")

;;;###autoload
(define-minor-mode sentex-minor-mode
  "Locally remap sentence navigation commands to use their `sentex' counterparts."
  :lighter " Sentex" :keymap sentex-mode-map)

(provide 'sentex)
;;; sentex.el ends here
