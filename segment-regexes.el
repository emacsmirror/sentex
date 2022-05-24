;;; segment-regexes.el --- Regexes for sentence segmentation rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Marty Hiatt <martianhiatus AT riseup.net>
;; Author: Marty Hiatt <martianhiatus AT riseup.net>
;; Version: 0.1
;; URL: https://codeberg.org/martianh/segment
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages, convenience, translation, sentences, text

;;; License:

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

;; Regexes for complex sentence segmentation rules.
;; Roughly ported from OmegaT and Okapi Framework default/English srx rules.

;; each rule has two regexes, before and after, to facilitate sophisticated
;; conditional breaks, such as break only if regex X is followed by regex Y.

;;; Code:

;; TODO: best to keep the structure of the separate files, and let the user
;; choose which rules to use

;; TODO: okapi_default_icu4j.srx (looks more comprehensive than the other okapi)

;; NB: after-break rules must always be provided

(defvar segment-regexes-en-list nil
  "Holds the composite list of en regexes.
\nRun `segment-regexes-construct-en-list' to obtain the overall value.")

(defconst segment-regexes-omegat-en-list
  '(
    ;; Omega defaultRules.srx (English):
    
    ("etc\\."
     "[[:space:]]+[[:lower:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>etc\.</beforebreak>
    ;; <afterbreak>\s+\P{Lu}</afterbreak>
    ;; </rule>

    ("Dr\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>Dr\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("U\\.K\\."
     "[[:space:]][[:lower:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>U\.K\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("M\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>M\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("Mr\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>Mr\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("Mrs\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>Mrs\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("Ms\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>Ms\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("Prof\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>Prof\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[Ee]\\.g\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>(?i :break nil)e\.g\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[Ii]\\.e\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>(?i)i\.e\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("resp\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>resp\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("stel\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\stel\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[Ff]ig\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>(?i)fig\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("St\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>St\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[[:space:]][A-Z]\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\s[A-Z]\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[apAP]\\.?[mM]\\."
     "[[:space:]][a-z]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>[apAP]\.?[mM]\.</beforebreak>
    ;; <afterbreak>\s[a-z]</afterbreak>
    ;; </rule>

    ("Mt\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>Mt\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("No\\."
     "[[:space:]][[:digit:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>No\.</beforebreak>
    ;; <afterbreak>\s\d</afterbreak>
    ;; </rule>

    ("[Aa]pprox\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>[Aa]pprox\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[[:digit:][:space:]]mi?n\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\d\smi?n\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[[:digit:][:space:]]sec\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\d\ssec\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[[:space:]][Vv][sS]?\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\s[vV][sS]?\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>
    ;; </languagerule>
    ))

(defconst segment-regexes-okapi-en-list
  '(
;;; Okapi defaultSegmentation.srx
    ;; default lang
    
    ("\\b\\(pp?\\|\"e\\.?[[:space:]]*g\\|i\\.?[[:space:]]*e\\|no\\|[Vv]ol\\|[Rr]col\\|maj\\|Lt\\|[Ff]ig\\|[Ff]igs\\|[Vv]iz\\|[Vv]ols\\|[Aa]pprox\\|[Ii]ncl\\|Pres\\|Prof\\|[Dd]ept\\|min\\|max\\|[Gg]ovt\\|c\\.?[[:space:]]*f\\|[Vv]\\.?[[space:]]\\)\\."
     "[[:space:]]+[^[:upper:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\b(pp|e\.?\s*g|i\.?\s*e|no|[Vv]ol|[Rr]col|maj|Lt|[Ff]ig|[Ff]igs|[Vv]iz|[Vv]ols|[Aa]pprox|[Ii]ncl|Pres|Prof|[Dd]ept|min|max|[Gg]ovt|c\.?\s*f|vs)\.</beforebreak>
    ;; <afterbreak>\s+[^\p{Lu}]</afterbreak>

    ;; months followed by a digit
    ("\\([Jj]an\\|[Ff]eb\\|[Mm]ar\\|[Aa]pr\\|[Jj]un\\|[Jj]ul\\|[Aa]ug\\|[Ss]ep\\|[Oo]ct\\|[Nn]ov\\|[Dd]ec\\|[Ee]st\\|[Tt]el\\|[Pp]h\\)\\."
     "[[:space:]+[:digit:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>((?i)jan|feb|mar|apr|jun|jul|aug|sep|oct|nov|dec|est|tel)\.</beforebreak>
    ;; <afterbreak>\s+\d</afterbreak>
    ;; </rule>

    ;; titles, followed by uppercase
    ("\\b\\(St\\|Gen\\|Hon\\|Dr\\|Mr\\|Ms\\|Mrs\\|Col\\|Maj\\|Brig\\|Sgt\\|Capt\\|Cmnd\\|Sen\\|Rev\\|Rep\\|Revd\\)\\."
     "[[:space:]][[:upper:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\b(St|Gen|Hon|Dr|Mr|Ms|Mrs|Col|Maj|Brig|Sgt|Capt|Cmnd|Sen|Rev|Rep|Revd)\.</beforebreak>
    ;; <afterbreak>\s+\p{Lu}</afterbreak>

;;; Okapi alternate-default.srx
    ;; English only

    ;; basics, done above i think:

    ;; <languagerule languagerulename="English">
    ;; <rule break="no">
    ;; <beforebreak>\b[nN]o\.\s</beforebreak>
    ;; <afterbreak>\p{N}</afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\b(pp|[Vv]iz|i\.?\s*e|[Vvol]|[Rr]col|maj|Lt|[Ff]ig|[Ff]igs|[Vv]iz|[Vv]ols|[Aa]pprox|[Ii]ncl|Pres|[Dd]ept|min|max|[Gg]ovt|lb|ft|c\.?\s*f|vs)\.\s</beforebreak>
    ;; <afterbreak>[^\p{Lu}]|I</afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\b[Ff]igs?\.\s</beforebreak>
    ;; <afterbreak>\p{N}</afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\be\.g\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\bvs\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\besp\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <!--"Etc." can end the sentence, so we check for the uppercase letter after it.-->
    ;; <rule break="no">
    ;; <beforebreak>\b[Ee]tc\.\s</beforebreak>
    ;; <afterbreak>[^p{Lu}]</afterbreak>
    ;; </rule>

    ;; months (done in omegat above)

    ;; <rule break="no">
    ;; <beforebreak>\bJan\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\bFeb\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\bMar\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\bApr\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\bJun\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\bJul\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\bAug\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\bSept?\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\bOct\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\bNov\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\bDec\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ("[[:blank:]]Ph\\.?D\\.[[:blank:]]"
     "" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\bPh\.?D\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">

    ;; et al.
    ("\\bet\\b[[:space:]]al\\.[[:space:]]" "" :break nil)
    ;; <beforebreak>\bet\b\s\bal\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ;; titles (done above but are there any diff ones)

    ;; <rule break="no">
    ;; <beforebreak>\b(St|Gen|Hon|Prof|Dr|Mr|Ms|Mrs|[JS]r|Col|Maj|Brig|Sgt|Capt|Cmnd|Sen|Rev|Rep|Revd)\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\b(St|Gen|Hon|Prof|Dr|Mr|Ms|Mrs|[JS]r|Col|Maj|Brig|Sgt|Capt|Cmnd|Sen|Rev|Rep|Revd)\.\s[A-Z]\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ;; cf. already done:
    ;; <rule break="no">
    ;; <beforebreak>\bcf\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">

    ("\\bInc\\.[[:space:]]" "" :break nil)
    ;; <beforebreak>\bInc\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ("\\bCorp\\.[[:space:]]" "" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\bCorp\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ("\\bBros\\.[[:space:]]" "" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\bBros\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ("\\bDist\\.[[:space:]]" "" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\bDist\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ("\\bCo\\.[[:space:]]" "" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\bCo\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ;; ellipsis, poss surrounded by [] or ():
    ("[[(]*…[])]*"
     "[[:lower:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>[\[\(]*…[\]\)]* </beforebreak>
    ;; <afterbreak>\p{Ll}</afterbreak>
    ;; </rule>

    ;; any opening bracket + ! and/or ? + any closing bracket
    ("[[({][!?]+[])}]"
     "" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\p{Ps}[!?]+\p{Pe} </beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ("[\\.!?…]+[])}]"
     "[[:space:]][[:lower:]]" :break nil) ; edit: mandate a space first
    ;; <rule break="no">
    ;; <beforebreak>[\.!?…]+\p{Pe} </beforebreak>
    ;; <afterbreak>\p{Ll}</afterbreak>
    ;; </rule>

    ("\"”'[[:space:]]*"
     "[[:space:]]*[[:lower:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>[\"”']\s*</beforebreak>
    ;; <afterbreak>\s*\p{Ll}</afterbreak>
    ;; </rule>

    ;; what the hell's this shit
    ("'\"„][\\.!?…]['\"”][[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>['"„][\.!?…]['"”]\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ;; if character + period followed by character + SPC + period
    ("\\b[[:alpha:]]\\.[[:space:]]"
     "[[:alpha:]]\\.[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\b\p{L}\.\s</beforebreak>
    ;; <afterbreak>\p{L}\.\s</afterbreak>
    ;; </rule>

    ;; if character + period followed by character + period
    ("\\b[[:alpha:]]\\."
     "[[:alpha:]]\\." :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\b\p{L}\.</beforebreak>
    ;; <afterbreak>\p{L}\.</afterbreak>
    ;; </rule>

    ("[\\.[[:space:]]][[:alpha:]]\\{1,2\\}\\.[[:space:]]"
     "[[:digit:][:lower:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>[\.\s]\p{L}{1,2}\.\s</beforebreak>
    ;; <afterbreak>[\p{N}\p{Ll}]</afterbreak>
    ;; </rule>

    ;; three stops as ellipsis, poss surrounded in [] or ()
    ("[[(]*\\.\\.\\.[])]*"
     "[[:lower:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>[\[\(]*\.\.\.[\]\)]* </beforebreak>
    ;; <afterbreak>[^\p{Lu}]</afterbreak>
    ;; </rule>

    ("\\b[[:lower:]]\\.[[:space:]][[:lower:]]\\.[[:space:]]"
     "" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\b\p{Lu}\.\s\p{Lu}\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ("\\b[[:lower:]]\\.[[:lower:]]\\.[[:space:]]"
     "" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\b\p{Lu}\.\p{Lu}\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ;; Name initials (capital not preceded by period):
    ("[^\\.][[:space:]][A-Z]\\.[[:space:]]"
     "" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>[^\.]\s[A-Z]\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ;; TODO: YES breaks from Okapi:

    ;; <rule break="yes">
    ;; <beforebreak>[\.!?…][\u00BB\u2019\u201D\u203A"'\p{Pe}\u0002]*\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="yes">
    ;; <beforebreak>[\.!?…]['"\u00BB\u2019\u201D\u203A\p{Pe}\u0002]*</beforebreak>
    ;; <afterbreak>\p{Lu}[^\p{Lu}]</afterbreak>
    ;; </rule>

    ;; <rule break="yes">
    ;; <beforebreak>\s\p{L}[\.!?…]\s</beforebreak>
    ;; <afterbreak>\p{Lu}\p{Ll}</afterbreak>
    ;; </rule>
    ;; </languagerule>
    ))

(defcustom segment-regexes-additional-en-list
  '(
    ;;; segment.el additions

    ;; Sr. / Jr. can end a sentence
    ;; a single title addition from okapi above (otherwise we are using omegat)
    ("[JS]r\\."
     "[[:space:]][[:lower:]]" :break nil)

    ;; this is wrong, but how to differentiate U.S. ending a sentence, and
    ;; U.S. followed by a proper noun?
    ;; omegat's U.K. rule above only mandates a space, which is also imperfect
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
     "[[:space:]][[:digit:]]" :break nil))
  "Additional regexes of before break / after break rules.
Used for ending or not ending sentences."
  :group 'segment-regexes
  :type 'plist)

;; TODO make construct-list language-generic
(defun segment-regexes--construct-en-list ()
  "Return the full collection of regex rules for English."
  (setq segment-regexes-en-list
        ;; TODO: work out correct order
        (append
         segment-regexes-additional-en-list
         (cond ((eq segment-ruleset-framework 'omegat)
                segment-regexes-omegat-en-list)
               ((eq segment-ruleset-framework 'okapi)
                segment-regexes-okapi-en-list)
               ((eq segment-ruleset-framework 'all)
                segment-regexes-omegat-en-list
                segment-regexes-okapi-en-list)))))

;;; omegat French rules:

(defconst segment-regexes-omegat-fr-list
  '(
    ;; <languagerule languagerulename="French">

    ("pp\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>pp\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[^a-z]p\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>[^a-z]p\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("\\([A-Z]\\.\\)\\{2\\}"
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>([A-Z]\.){2,}</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ;; not too sure about this?
    ("^M\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>^M\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("MM\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>MM\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ;; also not too sure about this?
    ("[^A-Z]M\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>[^A-Z]M\.</beforebreak>
    ;; ;; <afterbreak>\s</afterbreak>
    ;; ;; </rule>

    ("etc\\."
     "[[:space:]][a-z]" :break nil)
    ;; ;; <rule break="no">
    ;; ;; <beforebreak>etc\.</beforebreak>
    ;; ;; <afterbreak>\s[a-z]</afterbreak>
    ;; ;; </rule>

    ("Mr\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>Mr\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("Mme\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>Mme\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("Dr\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>Dr\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("Mlle\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>Mlle\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("Prof\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>Prof\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("Resp\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>Resp\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[rR]éf\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>Réf\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>réf\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[C\\.[[:space:]]?A\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>C\.A\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>C\.\sA\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[[:space:]][A-Z]\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\s[A-Z]\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[cC]f\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>Cf\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>cf\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[Aa]rt\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>(A|a)rt\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[Aa]rt\\."
     "[[:digit:]]+" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>(A|a)rt\.</beforebreak>
    ;; <afterbreak>\d+</afterbreak>
    ;; </rule>

    ;; FIXME: 'vol' can end a sentence!: J'ai raté mon vol.
    ;; maybe the after-break should be roman numeral or digit
    ("[Vv]ol\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>[Vv]ol\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("i\\.e\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>i\.e\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("St\\."
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>St\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("\\."
     "[[:space:]][a-z]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\.</beforebreak>
    ;; <afterbreak>\s[a-z]</afterbreak>
    ;; </rule>

    ;; non-breaking space \u00A0 or space and closing quotation marks:
    ("\\."
     "[\u00A0[:space:]]»" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>\.</beforebreak>
    ;; <afterbreak>[\u00A0\s]»</afterbreak>
    ;; </rule>

    ("[.?!][\u00A0[:blank]]»"
     "[[:space:]]" :break nil)
    ;; <rule break="no">
    ;; <beforebreak>[\.\?\!][\u00A0\s]»</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ;; </languagerule>
    ))


(provide 'segment-regexes)
;;; segment-regexes.el ends here
