;; segment-regexes.el --- regexes for sentence segmentation rules  -*- lexical-binding: t; -*-

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

;; regexes for complex sentence segmentation rules.
;; ported from OmegaT and Okapi Framework default/English srx rules.

;; each rule has two regexes, before and after, to facilitate sophisticated
;; conditional breaks, such as only if regex X is followed by regex Y.

;;; Code:

;; TODO: add a break yes/no to every item?

;; NB: after-break rules are only needed if they're different to [[:blank:]]

(defcustom segment-regexes-en-alist
  '(
    
    ("etc\\."
     "[[:blank:]]+[[:lower:]]")
    ;; <rule break="no">
    ;; <beforebreak>etc\.</beforebreak>
    ;; <afterbreak>\s+\P{Lu}</afterbreak>
    ;; </rule>

    ("Dr\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>Dr\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("U\\.K\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>U\.K\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("U\\.S\\."
     "[[:blank:]]")

    ("M\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>M\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("Mr\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>Mr\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("Mrs\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>Mrs\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("Ms\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>Ms\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("Prof\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>Prof\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[Ee]\\.g\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>(?i)e\.g\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[Ii]\\.e\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>(?i)i\.e\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("resp\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>resp\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("stel\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>\stel\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[Ff]ig\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>(?i)fig\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("St\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>St\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[[:blank:]][A-Z]\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>\s[A-Z]\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[apAP]\\.?[mM]\\."
     "[[:blank:]][a-z]")
    ;; <rule break="no">
    ;; <beforebreak>[apAP]\.?[mM]\.</beforebreak>
    ;; <afterbreak>\s[a-z]</afterbreak>
    ;; </rule>

    ("Mt\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>Mt\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("No\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>No\.</beforebreak>
    ;; <afterbreak>\s\d</afterbreak>
    ;; </rule>

    ("[Aa]pprox\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>[Aa]pprox\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[[:digit:][:blank:]]mi?n\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>\d\smi?n\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[[:digit:][:blank:]]sec\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>\d\ssec\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>

    ("[[:blank:]][Vv][sS]?\\."
     "[[:blank:]]")
    ;; <rule break="no">
    ;; <beforebreak>\s[vV][sS]?\.</beforebreak>
    ;; <afterbreak>\s</afterbreak>
    ;; </rule>
    ;; </languagerule>

;;; Okapi defaultSegmentation.srx
    ;; default lang

    ("\\b\\(pp\\|e\\.?[[:blank:]]*g\\|i\\.?[[:blank:]]*e\\|no\\|[Vv]ol\\|[Rr]col\\|maj\\|Lt\\|[Ff]ig\\|[Ff]igs\\|[Vv]iz\\|[Vv]ols\\|[Aa]pprox\\|[Ii]ncl\\|Pres\\|Prof\\|[Dd]ept\\|min\\|max\\|[Gg]ovt\\|c\\.?[[:blank:]]*f\\|[Vv]\\.?s\\)\\."
     "[[:blank:]][[:lower:]]")
    ;; <rule break="no">
    ;; <beforebreak>\b(pp|e\.?\s*g|i\.?\s*e|no|[Vv]ol|[Rr]col|maj|Lt|[Ff]ig|[Ff]igs|[Vv]iz|[Vv]ols|[Aa]pprox|[Ii]ncl|Pres|Prof|[Dd]ept|min|max|[Gg]ovt|c\.?\s*f|vs)\.</beforebreak>
    ;; <afterbreak>\s+[^\p{Lu}]</afterbreak>

    ;; months followed by a digit
    ("\\([Jj]an\\|[Ff]eb\\|[Mm]ar\\|[Aa]pr\\|[Jj]un\\|[Jj]ul\\|[Aa]ug\\|[Ss]ep\\|[Oo]ct\\|[Nn]ov\\|[Dd]ec\\|[Ee]st\\|[Tt]el\\|[Pp]h\\)\\."
     "[[:blank:]+[:digit:]]")
    ;; <rule break="no">
    ;; <beforebreak>((?i)jan|feb|mar|apr|jun|jul|aug|sep|oct|nov|dec|est|tel)\.</beforebreak>
    ;; <afterbreak>\s+\d</afterbreak>
    ;; </rule>


    ;; titles, followed by uppercase
    ("\\b\\(St\\|Gen\\|Hon\\|Dr\\|Mr\\|Ms\\|Mrs\\|Col\\|Maj\\|Brig\\|Sgt\\|Capt\\|Cmnd\\|Sen\\|Rev\\|Rep\\|Revd\\)\\."
     "[[:blank:]][[:upper:]]")
    ;; <rule break="no">
    ;; <beforebreak>\b(St|Gen|Hon|Dr|Mr|Ms|Mrs|Col|Maj|Brig|Sgt|Capt|Cmnd|Sen|Rev|Rep|Revd)\.</beforebreak>
    ;; <afterbreak>\s+\p{Lu}</afterbreak>


;;; TODO: okapi alt segmentation
    ;; en only

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

    ;; months (done above)

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


    ;; <rule break="no">
    ;; <beforebreak>\bPh\.?D\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">

    ("\\bet\\b[[:blank:]]al\\.[[:blank:]]" nil)
    ;; <beforebreak>\bet\b\s\bal\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ;; titles (done above?)
    
    ;; <rule break="no">
    ;; <beforebreak>\b(St|Gen|Hon|Prof|Dr|Mr|Ms|Mrs|[JS]r|Col|Maj|Brig|Sgt|Capt|Cmnd|Sen|Rev|Rep|Revd)\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\b(St|Gen|Hon|Prof|Dr|Mr|Ms|Mrs|[JS]r|Col|Maj|Brig|Sgt|Capt|Cmnd|Sen|Rev|Rep|Revd)\.\s[A-Z]\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\bcf\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    ;; <rule break="no">

    ("\\bInc\\.[[:blank:]]" nil)
    ;; <beforebreak>\bInc\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ("\\bCorp\\.[[:blank:]]" nil)
    ;; <rule break="no">
    ;; <beforebreak>\bCorp\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ("\\bBros\\.[[:blank:]]" nil)
    ;; <rule break="no">
    ;; <beforebreak>\bBros\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ("\\bDist\\.[[:blank:]]" nil)
    ;; <rule break="no">
    ;; <beforebreak>\bDist\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ("\\bCo\\.[[:blank:]]" nil)
    ;; <rule break="no">
    ;; <beforebreak>\bCo\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ;; ellipsis, poss surrounded by [] or ():
    ("[[(]*…[])]*"
     "[[:lower:]]")
    ;; <rule break="no">
    ;; <beforebreak>[\[\(]*…[\]\)]* </beforebreak>
    ;; <afterbreak>\p{Ll}</afterbreak>
    ;; </rule>
    
    ;; <rule break="no">
    ;; <beforebreak>\p{Ps}[!?]+\p{Pe} </beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ;; <rule break="no">
    ;; <beforebreak>[\.!?…]+\p{Pe} </beforebreak>
    ;; <afterbreak>\p{Ll}</afterbreak>
    ;; </rule>

    ;; <rule break="no">
    ;; <beforebreak>[\"”']\s*</beforebreak>
    ;; <afterbreak>\s*\p{Ll}</afterbreak>
    ;; </rule>

    ;; <rule break="no">
    ;; <beforebreak>['"„][\.!?…]['"”]\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>
    
    ;; <rule break="no">
    ;; <beforebreak>\b\p{L}\.\s</beforebreak>
    ;; <afterbreak>\p{L}\.\s</afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>\b\p{L}\.</beforebreak>
    ;; <afterbreak>\p{L}\.</afterbreak>
    ;; </rule>
    ;; <rule break="no">
    ;; <beforebreak>[\.\s]\p{L}{1,2}\.\s</beforebreak>
    ;; <afterbreak>[\p{N}\p{Ll}]</afterbreak>
    ;; </rule>

    ;; three stops as ellipsis, poss surrounded in [] or ()
    ("[[(]*\\.\\.\\.[])]*" nil)
    ;; <rule break="no">
    ;; <beforebreak>[\[\(]*\.\.\.[\]\)]* </beforebreak>
    ;; <afterbreak>[^\p{Lu}]</afterbreak>
    ;; </rule>

    ("\\b[[:lower:]]\\.[[:blank:]][[:lower:]]\\.[[:blank:]]" nil)
    ;; <rule break="no">
    ;; <beforebreak>\b\p{Lu}\.\s\p{Lu}\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ("\\b[[:lower:]]\\.[[:lower:]]\\.[[:blank:]]" nil)
    ;; <rule break="no">
    ;; <beforebreak>\b\p{Lu}\.\p{Lu}\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ;; Name initials:

    ("[^\\.][[:blank:]][A-Z]\\.[[:blank:]]" nil)
    ;; <rule break="no">
    ;; <beforebreak>[^\.]\s[A-Z]\.\s</beforebreak>
    ;; <afterbreak></afterbreak>
    ;; </rule>

    ;; YES breaks:
    ;; need to add a yes here
    
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

    )
  "Regexes of before break / after break rules.
Used for ending or not ending sentences."
  :group 'segment
  :type 'alist)


(provide 'segment-regexes)
;;; segment-regexes.el ends here
