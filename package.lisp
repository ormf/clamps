;;;; package.lisp

(defpackage #:orm-utils
  (:shadowing-import-from :closer-mop :defmethod :standard-generic-function :defgeneric)
  (:use #:cl #:closer-mop)
  (:nicknames #:ou)
  (:export 
   #:FILTER
   #:REDUCING
   #:WHILE
   #:EVERY-NTH
   #:FIRSTN
   #:SORT-BY
   #:RFIND
   #:DEFAULT
   #:WITH-OUTPUT-TO-FILE #:WITH-FILE-STREAM  #:WITH-STREAM-TO-STRING
   #:MAKE-ADJUSTABLE-STRING
   #:DROUND #:MAP-TREE #:FLATTEN #:MAP-PARAMS #:DEF-PARAMS #:DIFFERENTIATE #:INTEGRATE
   #:MAPPEND
   #:FV2CT #:CT2FV #:IP-EXP #:IP-LIN #:IP-LOG #:ROTATE
   #:GROUP
   #:GROUP-BY
   #:GROUP-BY-KEY
   #:REPEATED #:DO-REPEATED #:PERMUTE #:LAST-N
   #:AMP->DB #:DB->AMP
   #:FV->CT #:CT->FV
   #:MAPPLY #:FLATTEN-FN
   #:FIBONACCI
   #:FILE-STRING
   #:PARTITION-SEQ
   #:WITH-SHADOWED-VARIABLE
   #:SPLICE
   #:INSERT
   #:GET-DUPLICATES
   #:ALL-PERMUTATIONS
   #:COMBINATIONS
   #:REVERSE-ALL
   #:PARAM-EXP-FUNC
   #:REPEAT
   #:RANGE
   #:SUM_X
   #:MAP-INDEXED
   #:CASE-EXT
   #:LET-DEFAULT
   #:SETF-DEFAULT
   #:STR-CONCAT
   #:SLURP
   #:SLURP-STRING
   #:SPIT
   #:MAKE-KEYWORD
   #:MAP-ALL-PAIRS
   #:FILE-STRING
   #:WITH-CURR-DIR
   #:DATE-STRING
   #:PUSH-IF
   #:MEMORIZE-RANDOM-STATE
   #:RECALL-RANDOM-STATE
   #:COUNT-ELEMENTS
   #:WITH-PROPS
   #:MTON
   #:NTOM
   #:N-EXP
   #:N-LIN
   #:N-LIN-FN
   #:N-EXP-FN
   #:N-LIN-REV-FN
   #:N-EXP-REV-FN
   #:M-EXP
   #:M-EXP-ZERO
   #:M-LIN
   #:R-EXP
   #:R-LIN
   #:RANDM
;;   #:RAND
   #:N-EXP-DEV
   #:N-LIN-DEV
   #:M-EXP-DEV
   #:M-LIN-DEV
   #:M-LIN-FN
   #:M-LIN-REV-FN
   #:M-LIN-RD-FN
   #:M-LIN-RD-REV-FN
   #:M-EXP-FN
   #:M-EXP-REV-FN
   #:M-EXP-ZERO-FN
   #:M-EXP-ZERO-REV-FN
   #:M-EXP-RD-FN
   #:M-EXP-RD-REV-FN
   #:COPY-INSTANCE
   #:UCOPY
   #:MULTF
   #:RMPROP
   #:ARRAY-SLICE
   #:REPEAT-FORMAT
   #:CD
   #:PWD
   #:WITH-PROPS
   #:WITH-LIN-MIDI-FN
   #:WITH-EXP-MIDI-FN
   #:DEFCONST
   #:R-ELT
   #:R-GETF
   #:INDEX-SEQ
   ))
