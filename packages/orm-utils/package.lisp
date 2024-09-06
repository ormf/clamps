;;;; package.lisp

(defpackage #:orm-utils
  (:shadowing-import-from :closer-mop :defmethod :standard-generic-function :defgeneric)
  (:use #:cl #:closer-mop #:org.tfeb.hax.collecting)
  (:nicknames #:ou)
  (:export 
   #:SYSTEM-VERSION
   #:N
   #:FILTER
   #:REDUCING
;;   #:WHILE
   #:ENSURE-PROP
   #:EVERY-NTH
;;   #:FIRSTN
   #:RFIND
   #:DEFAULT
   #:WITH-OUTPUT-TO-FILE
   #:MAKE-ADJUSTABLE-STRING
   #:DROUND #:MAP-TREE #:FLATTEN #:MAP-PARAMS #:DEF-PARAMS #:DIFFERENTIATE #:INTEGRATE
   #:MAPPEND
   #:FV2CT #:CT2FV #:MTOF #:FTOM
;;   #:IP-EXP #:IP-LIN #:IP-LOG
   #:ROTATE
   #:GROUP
   #:GROUP-BY
   #:GROUP-BY-KEY
   #:REPEATED #:DO-REPEATED #:PERMUTE #:LAST-N
   #:N-APPLY
   #:AMP->DB #:DB->AMP
   #:FR->CT #:CT->FR
   #:MAPPLY
   #:FLATTEN-FN
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
   #:V-COLLECT
   #:N-COLLECT
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
   #:CALL/COLLECTING
   #:COUNT-ELEMENTS
   #:PARSE-PROPLIST
   #:WITH-PROPS
   #:WITH-SHADOWED-VARIABLE
   #:MAP-PROPLIST
   #:DO-PROPLIST
   #:DO-PROPLIST/COLLECTING
   #:GET-PROP
   #:GET-PROPS-LIST
   #:DELETE-PROPS
   #:GET-PROPS-LIST
   #:MTON
   #:NTOM
   #:N-EXP
   #:N-LIN
   #:LIN-N
   #:EXP-N
   #:N-LIN-DEV
   #:N-EXP-DEV
   #:N-LIN-FN
   #:N-EXP-FN
   #:N-LIN-REV-FN
   #:N-EXP-REV-FN
   #:M-EXP
   #:N-EXP-ZERO
   #:M-EXP-ZERO
   #:M-LIN
   #:R-EXP
   #:R-LIN
   #:RANDM
;;   #:RAND
   #:N-EXP-DEV
   #:N-LIN-DEV
   #:R-EXP-DEV
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
   #:WITH-LIN-MIDI-FN
   #:WITH-EXP-MIDI-FN
   #:DEFCONST
   #:R-ELT
   #:R-GETF
   #:INDEX-LIST
   #:SUBSEQX
   #:DELETE-PROPS
   #:CALCSNDBYTES
   #:GET-TIME
   #:FORMAT-TIME
   #:MAKE-QUANTLIST
   #:QUANTIZE-TIME
   #:PORT-AVAILABLE-P
   #:DEFPARAMETER*
   #:DEFVAR*
   #:CLIP
   )
  )
