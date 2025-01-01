;;;
;;; incudine-setup.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2018-24 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package #:incudine)
(export '(setup-meters
          setup-io
          node-free-unprotected)
        :incudine)

(defun setup-io ()
  (free 0)
  (sleep 0.1)
  (make-group 100)
  (make-group 200 :after 100)
  (make-group 300 :after 200)
  (make-group 400 :after 300)
;;  (clear-buses 0 32 :id 1 :head 100)
;;  (cp-input-buses :id 2 :tail 100)
;;  (mix-bus-to-out :id 3 :startidx 0 :head 300)
;;  (cp-output-buses :id 4 :tail 300)
  )

(defun node-free-unprotected ()
  "Free all Incudine nodes of /group 200/. For details of the function of
this group refer to section
<<clamps:General Incudine Setup>> in Clamps Packages.

@See-also
rts-hush
"
  (dogroup (n (node 200))
    (free n)))

;;; (setup-io)
;;; (dump (node 0))
;;; (block-size)
;;; (set-rt-block-size 256)
;;; (rt-start)

