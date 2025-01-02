;;;; -*- Mode: lisp -*-
;;;
;;; package.lisp
;;;
;;; Copyright (c) 2024 Orm Finnendahl
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

(defpackage :cuda-usocket-osc
  (:use #:cl #:usocket #:bt)
  (:nicknames #:cu-osc)
  (:import-from :alexandria #:define-constant #:positive-fixnum
                #:non-negative-fixnum #:with-gensyms)
;;;  (:shadow #:open #:close #:stream #:input-stream-p #:output-stream-p)
  (:export #:make-osc-receiver
           #:remove-osc-responder
           #:open #:close #:send #:stream #:input-stream #:output-stream
           #:stream-p #:input-stream-p #:output-stream-p
           #:out-stream-open?
           #:input-cu-osc-stream
           #:input-cu-osc-stream-receiver


           ))

