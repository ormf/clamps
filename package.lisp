;;;; package.lisp
;;
;;; **********************************************************************
;;; Copyright (c) 2024 Orm Finnendahl
;;; <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(defpackage #:clog-dsp-widgets
  (:use #:cl #:clog #:cl-refs)
  (:export
   #:*bindings*
   #:clear-bindings
   #:binding #:b-ref #:b-attr #:b-elist #:b-map #:b-unwatch
   #:binding-name
   #:define-watch
   #:bind-ref-to-attr
   #:b-unregister
   #:set-on-data
   #:create-o-knob
   #:create-o-numbox
   #:create-o-bang
   #:create-o-toggle
   #:create-o-radio
   #:create-o-slider
   #:create-o-multislider
   #:create-o-vumeter
   #:create-hide-button
   #:create-collection
   #:format-style
   #:opt-format-attr
   #:create-grid

   #:flash
   #:pulse-on
   #:pulse-off
   #:highlight
   #:start
   #:new-window

   #:clog-dsp-widgets-initialize))
