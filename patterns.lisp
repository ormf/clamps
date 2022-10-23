;;; patterns.lisp
;;;
;;; Copyright (c) 2018 Orm Finnendahl
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


;;; 
;;; more pattern classes not existent in original cm2
;;;

(in-package #:cm)

;;; permutation pattern class
;;;
;;; repeatedly apply a permutation to a given list of items.
;;;
;;; :of keyword specifies the items to permutate.
;;;
;;; :idxs keyword is a list of permutation indexes. Make sure it has
;;; the same length as the item list and contains all indexes from 0
;;; to length-1.
;;;
;;; :immediately specifies whether the permutation is immediately
;;; applied to the item list before accessing the first element of the
;;; pattern. If set to nil, next first returns one period of the
;;; original item list before applying the permutation. Default is t. 
;;;
;;;
;;;

#|

Examples:

(let ((seq (new permutation :of '(a b c d e) :idxs '(4 3 0 2 1) :immediately nil)))
  (loop for x below 6 collect (next seq t)))

-> ((A B C D E) (E D A C B) (B C E A D) (D A B E C) (C E D B A) (A B C D E))


(let ((seq (new permutation :of '(a b c d e) :idxs '(4 3 0 2 1) :immediately t)))
  (loop for x below 6 collect (next seq t))) 

-> ((E D A C B) (B C E A D) (D A B E C) (C E D B A) (A B C D E) (E D A C B))

|#

(progn
  (defclass permutation (cycle)
    ((idxs :initform 0 :initarg :idxs :accessor idxs)
     (immediately :initform t :initarg :immediately :accessor immediately)))
  (defparameter <permutation> (find-class 'permutation))
  (finalize-class <permutation>)
  (values))

(defmethod pattern-external-inits ((obj permutation))
  (let ((inits (call-next-method)))
    (append inits
            (if (equal (permutation obj) 0)
                (list)
                (list ':idxs (expand-pattern-value (idxs obj))
                      ':immediately (immediately obj))))))

(defmethod initialize-instance :after ((obj permutation) &rest args)
  args
  (let ((cyc (pattern-data obj)))
    (cycl-data-set! cyc (copy-list (cycl-data cyc)))
    (unless (immediately obj) (cycl-tail-set! cyc (copy-list (cycl-data cyc))))
    (values)))

(defmethod next-in-pattern ((obj permutation))
  (flet ((perm-in-place (lis len perm)
           (block main
             (loop
                for i = 0 then (incf i)
                while (< i len)
                do (progn
                     (loop ;;; skip already accessed elems of perm
                        while (< (elt perm i) 0) ;;; is elem lready done/accessed?
                        do (progn
                             (incf i)
                             (if (= i len) (return-from main))))
                     (loop ;;; do a full cycle of replacements starting with the i-th element of perm
                        for from = i then to
                        for to = (elt perm from)
                        until (= to i)
                        for tmp = (elt lis to)
                        do (setf (elt lis to) (elt lis from) ;;; swap list elements
                                 (elt lis from) tmp
                                 (elt perm from) (+ -1 (* -1 (elt perm from)))) ;;; tag elem of perm as accessed/done.
                        finally (setf (elt perm from) (+ -1 (* -1 (elt perm from)))))))) ;;; tag starting element of permutation cycle as accessed/done.
           (loop for i below len do (setf (elt perm i) (* -1 (+ 1 (elt perm i))))) ;;; restore all elements of permutation.
           lis))
         (let ((cyc (pattern-data obj)))
           (if (null (cycl-tail cyc)) ;;; end of period after next element?
               (cycl-tail-set! cyc
                (perm-in-place (cycl-data cyc) (pattern-length obj) (idxs obj))))
           (pop-cycl cyc))))

(export 'permutation 'cm)
