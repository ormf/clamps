;;; 
;;; universal-copy.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2019 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :orm-utils)

;;;; Filename: universal-copy.lisp

;;; Deep (recursive) copy of a Common Lisp object.
;;; For example, (ucopy array0) where array0 is an array of structures.
;;; Assumes MOP is loaded.

(defmethod ucopy ((sym symbol))
  "Simply return the symbol."
  sym)

(defmethod ucopy ((num number))
  "Simply return the number."
  num)

(defmethod ucopy ((char character))
  "Simply return the character."
  char)

(defmethod ucopy ((fn function))
  "Simply return the function."
  fn)

(defmethod ucopy ((path pathname))
  "Simply return the path."
  path)

(defmethod ucopy ((seq sequence))
  "Copy a sequence recursively."
  (map (type-of seq) #'ucopy seq))

(defmethod ucopy ((ht hash-table))
  "Copy a hash table recursively."
  (loop with new-ht = (make-hash-table
                        :test (hash-table-test ht)
                        :size (hash-table-size ht)
                        :rehash-size (hash-table-rehash-size ht)
                        :rehash-threshold (hash-table-rehash-threshold ht))
      for key being the hash-key in ht using (hash-value value)
      do (setf (gethash (ucopy key) new-ht) (ucopy value))
      finally (return new-ht)))

(defmethod ucopy ((arr array))
  "Copy an array recursively."
  (let ((new-arr (make-array (array-dimensions arr)
                             :element-type (array-element-type arr)
                             :adjustable (adjustable-array-p arr))))
    (dotimes (i (array-total-size arr))
      (setf (row-major-aref new-arr i)
        (ucopy (row-major-aref arr i))))
    new-arr))

(defmethod ucopy ((struct structure-object))
  "Copy a structure recursively."
  (let ((new-struct (copy-structure struct))
        (slots (class-direct-slots (class-of struct))))
    (dolist (slot slots)
      (let ((slot-name (slot-definition-name slot)))
        (setf (slot-value new-struct slot-name)
          (ucopy (slot-value struct slot-name)))))
    new-struct))

(defmethod ucopy ((inst standard-object))
  "Copy an instance of a class recursively."
  (let ((new-inst (allocate-instance (class-of inst)))
        (slots (class-direct-slots (class-of inst))))
    (dolist (slot slots)
      (let ((slot-name (slot-definition-name slot)))
        (when (slot-boundp inst slot-name)
          (setf (slot-value new-inst slot-name)
            (ucopy (slot-value inst slot-name))))))
    new-inst))
