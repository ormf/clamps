;;; 
;;; songplayer.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :cl-poolplayer)

(defstruct state
  (x 0)
  (y 0)
  (xfac 1)
  (yfac 1)
  (local-x-offs 0)
  (length 0)
  (bufid -1)
  (curr-innermost-rec-obj nil) ;; the rec-obj we're in (if any) while traversing the score
  (obj-hash (make-hash-table :test #'equal)))

;;; (make-state)

(defmacro update-slot! (slot y)
  "set slot to y if (> y slot)"
  `(setf ,slot (max ,slot ,y)))

(defmacro class-maker (name superclass &rest slot-defs)
  `(progn
     (defclass ,name ,superclass
       ,(loop
	   for x in slot-defs
	   collect (let ((plist x))
		     `(,(first x) 
                        :initform ,(getf plist :initform (second x)) 
                        :initarg ,(getf plist :initarg (ou:make-keyword (format nil "~a" (first x))))
                        :accessor ,(getf plist :accessor (first x))
                        :type ,(getf plist :type t)))))
     (defun ,(intern (string-upcase (format nil "~a-p" name))) (obj)
       (eq (type-of obj) ',name))))

(class-maker 
 rec-obj ()
 (name "" :type string)
 (start nil)
 (elems nil)
 ;; contains line-obs and box-obs. box-objs are rec-objs
 ;; or play-objs reduced to their start time and length
 ;; as placeholders to be able to determine the total
 ;; length of the rec-obj. box-objs aren't drawn by the
 ;; drawing routines to avoid double drawing.
 ;; 
 ;; should it automatically calc the inroute?
 (duration 0))

(class-maker
 song-obj ()
 (x1 nil)
 (y1 nil)
 (duration nil)
 (y2 nil)
 (local-x-offs 0)
 (amp 0)
 (ref nil)
 (color 1)
 (args ()))


(defclass play-obj nil
  ((name :initform "" :initarg :name :accessor name :type string)
   (start :initform 0 :initarg :start :accessor start :type t)
   (local-x-offs :initform 0 :initarg :local-x-offs :accessor local-x-offs :type t)
   (duration :initform 0 :initarg :duration :accessor duration :type t)
   (transp :initform 0 :initarg :transp :accessor transp :type t)
   (stretch :initform 1 :initarg :stretch :accessor stretch :type t)
   (args :initform '() :initarg :args :accessor args :type t)))

(defun collect-items (items state)
  "this routine projects the syntactical representation onto the
structs, creating the objects on the fly, storing references of the
record objects in the obj-hash table of the state argument and
returning a structurally similar list as the syntactical
representation but containing the created objects instead."
  (loop for x in items
        append (apply (symbol-function
                       (intern
                        (format nil "~@:(~a~)" (car x))
                        :cl-poolplayer))
                      (cons state (cdr x)))))

(defun rec (state name dx elems)
  (with-slots (obj-hash x xfac length bufid)
      state
    (let ((name (format nil "~(~a~)" name)))
      (when (gethash name obj-hash) 
        (error "duplicate rec-object name: ~a" name))
      (let ((subtree-state (copy-state state)))
        (list 
         (setf (gethash name obj-hash)
               (make-instance 'rec-obj 
                              :name name
                              :start (incf x (* xfac dx))
                              :elems (prog1
                                         (progn
                                           (setf (slot-value subtree-state 'x) 0)
                                           (incf (slot-value subtree-state 'local-x-offs) 
                                                 (slot-value state 'x))
                                           (setf (slot-value subtree-state 'length) 0)
                                           (collect-items elems subtree-state))
                                       (update-slot! length (+ x (state-length subtree-state)))
                                       (update-slot! bufid (state-bufid subtree-state)))
                              :duration (state-length subtree-state))))))))

(defun play (state name dtime &rest args)
  (let (tmp rec-obj (stretch (getf args :stretch 1)))
    (with-slots (x y xfac yfac local-x-offs length obj-hash)
        state
      (progn
        ;;  To avoid keyword parsing on the args argument we first
        ;;  create the play-object and then modify the slots according
        ;;  to the state. As the stretch and global-speed slots are
        ;;  missing in Version 2 play-objects, we have to remove them
        ;;  from the args argument. stretch and global-speed are
        ;;  directly applied to the speed values.
	(remf args :stretch)
	(remf args :global-speed)
        (setf tmp (apply #'make-instance 'play-obj
			 :name (format nil "~(~a~)" name)
			 :start (incf x (* xfac dtime))
;;; local-x-offs is the absolute x-offs upon entry in current subtree
			 :local-x-offs local-x-offs
                         :stretch stretch
			 args))
        (unless (setf rec-obj (gethash (name tmp) obj-hash))
          (error "no rec object \"~a\"!" (name tmp)))
        (with-slots
              (transp amp region duration)
            tmp
          (setf transp (if (consp transp)
			   (list (+ y (* yfac (first transp)))
				 (+ y (* yfac (second transp))))
			   (list (+ y (* yfac transp))
				 (+ y (* yfac transp)))))
	  (setf duration (* (duration rec-obj) stretch))
;;; update length of state if the end of the new play object exceeds
;;; the previous length.
          (update-slot! length (+ x duration)))
        (list tmp))
      )))

(defun song (state ref dx transp dur color &rest args)
  (declare (ignorable args))
  (with-slots (x xfac yfac local-x-offs length)
      state
    (let* ((x1 (incf x (* xfac dx)))
	   (duration (* xfac dur))
	   (y1 (* yfac transp))
           (y2 y1)
           ;; (y2 (+ y1 (* yfac dy))) ;;; TODO: implement processes along the y axis
           )
      (prog1
          (list
           (make-instance 'song-obj
                          :ref ref
                          :x1 x1 :y1 y1 
	                  :duration duration :y2 y2
                          :local-x-offs local-x-offs
                          :color color
                          :args args))
        (update-slot! length (+ x1 duration))))))

(defun copy-transform (elems offs play-obj)
  "copy all contents of a play object (the stored graph of its rec-obj)
and update their slots according to the global offset and the local
offset of the play-obj and its transp.

Here is an in-depth explanation:

copy-transform gets called in the context of serializing a rec-obj and
all its containied rec-objs and play-objs (in this context we call
this rec-obj the tl-rec-obj).

In case tl-rec-obj contains another rec-obj (called curr-rec-obj),
which contains a play-obj, then copy-transform is called with offs
being (- (x-offset tl-rec-obj) (x-offset curr-rec-obj)).

All elements of the play-obj are returned. Their x1 is scaled
by (stretch play-obj) and shifted by (+ offs (start play-obj)),
duration is scaled by (stretch play-obj) and y1 and y2 are transposed
by (transp play-obj)."
  (with-slots (start transp stretch) play-obj
    (destructuring-bind (transp1 transp2) transp
      (mapcar (lambda (elem)
                (let ((new (cm:copy-object elem)))
                  (setf (x1 new) (+  offs start (* stretch (x1 new))))
                  (incf (y1 new) transp1)
                  (incf (y2 new) transp2)
                  (setf (duration new) (* stretch (duration new)))
                  new))
              elems)))

  (defun shift-copy (elem x-offs)
    (let ((new (cm:copy-object elem)))
      (incf (x1 new) x-offs)
      new)))

(defun expand-rec-obj (elems x-offs expanded-hash)
  (cond ((null elems) ())
        ((song-obj-p (first elems))
         (cons (shift-copy (first elems) x-offs)
               (expand-rec-obj (rest elems) x-offs expanded-hash)))
        ((rec-obj-p (first elems))
         (append (mapcar (lambda (e) (shift-copy e (start (first elems))))
                         (gethash (name (first elems)) expanded-hash))
               (expand-rec-obj (rest elems) x-offs expanded-hash)))
        (t (append (copy-transform (gethash (name (first elems)) expanded-hash) x-offs (first elems))
                   (expand-rec-obj (rest elems) x-offs expanded-hash)))))

(defun expand-obj-hash (obj-hash)
"expand all entries of the record objects by serializing any record
objects contained in the elements. This is some sort of caching to
avoid multiple serializing."
  (let ((expanded-hash (make-hash-table :test #'equal)))
    (maphash
     (lambda (k v)
       (setf (gethash k expanded-hash) (expand-rec-obj (elems v) 0 expanded-hash)))
     obj-hash)
    expanded-hash))

(defun serialize-score (score)
  (let* ((state (make-state))
         (items (collect-items score state))
         (expanded-hash (expand-obj-hash (state-obj-hash state)) ))
    (apply #'append
           (mapcar (lambda (elem)
                     (let ((graph (gethash (name elem) expanded-hash)))
                       (mapcar (lambda (e) (shift-copy e (start elem)))
                               graph)))
                   items))))
