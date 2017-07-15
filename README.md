cm-utils adds coroutine-like process constructs for realtime common
music 2. It allows for a more general concept of a process. Unlike
cm's process macro, which is akin common lisp's loop macro, rt-proc
will work on arbitrary lisp bodys enabling multiple waits and
recursive synchronous subprocesses.

As the package depends on cl-cont (via cl-coroutine), the limitations
of cl-cont apply to this as well.

Dependencies:

- incudine
- cm
- cm-incudine (for realtime output)
- cl-coroutine

Implemented macros are:

rt-proc: similar to cm's "process" macro, allowing arbitrary lisp forms within its body

subproc: a "blocking" subprocess. In contrast to sprout subproc will finish
         the subprocess before continuing.

rt-wait: similar to cm's wait.

NOTE: cm's "events" function can also be used with rt-proc! 


examples comparing the traditional cm process syntax to cm-util's
syntax:

cm:
```lisp
(sprout
 (process
   for keynum in '(60 62 64 65)
   output (new midi :time (now) :keynum keynum :duration 1)
   wait 0.5))
```
cm-utils:
```
(sprout
 (rt-proc
   (loop
      for keynum in '(60 62 64 65)
      do (progn
           (output (new midi :time (now) :keynum keynum))
           (rt-wait 0.5)))))

;;; the same using dolist:

(sprout
 (rt-proc
   (dolist (keynum '(60 62 64 65))
      (output (new midi :time (now) :keynum keynum))
      (rt-wait 0.5))))

;;; using multiple waits:

(sprout
 (rt-proc
   (loop
      for keynum in '(60 62 64 65)
      do (progn
           (output (new midi :time (now) :keynum keynum))
           (rt-wait 0.1)
           (output (new midi :time (now) :keynum (+ keynum 11)))
           (rt-wait 0.2)))))

;;; defining as function:

(defun proc-01 ()
 (rt-proc
   (loop
      for keynum in '(60 62)
      do (progn
           (output (new midi :time (now) :keynum keynum))
           (rt-wait 0.1)
           (output (new midi :time (now) :keynum (+ keynum 11)))
           (rt-wait 0.2)))))

(sprout (proc-01))

;;; using recursive synchronous processes:

(defun proc-02 ()
  (rt-proc
   (loop
      for keynum in '(50 52)
      do (progn
           (output (new midi :time (now) :keynum keynum))
           (rt-wait 0.5)
           (subproc (proc-01))
           (output (new midi :time (now) :keynum (+ keynum 11)))
           (rt-wait 0.2)))))

(sprout (proc-02))
```

The code is (c) Orm Finnendahl, released under the GPL (version 2 or
later), without any warranties. Use at your own risk.