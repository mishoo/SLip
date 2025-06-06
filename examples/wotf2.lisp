(defpackage :wotf
  (:use :sl :%))

(in-package :wotf)

(defun call/cc (func)
  (funcall func (%::c/c)))

(defglobal *fail* (lambda (ret)
                    (console.print "TOTAL FAILURE")))

(defun amb (alternatives)
  (if alternatives
      (call/cc
       (lambda (k1)
         (loop with fail = *fail*
               for val in alternatives
               do (call/cc (lambda (k2)
                             (setf *fail* k2)
                             (funcall k1 val)))
               finally (funcall fail nil))))
      (funcall *fail* nil)))

;; -----------------------------------------------------------------

;; Who owns the fish? -- See http://weitz.de/einstein.html for a
;; description of the problem.

;; The solution here is based on my implementation in JavaScript:
;; http://mihai.bazon.net/blog/amb-in-javascript/take-two#wotf

(defparameter *backtrack-count* 0)

;; fail if condition is not true
(shadow 'assert)
(defmacro assert (condition)
  `(unless ,condition
     (incf *backtrack-count*)
     (amb nil)))

;; a house is represented as a list of 5 elements -- nationality,
;; beverage, tobacco brand, pet and color (in this order).  This
;; function retrieves the requested property of a house.
(defun house-prop (prop house)
  (case prop
    (:nationality (car house))
    (:beverage (cadr house))
    (:tobacco (caddr house))
    (:pet (cadddr house))
    (:color (cadddr (cdr house)))))

;; In a list of `houses', locate the one that has property `type'
;; equal to `value' and return its index (zero-based).  Return NIL if
;; not found.
(defun find-house (houses type value)
  (let rec ((houses houses)
            (i 0))
    (when houses
      (if (eq (house-prop type (car houses)) value)
          i
          (rec (cdr houses) (1+ i))))))

;; asserts that houses having property `t1' = `v1' and `t2' = `v2' are
;; neighbors (distance between them is 1 or -1).
(defun neighbors (houses t1 v1 t2 v2)
  (let* ((i1 (find-house houses t1 v1))
         (i2 (find-house houses t2 v2))
         (diff (- i1 i2)))
    (assert (or (= 1 diff)
                (= -1 diff)))))

;; main entry point into the problem.  using the `pick' macro, select
;; nationality, beverage, tobacco, pets and colors, such that the rest
;; of the program doesn't fail.  The “rest of the program” simply
;; asserts the problem conditions, using the helper functions and
;; macros defined above.
(defun who-owns-the-fish ()
  (setf *backtrack-count* 0)
  (labels
      ((pick (houses type . choices)
         (let ((tmp (amb choices)))
           (assert (not (find-house houses type tmp)))
           tmp))
       (iff (c1 c2)
         (assert (eq c1 c2)))
       (add (houses index)
         (let ((nat (pick houses :nationality  'british  'swedish 'danish   'norwegian 'german))
               (col (pick houses :color        'red      'green   'white    'yellow    'blue))
               (bev (pick houses :beverage     'tea      'milk    'coffee   'beer      'water))
               (tob (pick houses :tobacco      'pallmall 'dunhill 'marlboro 'winfield  'rothmans))
               (pet (pick houses :pet          'dogs     'cats    'horses   'birds     'fish)))
           (iff (eq nat 'british) (eq col 'red))
           (iff (eq nat 'swedish) (eq pet 'dogs))
           (iff (eq nat 'danish) (eq bev 'tea))
           (iff (eq col 'white)
                (and (> index 0)
                     (eq 'green
                         (house-prop :color (elt houses (- index 1))))))
           (iff (eq col 'green) (eq bev 'coffee))
           (iff (eq tob 'pallmall) (eq pet 'birds))
           (iff (eq col 'yellow) (eq tob 'dunhill))
           (iff (= index 2) (eq bev 'milk))
           (iff (= index 0) (eq nat 'norwegian))
           (iff (eq tob 'winfield) (eq bev 'beer))
           (iff (eq nat 'german) (eq tob 'rothmans))
           (let* ((h (list nat bev tob pet col))
                  (houses (append houses (list h))))
             ;;(clog houses) ; log something so we don't look like we're frozen.
             (if (= index 4)
                 (progn
                   (neighbors houses :tobacco 'marlboro :pet 'cats)
                   (neighbors houses :pet 'horses :tobacco 'dunhill)
                   (neighbors houses :nationality 'norwegian :color 'blue)
                   (neighbors houses :tobacco 'marlboro :beverage 'water)
                   (console.print "*** SOLUTION!")
                   ;; and return it
                   houses)
                 (add houses (+ index 1)))))))
    (add () 0)))
