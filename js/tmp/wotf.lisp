;;; amb

(set! *amb-fail* (lambda (arg)
                   (clog "TOTAL FAILURE")))

(defmacro amb alternatives
  (if alternatives
      `(let ((+prev-amb-fail *amb-fail*))
         (with-cc +sk
           ,@(map (lambda (alt)
                    `(with-cc +fk
                       (set! *amb-fail* +fk)
                       (+sk ,alt)))
                  alternatives)
           (set! *amb-fail* +prev-amb-fail)
           (+prev-amb-fail nil)))
      `(*amb-fail* nil)))

;; -----------------------------------------------------------------

;; Who owns the fish? -- See http://weitz.de/einstein.html for a
;; description of the problem.

;; The solution here is based on my implementation in JavaScript:
;; http://mihai.bazon.net/blog/amb-in-javascript/take-two#wotf

;; fail if condition is not true
(defmacro assert (condition)
  `(unless ,condition (amb)))

;; asserts that both conditions are either true or false
(defmacro iff (c1 c2)
  `(assert (eq ,c1 ,c2)))

;; This is an *ugly* macro.  It assumes the existence of some
;; variables in the scope where it is used.  Normally I would use
;; macrolet for this, but JCLS doesn't yet have macrolet.
(defmacro pick (type . choices)
  `(let ((tmp (amb ,@choices)))
     (assert (not (find-house houses ,type tmp)))
     tmp))

;; a house is represented as a list of 5 elements -- nationality,
;; beverage, tobacco brand, pet and color (in this order).  This
;; function retrieves the requested property of a house.
(defun house-prop (prop house)
  (case prop
    (:nationality (elt house 0))
    (:beverage (elt house 1))
    (:tobacco (elt house 2))
    (:pet (elt house 3))
    (:color (elt house 4))))

;; In a list of `houses', locate the one that has property `type'
;; equal to `value' and return its index (zero-based).  Return NIL if
;; not found.
(defun find-house (houses type value)
  (with-cc return
    (let ((i 0))
      (foreach houses
               (lambda (h)
                 (when (eq (house-prop type h) value)
                   (return i))
                 (set! i (+ i 1)))))))

;; asserts that houses having property `t1' = `v1' and `t2' = `v2' are
;; neighbors (distance between them is 1 or -1).
(defun neighbors (houses t1 v1 t2 v2)
  (let* ((i1 (find-house houses t1 v1))
         (i2 (find-house houses t2 v2))
         (diff (- i1 i2)))
    (assert (or (= 1 diff)
                (= (- 1) diff)))))

;; main entry point into the problem.  using the `pick' macro, select
;; nationality, beverage, tobacco, pets and colors, such that the rest
;; of the program doesn't fail.  The “rest of the program” simply
;; asserts the problem conditions, using the helper functions and
;; macros defined above.
(defun who-owns-the-fish ()
  (labels ((add (houses index)
             (let ((nat (pick :nationality  'british  'swedish 'danish   'norwegian 'german))
                   (col (pick :color        'red      'green   'white    'yellow    'blue))
                   (bev (pick :beverage     'tea      'milk    'coffee   'beer      'water))
                   (tob (pick :tobacco      'pallmall 'dunhill 'marlboro 'winfield  'rothmans))
                   (pet (pick :pet          'dogs     'cats    'horses   'birds     'fish)))
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
                       (clog "*** SOLUTION!")
                       ;; and return it
                       houses)
                     (add houses (+ index 1)))))))
    (add () 0)))

(who-owns-the-fish)

(defun sumis (n sum)
  (let (solutions)
    (with-cc *amb-fail*
      (labels ((add (numbers)
                 (labels ((rec (sum numbers)
                            (if numbers
                                (rec (+ sum (car numbers)) (cdr numbers))
                                sum)))
                   (rec 0 numbers)))
               (required-sum? (numbers)
                 (= sum (add numbers)))
               (rec (numbers next)
                 (if (= next 0)
                     (progn
                       (when (required-sum? numbers)
                         (set! solutions (cons numbers solutions)))
                       (amb))
                     (rec (cons (amb next (- next))
                                numbers)
                          (- next 1)))))
        (rec (list) n)))
    (clog (length solutions))))

(sumis 14 1)
