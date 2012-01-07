;; (set! qq-expand-list
;;       (lambda (x)
;;         (if (consp x)
;;             (if (eq (car x) 'qq-unquote)
;;                 (list 'list (cadr x))
;;                 (if (eq (car x) 'qq-splice)
;;                     (cadr x)
;;                     (if (eq (car x) 'quasiquote)
;;                         (qq-expand-list (qq-expand (cadr x)))
;;                         (list 'list (list 'append
;;                                           (qq-expand-list (car x))
;;                                           (qq-expand (cdr x)))))))
;;             (list 'quote (list x)))))

;; (set! qq-expand
;;       (lambda (x)
;;         (if (consp x)
;;             (if (eq (car x) 'qq-unquote)
;;                 (cadr x)
;;                 (if (eq (car x) 'quasiquote)
;;                     (qq-expand (qq-expand (cadr x)))
;;                     (list 'append
;;                           (qq-expand-list (car x))
;;                           (qq-expand (cdr x)))))
;;             (list 'quote x))))

;; props to http://norstrulde.org/ilge10/
(set! qq
      (lambda (x)
        (if (consp x)
            (if (eq 'qq-unquote (car x))
                (cadr x)
                (if (eq 'quasiquote (car x))
                    (qq (qq (cadr x)))
                    (if (consp (car x))
                        (if (eq 'qq-splice (caar x))
                            (list 'append (cadar x) (qq (cdr x)))
                            (list 'cons (qq (car x)) (qq (cdr x))))
                        (list 'cons (qq (car x)) (qq (cdr x))))))
            (list 'quote x))))

(defmacro quasiquote (thing)
  (qq thing))

;;;; let the show begin

(defmacro defun (name args . body)
  `(%set-function-name (set! ,name (lambda ,args ,@body)) ',name))

(defmacro when (pred . body)
  `(if ,pred (progn ,@body)))

(defmacro unless (pred . body)
  `(if ,pred nil (progn ,@body)))

(defun mapcar (func list)
  (when list
    (cons (func (car list)) (mapcar func (cdr list)))))

(defun foreach (list func)
  (when list
    (func (car list))
    (foreach (cdr list) func)))

(defmacro let (defs . body)
  `((lambda ,(mapcar (lambda (x)
                       (if (listp x)
                           (car x)
                           x)) defs)
      ,@body)
    ,@(mapcar (lambda (x)
                (if (listp x)
                    (cadr x))) defs)))

(defmacro let* (defs . body)
  (if defs
      `(let (,(car defs))
         (let* ,(cdr defs)
           ,@body))
      `(progn ,@body)))

(defmacro labels (defs . body)
  `(let ,(mapcar (lambda (x) (car x)) defs)
     ,@(mapcar (lambda (x)
                 `(set! ,(car x) (lambda ,(cadr x) ,@(cddr x)))) defs)
     ,@body))

(defmacro flet (defs . body)
  `(let ,(mapcar (lambda (x)
                   `(,(car x) (lambda ,(cadr x) ,@(cddr x)))) defs)
     ,@body))

(defmacro or exps
  (when exps
    (let ((x (gensym "OR")))
      `(let ((,x ,(car exps)))
         (if ,x ,x (or ,@(cdr exps)))))))

(defmacro and exprs
  (if exprs
      (let ((x (gensym "AND")))
        `(let ((,x ,(car exprs)))
           (when ,x
             ,(if (cdr exprs) `(and ,@(cdr exprs)) x))))
      t))

(defmacro cond cases
  (if cases
      `(if ,(caar cases)
           (progn ,@(cdar cases))
           (cond ,(cdr cases)))))

(defmacro call/cc (func)
  `(,func (c/c)))

(defmacro with-cc (name . body)
  `((lambda (,name) ,@body) (c/c)))

(defun member (item list)
  (if list
      (if (eq item (car list))
          list
          (member item (cdr list)))))

(defmacro case (expr . cases)
  (let ((vexpr (gensym "CASE")))
    `(let ((,vexpr ,expr))
       ,(labels ((recur (cases)
                        (when cases
                          (if (listp (caar cases))
                              `(if (member ,vexpr ',(caar cases))
                                   (progn ,@(cdar cases))
                                   ,(recur (cdr cases)))
                              (if (and (not (cdr cases))
                                       (or (eq (caar cases) 'otherwise)
                                           (eq (caar cases) t)))
                                  `(progn ,@(cdar cases))
                                  `(if (eq ,vexpr ',(caar cases))
                                       (progn ,@(cdar cases))
                                       ,(recur (cdr cases))))))))
                (recur cases)))))

(defun macroexpand (form)
  (if (and (consp form)
           (symbolp (car form))
           (%macrop (car form)))
      (macroexpand (macroexpand-1 form))
      form))

(defun macroexpand-all (form)
  (if (consp form)
      (let ((form (macroexpand form)))
        (mapcar macroexpand-all form))
      form))

;;;;;

(set! *amb-fail* (lambda (arg)
                   (clog "TOTAL FAILURE")))

(defmacro amb alternatives
  (if alternatives
      `(let ((+prev-amb-fail *amb-fail*))
         (with-cc +sk
           ,@(mapcar (lambda (alt)
                       `(with-cc +fk
                          (set! *amb-fail* +fk)
                          (+sk ,alt)))
                     alternatives)
           (set! *amb-fail* +prev-amb-fail)
           (+prev-amb-fail nil)))
      `(*amb-fail* nil)))

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

;; (sumis 14 1)

;;;;;

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

(macroexpand-all
 '(defun who-owns-the-fish ()
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
     (add () 0))))
