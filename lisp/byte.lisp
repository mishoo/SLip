(in-package :sl)

(export '(ash byte byte-size byte-position ldb ldb-test dpb logand logandc1
          logandc2 logeqv logior lognand lognor lognot logorc1 logorc2 logxor
          logtest logcount integer-length))

(defpackage :sl-byte
  (:use :sl))

;; (setq %:*enable-inline* t)

(in-package :sl-byte)

(declaim (inline byte byte-size byte-position
                 logand logior lognot logxor logtest logcount
                 ash ldb ldb-test dpb))

(defun byte (size position)
  (cons size position))

(define-compiler-macro byte (size position)
  (cond
    ((and (integerp size)
          (integerp position))
     `'(,size . ,position))
    (t
     `(cons ,size ,position))))

(defun byte-size (byte)
  (car byte))

(define-compiler-macro byte-size (byte)
  `(car ,byte))

(defun byte-position (byte)
  (cdr byte))

(define-compiler-macro byte-position (byte)
  `(cdr ,byte))

(defun constant-bytespec-p (form)
  (and (consp form)
       (eq 'byte (car form))
       (integerp (cadr form))
       (integerp (caddr form))))

(defmacro with-constant-bytespec ((bytespec size pos) &body body)
  `(when (constant-bytespec-p ,bytespec)
     (let ((,size (cadr ,bytespec))
           (,pos (caddr ,bytespec)))
       ,@body)))

(defmacro op-and (b1 b2)
  `(%:%op BAND ,b1 ,b2))

(defmacro op-ior (b1 b2)
  `(%:%op BIOR ,b1 ,b2))

(defmacro op-xor (b1 b2)
  `(%:%op BXOR ,b1 ,b2))

(defmacro op-ash (num count)
  `(%:%op BASH ,num ,count))

(defmacro op-cnt (num)
  `(%:%op BCNT ,num))

(defmacro op-not (num)
  `(%:%op BNOT ,num))

(defmacro op-dpb (newbyte pos mask integer)
  `(%:%op ,(vector (vector "DPB" pos mask))
          ,newbyte ,integer))

(defun logand (&rest integers)
  (do ((ret -1)
       (p integers (cdr p)))
      ((not p) ret)
    (setq ret (op-and ret (car p)))))

(define-compiler-macro logand (&rest integers)
  (cond
    ((null integers) -1)
    ((null (cdr integers)) (car integers))
    ((and (integerp (car integers))
          (integerp (cadr integers)))
     `(logand ,(op-and (car integers) (cadr integers))
              ,@(cddr integers)))
    (t
     `(logand (op-and ,(car integers) ,(cadr integers))
              ,@(cddr integers)))))

(defun logior (&rest integers)
  (do ((ret 0)
       (p integers (cdr p)))
      ((not p) ret)
    (setq ret (op-ior ret (car p)))))

(define-compiler-macro logior (&rest integers)
  (cond
    ((null integers) 0)
    ((null (cdr integers)) (car integers))
    ((and (integerp (car integers))
          (integerp (cadr integers)))
     `(logior ,(op-ior (car integers) (cadr integers))
              ,@(cddr integers)))
    (t
     `(logior (op-ior ,(car integers) ,(cadr integers))
              ,@(cddr integers)))))

(defun logxor (&rest integers)
  (do ((ret 0)
       (p integers (cdr p)))
      ((not p) ret)
    (setq ret (op-xor ret (car p)))))

(define-compiler-macro logxor (&rest integers)
  (let ((len (length integers)))
    (cond
      ((null integers) 0)
      ((null (cdr integers)) (car integers))
      ((and (integerp (car integers))
            (integerp (cadr integers)))
       `(logxor ,(op-xor (car integers) (cadr integers))
                ,@(cddr integers)))
      (t
       `(logxor (op-xor ,(car integers) ,(cadr integers))
                ,@(cddr integers))))))

(defun logtest (integer1 integer2)
  (/= 0 (op-and integer1 integer2)))

(define-compiler-macro logtest (integer1 integer2)
  `(/= 0 (op-and ,integer1 ,integer2)))

(defun logcount (integer)
  (op-cnt integer))

(define-compiler-macro logcount (integer)
  (cond
    ((integerp integer)
     (op-cnt integer))
    (t
     `(op-cnt ,integer))))

(defun lognot (integer)
  (op-not integer))

(define-compiler-macro lognot (integer)
  (cond
    ((integerp integer)
     (op-not integer))
    (t
     `(op-not ,integer))))

(defun ash (integer count)
  (op-ash integer count))

(define-compiler-macro ash (integer count)
  (cond
    ((and (integerp integer)
          (integerp count))
     (op-ash integer count))
    ((eql count 0)
     integer)
    (t
     `(op-ash ,integer ,count))))

(defun ldb (bytespec integer)
  (let* ((size (byte-size bytespec))
         (pos (byte-position bytespec)))
    (logand (ash integer (- pos)) (1- (ash 1 size)))))

(define-compiler-macro ldb (&whole form bytespec integer)
  (cond
    ((with-constant-bytespec (bytespec size pos)
       `(logand (ash ,integer ,(- pos)) ,(1- (ash 1 size)))))
    (t form)))

(defun ldb-test (bytespec integer)
  (let* ((size (byte-size bytespec))
         (pos (byte-position bytespec))
         (mask (1- (ash 1 size))))
    (/= 0 (logand integer (ash mask pos)))))

(define-compiler-macro ldb-test (&whole form bytespec integer)
  (cond
    ((with-constant-bytespec (bytespec size pos)
       (let ((mask (1- (ash 1 size))))
         `(/= 0 (logand ,integer ,(ash mask pos))))))
    (t form)))

(defun dpb (newbyte bytespec integer)
  (let ((pos (byte-position bytespec))
        (mask (1- (ash 1 (byte-size bytespec)))))
    (logior (logand integer (lognot (ash mask pos)))
            (ash (logand newbyte mask) pos))))

(define-compiler-macro dpb (&whole form newbyte bytespec integer)
  (cond
    ((with-constant-bytespec (bytespec size pos)
       (let ((mask (1- (ash 1 size))))
         (cond
           ((integerp newbyte)
            (let ((dep (ash (logand newbyte mask) pos)))
              (if (= (logcount dep) size)
                  `(logior ,integer ,dep)
                  `(op-dpb ,newbyte ,pos ,mask ,integer))))
           (t
            `(op-dpb ,newbyte ,pos ,mask ,integer))))))
    (t form)))

(define-setf-expander ldb (bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion int)   ;Get setf expansion for int.
    (if (cdr stores) (error "Can't expand this."))
    (cond
      ((constant-bytespec-p bytespec)
       (let ((store (gensym))
             (stemp (first stores)))
         (values temps
                 vals
                 (list store)
                 `(let ((,stemp (dpb ,store ,bytespec ,access-form)))
                    ,store-form
                    ,store)
                 `(ldb ,bytespec ,access-form))))
      (t
       ;; this is taken straight from the spec.
       ;; https://novaspec.org/cl/f_define-setf-expander
       (let ((btemp (gensym))     ;Temp var for byte specifier.
             (store (gensym))     ;Temp var for byte to store.
             (stemp (first stores))) ;Temp var for int to store.
         ;;; Return the setf expansion for LDB as five values.
         (values (cons btemp temps)       ;Temporary variables.
                 (cons bytespec vals)     ;Value forms.
                 (list store)             ;Store variables.
                 `(let ((,stemp (dpb ,store ,btemp ,access-form)))
                    ,store-form
                    ,store)               ;Storing form.
                 `(ldb ,btemp ,access-form) ;Accessing form.
                 ))))))

(defun integer-length (integer)
  (values (ceiling (log (if (minusp integer)
                            (- integer)
                            (1+ integer))
                        2))))
