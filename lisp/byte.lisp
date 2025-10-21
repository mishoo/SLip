(in-package :sl)

(export '(ash byte byte-size byte-position ldb dpb logand logandc1 logandc2
          logeqv logior lognand lognor lognot logorc1 logorc2 logxor logtest
          logcount))

(defpackage :sl-byte
  (:use :sl :%))

(in-package :sl-byte)

(defun byte (size position)
  (cons size position))

(define-compiler-macro byte (&whole form size position)
  (cond
    ((and (integerp size)
          (integerp position))
     `'(,size . ,position))
    (t form)))

(defun byte-size (byte)
  (car byte))

(define-compiler-macro byte-size (byte)
  `(car ,byte))

(defun byte-position (byte)
  (cdr byte))

(define-compiler-macro byte-position (byte)
  `(cdr ,byte))

(define-compiler-macro op-and (b1 b2)
  `(%:%op BAND ,b1 ,b2))

(define-compiler-macro op-ior (b1 b2)
  `(%:%op BOR ,b1 ,b2))

(define-compiler-macro op-xor (b1 b2)
  `(%:%op BXOR ,b1 ,b2))

(define-compiler-macro op-ash (num count)
  `(%:%op BASH ,num ,count))

(define-compiler-macro op-cnt (num)
  `(%:%op BCNT ,num))

(defun logand (&rest integers)
  (do ((ret -1)
       (p integers (cdr p)))
      ((not p) ret)
    (setq ret (op-and ret (car p)))))

(define-compiler-macro logand (&rest integers)
  (let ((len (length integers)))
    (cond
      ((zerop len) -1)
      ((= len 1) (car integers))
      ((>= len 2)
       (cond
         ((and (integerp (car integers))
               (integerp (cadr integers)))
          `(logand ,(op-and (car integers) (cadr integers))
                   ,@(cddr integers)))
         (t
          `(logand (op-and ,(car integers) ,(cadr integers))
                   ,@(cddr integers))))))))

(defun logior (&rest integers)
  (do ((ret 0)
       (p integers (cdr p)))
      ((not p) ret)
    (setq ret (op-ior ret (car p)))))

(define-compiler-macro logior (&rest integers)
  (let ((len (length integers)))
    (cond
      ((zerop len) -1)
      ((= len 1) (car integers))
      ((>= len 2)
       (cond
         ((and (integerp (car integers))
               (integerp (cadr integers)))
          `(logior ,(op-ior (car integers) (cadr integers))
                   ,@(cddr integers)))
         (t
          `(logior (op-ior ,(car integers) ,(cadr integers))
                   ,@(cddr integers))))))))

(defun logxor (&rest integers)
  (do ((ret 0)
       (p integers (cdr p)))
      ((not p) ret)
    (setq ret (op-xor ret (car p)))))

(define-compiler-macro logxor (&rest integers)
  (let ((len (length integers)))
    (cond
      ((zerop len) -1)
      ((= len 1) (car integers))
      ((>= len 2)
       (cond
         ((and (integerp (car integers))
               (integerp (cadr integers)))
          `(logxor ,(op-xor (car integers) (cadr integers))
                   ,@(cddr integers)))
         (t
          `(logxor (op-xor ,(car integers) ,(cadr integers))
                   ,@(cddr integers))))))))

(defun logtest (integer1 integer2)
  (/= 0 (op-and integer1 integer2)))

(defun logcount (integer)
  (op-cnt integer))

(define-compiler-macro logcount (integer)
  (cond
    ((integerp integer)
     (op-cnt integer))
    (t
     `(op-cnt ,integer))))

(defun ash (integer count)
  (op-ash integer count))

(define-compiler-macro ash (integer count)
  (cond
    ((and (integerp integer)
          (integerp count))
     (op-ash integer count))
    (t
     `(op-ash ,integer ,count))))

(defun ldb (bytespec integer)
  (let* ((size (byte-size bytespec))
         (pos (byte-position bytespec))
         (mask (1- (ash 1 size))))
    (ash (logand integer (ash mask pos)) (- pos))))

(define-compiler-macro ldb (&whole form bytespec integer)
  (cond
    ((and (eq 'byte (car bytespec))
          (integerp (cadr bytespec))
          (integerp (caddr bytespec)))
     (let* ((size (cadr bytespec))
            (pos (caddr bytespec))
            (mask (1- (ash 1 size))))
       `(ash (logand ,integer ,(ash mask pos)) ,(- pos))))
    (t form)))

(defun dpb (newbyte bytespec integer)
  (let* ((size (byte-size bytespec))
         (pos (byte-position bytespec))
         (mask (1- (ash 1 size)))
         (tmask (ash mask pos)))
    (logior (logxor tmask (logior integer tmask))
            (ash (logand newbyte mask) pos))))

(define-compiler-macro dpb (&whole form newbyte bytespec integer)
  (cond
    ((and (eq 'byte (car bytespec))
          (integerp (cadr bytespec))
          (integerp (caddr bytespec)))
     (let* ((size (cadr bytespec))
            (pos (caddr bytespec))
            (mask (1- (ash 1 size)))
            (tmask (ash mask pos)))
       `(logior (logxor ,tmask (logior ,integer ,tmask))
                (ash (logand ,newbyte ,mask) ,pos))))
    (t form)))
