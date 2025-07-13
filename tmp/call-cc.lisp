(in-package :sl-user)

(defun call/cc (func)
  (funcall func (%::c/c)))

;; https://docs.scheme.org/tyscheme/index-Z-H-15.html#TAG:__tex2page_sec_13.3

(defun tree->generator (tree)
  (let ((caller nil)
        (generate-leaves nil))
    (setf generate-leaves
          (lambda ()
            (let looop ((tree tree))
              (cond
                ((consp tree)
                 (looop (car tree))
                 (when (cdr tree)
                   (looop (cdr tree))))
                (t
                 (call/cc
                  (lambda (rest-of-tree)
                    (setf generate-leaves
                          (lambda ()
                            (funcall rest-of-tree 'resume)))
                    (funcall caller tree))))))
            (funcall caller '())))
    (lambda ()
      (call/cc
       (lambda (k)
         (setf caller k)
         (funcall generate-leaves))))))

(defun same-fringe (tree1 tree2)
  (let ((gen1 (tree->generator tree1))
        (gen2 (tree->generator tree2)))
    (let looop ()
      (let ((leaf1 (funcall gen1))
            (leaf2 (funcall gen2)))
        (if (eq leaf1 leaf2)
            (if (null leaf1) t (looop))
            nil)))))

;; shift, reset, yield. https://lisperator.net/pltut/cps-evaluator/yield

(defparameter pstack nil)

(let ((v (call/cc (lambda (k)
                    (setf (symbol-function 'goto) k)
                    (funcall k nil)))))
  (when v
    (let ((r (funcall v)))
      (funcall (pop pstack) r))))

(defun reset (thunk)
  (call/cc (lambda (k)
             (push k pstack)
             (goto thunk))))

(defun shift (f)
  (call/cc
   (lambda (k)
     (goto (lambda ()
             (funcall f (lambda (&optional v)
                          (call/cc
                           (lambda (k1)
                             (push k1 pstack)
                             (funcall k v))))))))))

(defun call-with-yield (func)
  (let ((yield (lambda (val)
                 (shift (lambda (k)
                          (setf func k)
                          val)))))
    (lambda (&optional (val yield))
      (reset (lambda ()
               (funcall func val))))))

(defmacro defun/yield (name args &body body)
  (let ((func (gensym (symbol-name name))))
    `(macrolet ((yield (val)
                  `(shift (lambda (k)
                            (setf ,',func k)
                            ,val))))
       (let (,func)
         (setf ,func (%:%fn ,name ,args ,@body))
         (setf (symbol-function ',name)
               (lambda args
                 (reset (lambda ()
                          (apply ,func args)))))))))

(defun/yield fib ()
  (let rec ((a 1)
            (b 1))
    (yield b)
    (rec b (+ a b))))

