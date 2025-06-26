(in-package :sl-user)

;; https://rosettacode.org/wiki/Sieve_of_Eratosthenes#Common_Lisp

(defun sieve-of-eratosthenes (maximum)
  (loop with sieve = (make-array (1+ maximum) :initial-element 0)
        for candidate from 2 to maximum
        when (zerop (aref sieve candidate))
        collect candidate
        and do (loop for composite from (expt candidate 2)
                     to maximum by candidate
                     do (setf (aref sieve composite) 1))))

(defun sieve-odds (maximum)
  "Prime numbers sieve for odd numbers.
   Returns a list with all the primes that are less than or equal to maximum."
  (loop :with maxi = (floor (/ (1- maximum) 2))
        :with stop = (floor (/ (sqrt maximum) 2))
        :with sieve = (make-array (1+ maxi) :initial-element 0)
        :for i :from 1 :to maxi
        :for odd-number = (1+ (* i 2))
        :when (zerop (aref sieve i))
        :collect odd-number :into values
        :when (<= i stop)
        :do (loop :for j :from (* i (1+ i) 2) :to maxi :by odd-number
                  :do (setf (aref sieve j) 1))
        :finally (return (cons 2 values))))