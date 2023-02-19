;;
 ; File: complex.s
 ;
 ; Description: This file contains a complex number
 ;              package that simulates the following
 ;              Common LISP functions:
 ;
 ;                     (complex <real-part> <imaginary-part>)
 ;                         ==> a list
 ;                     (realpart <a list of 2 numbers>)
 ;                         ==> the first number in the list
 ;                     (imagpart <a list of 2 numbers>)
 ;                         ==> the second number in the list
 ;                     (complex_+ <2 number list> <2 number list>)
 ;                         ==> 
 ;                     (complex_* <2 number list> <2 number list>)
 ;                         ==> 

(define complex 
  (lambda (x y)
    (list x y)))

(define realpart
  (lambda (z)
    (car z)))

(define imagpart
  (lambda (z)
    (cadr z)))

(define complex_+
  (lambda (z1 z2)
    (list (+ (realpart z1) (realpart z2))
	  (+ (imagpart z1) (imagpart z2)))))

(define complex_*
  (lambda (z1 z2)
    (list
     (-
      (* (realpart z1) (realpart z2))
      (* (imagpart z1) (imagpart z2)))
     (+
      (* (realpart z1) (imagpart z2))
      (* (realpart z2) (imagpart z1))))))
