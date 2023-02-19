;; File: array.s
;;
;; Description: This file contains an implementation
;;              of two dimensional arrays
;;
;; Public functions:
;;
;;  (make-2D-array num-first-index num-second-index)
;;     --> a 2D array data structure
;;
;;  (2D-array-ref a-2D-array index-1 index-2)
;;     --> array value
;;
;;  (2D-array-set! a-2D-array index-1 index-2 new-value)
;;     --> previous value stored in array element
;;
;;  (2D-array-length a-2D-array dimension)
;;     --> returns the first dimension size if
;;         dimension argument equals 0 and returns
;;         the second dimension argument if the
;;         dimension argument equals 1
;;
;;  Copyright 1995, Mark Watson
;;

(define make-2D-array
  (lambda (num-1 num-2)
    (let ((data-size (* num-1 num-2))
	  (return-value #f))
      (set!
       return-value
       (make-vector (+ 2 data-size) 0.0))
      (vector-set! return-value 0 num-1)
      (vector-set! return-value 1 num-2)
      return-value)))

(define 2D-array-ref
  (lambda (an-array index-1 index-2)
    (let ((size-1 (vector-ref an-array 0))
	  (size-2 (vector-ref an-array 1)))
      (if (or
	   (< index-1 0)
	   (>= index-1 size-1))
	  (begin
	    (display "illegal first index for 2D-array:")
	    (display index-1)
	    (newline)
	    0)
	  (if (or 
	       (< index-2 0)
	       (>= index-2 size-2))
	      (begin
		(display "illegal first index for 2D-array:")
		(display index-2)
		(newline)
		0)
	      ;; indices are OK, proceed:
	      (vector-ref
	       an-array
	       (+
		2
		index-2
		(* index-1 size-2))))))))

(define 2D-array-set!
  (lambda (an-array index-1 index-2 new-value)
    (let ((size-1 (vector-ref an-array 0))
	  (size-2 (vector-ref an-array 1)))
      (if (or
	   (< index-1 0)
	   (>= index-1 size-1))
	  (begin
	    (display "illegal first index for 2D-array:")
	    (display index-1)
	    (newline)
	    0)
	  (if (or 
	       (< index-2 0)
	       (>= index-2 size-2))
	      (begin
		(display "illegal first index for 2D-array:")
		(display index-2)
		(newline)
		0)
	      ;; indices are OK, proceed:
	      (let ((old-value
		     (vector-ref
		      an-array
		      (+
		       2
		       index-2
		       (* index-1 size-2)))))
		(vector-set!
		 an-array
		 (+
		  2
		  index-2
		  (* index-1 size-2))
		 new-value)
		old-value))))))

(define 2D-array-length
  (lambda (a-2D-array dimension)
    (vector-ref a-2D-array dimension)))

;; Test code:

; (define a)
; (set! a (make-2D-array 3 4))
; (pp a)

(define test
  (lambda ()
    (do ((i 0 (+ i 1)))
	((equal? i 3))
      (do ((j 0 (+ j 1)))
	  ((equal? j 4))
	(2D-array-set! a i j (+ i (* j 1000)))))
    (do ((i 0 (+ i 1)))
	((equal? i 3))
      (do ((j 0 (+ j 1)))
	  ((equal? j 4))
	(display "a[")
	(display i)
	(display "][")
	(display j)
	(display "] = ")
	(display (2D-array-ref a i j))
	(newline)))))

; (test)
; (2D-array-ref a 4 5)

; (2D-array-length a 0)
; (2D-array-length a 1)
