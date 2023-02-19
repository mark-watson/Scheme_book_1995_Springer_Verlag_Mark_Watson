;; File: gen_exam.s
 ;
 ; Description: This file contains the example program
 ;              described in section 3.5.  We want to
 ;              set the sales/marketing strategy for a
 ;              company with several sales staff.  In
 ;              particular, we want to adjust the fraction
 ;              of time each sales person spends on the
 ;              following activities:
 ;
 ;                1. Telephone sales calls
 ;                2. Personal sales calls to customer sites
 ;                3. Preparation and mailing of sales letters
 ;
 ;              The fitness function for each sales person
 ;              will simply be the dollar amount of sales
 ;              in the current period, normalized by the
 ;              sales of the other sales staff (this will
 ;              help reduce seasonal effects on evaluating
 ;              sales productivity).
 ;
 ;  Copyright 1995, Mark Watson.
 ;
 ;  This software may be used without restriction in compiled
 ;  form.  All source code rights reserved.
 ;;

; We will use a global variable to hold the current sales
; productions.  Usually we avoid global variables, but it
; does make it easier for our genetic algorithm fitness
; function to access sales figures.

(define Company-X-sales #f)

; Define global genetic algorithm data object.  We want
; to encode three weighting factors in each chromosome; we
; will use three genes per weighting factor (range 0 through 7),
; requiring 9 genes total per chromosome:

(define ge #f)

; Define the average sales productivity:

(define average-sales 0.0)

;;  Define the fitness function:

(define (fitness-1 a-chrom chromosome-index)
  (- (vector-ref Company-X-sales chromosome-index) average-sales))

(define (make-sales-data number-of-sales-staff)
  (set!
    ge
    (create-genetic-experiment 
       9 number-of-sales-staff fitness-1))
  (set! Company-X-sales 
        (make-vector number-of-sales-staff 0.0)))

(define (update-sales sales-list)
  (if (vector? sales-list)
     (set! Company-X-sales (vector-copy sales-list))
     (set! Company-X-sales (list->vector sales-list))))


(define (update-sales-strategy new-sales-list)
  (update-sales new-sales-list)
  (set! average-sales 0.0)
  (do ((i 0 (+ i 1)))
      ((> i (- (list-ref ge 2)))) ;; (list-ref ge 2) == # sales people
    (set! average-sales
          (+ average-sales (list-ref new-sales-list i))))
  (set! average-sales (/ average-sales (list-ref ge 2)))
  (update-population ge)
  ;; display best chromosome:
  (display (best-chromosome-and-fitness ge)))
    
;; Test code:

(make-sales-data 10)    ;; 10 sales staff
(update-sales-strategy '(100 101 120 90 120 99 78 102 121 105))
