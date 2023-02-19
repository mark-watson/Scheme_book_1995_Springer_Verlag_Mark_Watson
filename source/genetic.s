;; File: genetic.scm
;;
;; Description: This file module contains a library for 
;;              genetic algorithm experiments, and two test
;;              functions at the end of the file.
;;
;; Public interface fucntions:
;;
;;  create-genetic-experiment
;;     This function creates a new genetic experiment data
;;     structure for a specified chromosome size, a
;;     specified population size, and a specified fitness
;;     function.
;;
;;  update-population
;;     This function sorts a population in descending order
;;     of fitness value, discards the half of the population
;;     with the lowest fitness, and uses cross over and
;;     mutation to replace the removed chromosomes.
;;
;;  best-chromosome-and-fitness
;;     This function returns a list containing the best
;;     chromosome in the population and its fitness value.
;;
;;  Copyright 1995, Mark Watson. All source code rights
;;  reserved: you may not redistribute this source file.
;;  No binary rights reserved: you may use this softeware
;;  in compiled form without restriction.
;;

;; Define a global DEBUG flag:

(define DEBUG #f)

(set! DEBUG #t)  ;; set to #f for no debug printout

(define create-genetic-experiment 
  (lambda (chromosome-size population-size a-fitness-function)
    (let ((the-population (make-vector population-size))
	  (vec #f))
      (do ((i 0 (+ i 1)))
	  ((equal? i population-size) the-population)
	(set! vec (make-vector chromosome-size 0))
	;; We want to set about one third of the genes to
	;; the value of one:
	(do ((j 0 (+ j 1)))
	    ((equal? j chromosome-size))
	  (if (> (random 10) 6)
	      (vector-set! vec j 1)))
	;; Store the chromosome vector in the population vector:
	(vector-set! the-population i vec))
      ;; Build the list that is the return value of this function:
      (list
       a-fitness-function
       chromosome-size
       population-size
       the-population
       (make-vector population-size -9999) ;; initial fitness values
       )
      )
    )
  )

(define update-population
  (let ((gene-ref vector-ref)
	(gene-set! vector-set!))
    (lambda (experiment)
      (apply
       (lambda (fitness-function 
		chromosome-size
		population-size
		population
		fitness-values)
	 (let ((chromosome-ref (lambda (c) (vector-ref population c)))
	       (chromosome-set! (lambda (c value)
				  (vector-set!
				   population
				   c
				   value)))
	       (fitness-ref (lambda (f) (vector-ref fitness-values f)))
	       (fitness-set! (lambda (f value)
			       (vector-set!
				fitness-values
				f
				value))))
	   
	   (letrec
	       ((set-fitness-values
		 ;; Iterate through all chromosomes in the population
		 ;; updating the fitness value in the fitness value
		 ;; vector for each chromosome.
		 
		 ;; NOTE: we make this a local function, so we can reset
		 ;;       the fitness values before returning from the
		 ;;       global function 'update-population:
		 
		 (lambda ()
		   (do ((c 0 (+ c 1)))
		       ((equal? c population-size))
		     (vector-set!
		      fitness-values
		      c
		      (fitness-function (vector-ref population c) c)))))
		
		(bubble-sort
		 (lambda ()
		   ;; We need to sort the fitness-values vector in descending
		   ;; order. As we move fitness values in the fitness-value
		   ;; vector, we move the corresponding chromosome in the
		   ;; population vector.
		   
		   ;; We will want to also sort the population in order
		   ;; of decreasing fitness value at the end of this function
		   ;; so we will make the "bubble sort" into a local function
		   ;; which we will use twice:
		   
		   (do ((i 0 (+ i 1)))
		       ((equal? i population-size))
		     (do ((j (- population-size 2) (- j 1)))
			 ((< j i))
		       (if (<
			    (vector-ref fitness-values j)
			    (vector-ref fitness-values (+ j 1)))
			   (let ((x (vector-ref fitness-values j))
				 ;; Note: we need to make a copy of one
				 ;;       of the chromosomes that we are
				 ;; going to modify:
				 (c (vector-ref population j)))
			     ;; Set the values of the fitness value and
			     ;; chromosome at index equal to 'j' to the
			     ;; values at index equal to 'j + 1':
			     (vector-set!
			      fitness-values
			      j
			      (vector-ref fitness-values (+ j 1)))
			     (vector-set!
			      population
			      j
			      (vector-ref population (+ j 1)))
			     
			     ;; Set the values of the fitness value and
			     ;; chromosome at index equal to 'j + 1' to the
			     ;; values at index equal to 'j':
			     (vector-set!
			      fitness-values
			      (+ j 1)
			      x)
			     (vector-set!
			      population
			      (+ j 1)
			      c))))))) ;; best chromosomes now are
		
		(cross-over
		 ;; Local function to do a "cross over" of genetic material:
		 (lambda (index-1 index-2)
		   (if DEBUG
		       (begin
			 (display "crossover at indicies: ")
			 (display index-1)
			 (display ", ")
			 (display index-2)
			 (display ": cross over allele is ")))
		   
		   (let ((allele (+ 2 (random (- chromosome-size 2))))
			 (chrom-1 (vector-copy (vector-ref population index-1)))
			 (chrom-2 (vector-ref population index-2)))
		     (if DEBUG
			 (begin
			   (display allele)
			   (newline)))
		     (do ((i 0 (+ i 1)))
			 ((equal? i allele))
		       (let ((gene-temp (vector-ref chrom-1 i)))
			 (vector-set! chrom-1 i (vector-ref chrom-2 i))
			 (vector-set! chrom-2 i gene-temp)))))))
	     
	     ;; Use the local function to initialize the fitness values:
	     (set-fitness-values)
	     
	     ;; at the lowest indices
	     (bubble-sort)
	     
	     ;; Perform a few randow cross over gene swaps:
	     ;; Note: we will leave the best chromosome at
	     ;; index 0 unchanged.
	     
	     (do ((i 0 (+ i 1)))
		 ((equal? i (truncate (/ population-size 3))))
	       (let ((index-1
		      (+ 1 (random (- population-size 1))))
		     (index-2
		      (+ 1 (random (- population-size 1)))))
		 (cross-over index-1 index-2)))
	     
	     ;; Perform a few random gene mutations:
	     
	     (let ((num-mutations
		    (truncate 
		     (/ (* population-size chromosome-size) 30))))
	       (do ((i 0 (+ i 1)))
		   ((equal? i num-mutations))
		 (let ((chrom-index ;; skip best chrom at index 0
			(+ 1 (random (- population-size 1))))
		       (gene (random chromosome-size)))
		   (let ((chromosome
			  (vector-ref population chrom-index)))
		     (if DEBUG
			 (begin
			   (display "mutating chromosome at index")
			   (display chrom-index)
			   (display ", value: ")
			   (display chromosome)
			   (display " at gene index ")
			   (display gene)
			   (newline)))
		     (vector-set!
		      chromosome
		      gene
		      (- 1 (vector-ref chromosome gene)))))))
	     
	     ;; We will now copy the best half of the chromosome
	     ;; population into the worst half:
	     (let ((num-copy (truncate (/ population-size 2)))
		   (best-index 0)
		   (worst-index (- population-size 1)))
	       (do ((j 0 (+ j 1)))
		   ((equal? j num-copy))
		 (vector-set!
		  fitness-values
		  worst-index
		  (vector-ref fitness-values best-index))
		 (vector-set!
		  population
		  worst-index
		  (vector-copy (vector-ref population best-index)))
		 (set! best-index (+ best-index 1))
		 (set! worst-index (- worst-index 1))))
	     
	     ;; Update all of the fitness values at once after doing
	     ;; the cross over and mutation operations:
	     (set-fitness-values)
	     
	     ;; we bubble sort the chromosomes a second time
	     ;; so that the application using this library
	     ;; can inspect the chromosomes in order.  It would
	     ;; however be more efficient not to sort a second time.
	     (bubble-sort))))
       experiment))))


;; Utility to return a list containing both the best
;; chromosome its fitness value:

;; NOTE: usually, the best chromosome is at index 0 in
;;       the population. The following function searches
;;       all chromosomes in case an application using this
;;       library creates a genetic experiment, and calls
;;       this function without first calling 'update-population'.

(define best-chromosome-and-fitness
  (lambda (experiment)
    (let ((fitness-function (car experiment))
	  (chromosome-size (cadr experiment))
	  (population-size (caddr experiment))
	  (population (cadddr experiment))
	  (fitness-values (cadddr (cdr experiment)))
	  (best-chromosome)
	  (best-fitness-value)
	  (best-index))
      ;; Iterate through all chromosomes in the population
      ;; finding the chromosome with the best fitness value:
      (set! best-index 0)
      (set!
       best-fitness-value
       (vector-ref fitness-values best-index))
      ;; start at the second chromosome (index 1):
      (do ((c 1 (+ c 1))) 
	  ((equal? c population-size))
	(if (> 
	     (vector-ref fitness-values c)
	     best-fitness-value)
	    (begin
	      (set! best-index c)
	      (set! 
	       best-fitness-value
	       (vector-ref fitness-values c)))))
      (set! best-chromosome (vector-ref population best-index))
      (list
       best-chromosome
       (fitness-function best-chromosome best-index)))))


;;
;;;;;;;;;;;;;;;;;;;;;;;;;; TEST FUNCTIONS:
;;

;;  Define a test fitness function:

(define fitness-1
  (lambda (a-chrom chromosome-index)
    (let ((sum 0)
	  (vector-len (vector-length a-chrom)))
      (do ((i 0 (+ i 1)))
	  ((= i vector-len) sum)
	(if (equal? (vector-ref a-chrom i) 1)
	    (set! sum (+ sum 1)))))))

;; Test function

(define (test)
  (let ((ge ;; experiment with 10 chromosomes, each with 6 genes
         (create-genetic-experiment 6 10 fitness-1)))
    (do ((i 0 (+ i 1)))
        ((equal? i 20))  ;; 20 "generations"
      (update-population ge)
      (let ((temp (best-chromosome-and-fitness ge)))
        (newline)
        (display "best fitness: ")
        (display (cadr temp))
        (display ", and best chromosome: ")
        (display (car temp))
        (newline)
        (display ge)
        (newline)))))

;; Evaluate the following to test the system:

; (test)