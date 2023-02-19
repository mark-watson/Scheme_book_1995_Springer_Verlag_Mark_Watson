;;
 ; File: May2.s
 ;
 ; Generates a magnified bifurcation diagram magnifying the area
 ; where period doubling turns into chaos. 
 ;
 ; Copyright 1990, 1995 by Mark Watson
 ;;

(define bifur
  (lambda ()
    (open-gr)
    (clear-plot)
    (let ((x-axis 0)
	  (delta-value 0)
	  (x 0.1)
	  (population 0))
      (do ((y-axis 0 (+ y-axis 1)))
	  ((> y-axis 219))
	(set! delta-value (* 4 (+ 0.85 (/ y-axis 1500.0)))) ; change the growth rate
	(do ((iter 0 (+ iter 1)))
	    ((> iter 197))
	  (set! population (* delta-value x (- 1 x)))
	  (set! x-axis (truncate (* population 500.02)))
	  (if (and  (> x-axis 0)   (< x-axis 501))
	      (plot-line
	       (* 2 x-axis)
	       (- 1000 (* 2 y-axis))
	       (* 2 x-axis)
	       (- 1000 (* 2 y-axis))))
	  (set! x population))))))
