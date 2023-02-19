;;
 ; File: May1.s
 ;
 ; Population growth model of Robert May: generates a bifurcation diagram
 ; showing large scale effect of the population growth rate.
 ; Note: the area to the upper right of the initial curve indicates
 ; extinction due to the rate of population growth being too small
 ; (the variable lambda).
 ;
 ;  Copyright 1990, 1995 by Mark Watson
 ;;


(define bifur
  (lambda ()
    (open-gr)
    (clear-plot)
    (plot-string 500 920 "Extinction")
    (plot-string 32 890 "Steady state")
    (plot-string 660 700 "Period doubled")
    (plot-string 400 500 "Chaos")
    (let ((x-axis 0)
	  (delta-value 0)
	  (x 0.1)
	  (population 0))
      (do ((y-axis 0 (+ y-axis 1)))
	  ((> y-axis 219))
	(set! delta-value (* 4 (+ 0.20 (/ y-axis 250.0))))
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