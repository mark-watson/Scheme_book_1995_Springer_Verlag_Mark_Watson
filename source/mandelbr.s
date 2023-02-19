;;
 ; File: Mandelbr.s
 ;
 ; Calculate and plot the Mandelbrot set
 ; Copyright 1990, 1994 by Mark Watson
 ;
 ; Required file modules: graph.s and complex.s
 ;
 ;;

;;
 ; Define the maximum number of gray-scale or color values
 ; that function plot-fill-rect uses as a pattern (or color) range:
 ; (Plot-fill-rect is defined in the graphics library in Chapter 2.)
 ;;
(define MAX_COLORS 250)

;;
 ; Define the number of complex points to evaluate for Mandelbrot
 ; set membership: a rectangle of points num-x-cells by num-y-cells:
 ;;

(define num-x-cells 80)
(define num-y-cells 80)

;;
 ; Set the cell width and height for plotting a single point
 ; on the complex plane:
 ;;

(define cell-width 10)
(define delta-x-cell 0)
(define delta-y-cell 0)

(set! delta-x-cell (/ 3.2 num-x-cells))
(set! delta-y-cell (/ 3.2 num-y-cells))

;;
 ;  Function M  is called to calculate the Mandelbrot set
 ; for complex points around the complex number zero.
 ;;

(define debug #f)

(define M
 (lambda ()
  (open-gr) ;; open a graphics window
  (plot-string 100 900 "Mandelbrot Plot")
  (let ((x 0.5) (y 0.5) (z (complex 0 0)))
    (do ((ix 0 (+ ix 1)))
	((= ix num-x-cells))
      (set! x (- (* ix delta-x-cell) 2.0))
;;      (display "x =") (display x) (newline)
      (do ((iy 0 (+ iy 1)))
	  ((= iy num-y-cells))
	(set! y (- (* iy delta-y-cell) 1.6))
;;      (display "y =") (display y) (newline)
	(set! z (complex x y))
	(let ((index 0))
	  (do ((i 0 (+ i 1)))
	      ((not (< i MAX_COLORS)))
	    (set! z (complex_+ 
		     (complex x y)
		     (complex_* z z)))
	    (set! index i)
	    (if (>  (+  (* (realpart z) (realpart z))
			(* (imagpart z) (imagpart z))) 4)
	      (set! i 1000)))
	  (if debug
	      (begin
		(display "ix = ") (display ix) (newline)
		(display "iy = ") (display iy) (newline) (newline)))
	  (plot-fill-rect-gray-scale 
	   (+ 10 (* ix cell-width))
	   (+ 10 (* iy cell-width))
	   cell-width cell-width
           (- MAX_COLORS index))))))))
