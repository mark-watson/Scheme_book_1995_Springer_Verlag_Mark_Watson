;; File: GO_PLOT.S
 ;
 ; Description: This file module contains optional plotting routines
 ;              for displaying the current Go board.
 ;
 ; File Module Dependencies: load GO_INIT.S, GO_DATA.S, and GO_PLAY.S
 ;                           before loading this file.
 ;
 ;                           The plotting library GRAPH.S
 ;
 ;;

(load "GRAPH.S")  ;; just in case...

;; The following global variable is used to see if we need to
;; initialize the plotting window.  Usually, we try to avoid using
;; global variables.

(define GO_PLOT_NEED_TO_INITIALIZE_GRAPHICS_RIGHT_NOW #t)

(define init-Go-plot
  (lambda ()
    (set! GO_PLOT_NEED_TO_INITIALIZE_GRAPHICS_RIGHT_NOW #f)
    (open-gr)))

(define plot-board
  (lambda (game)
    
    (define plot-solid-ellipse
      (lambda (left top right bottom color)
	     (plot-ellipse  left top right bottom color)
	     (if (and
		  (>= right left)
		  (>= top bottom))
		 (plot-solid-ellipse
		  (+ left 2)
		  (- top 2)
		  (- right 2)
		  (+ bottom 2)
		  color))))
    
    (if GO_PLOT_NEED_TO_INITIALIZE_GRAPHICS_RIGHT_NOW
	(init-Go-plot))
    
    (let* ((size (game-size game))
	   (del-x (inexact->exact (/ 800 size)))
	   (del-y (inexact->exact (/ 900 size)))
	   (x-start 150)
	   (y-start 150)
	   (board (game-board game)))
      
      (clear-plot)
      
      (do ((i 0 (+ i 1)))
	  ((>= i size))
	(plot-line (+ x-start (* i del-x))
		   y-start
		   (+ x-start (* i del-x))
		   (+ y-start (* (- size 1) del-y)))
	(plot-line x-start
		   (+ y-start (* i del-y))
		   (+ x-start (* (- size  1) del-x))
		   (+ y-start (* i del-y)))
	(plot-string 
	 (+ x-start -4 (* i del-x))
	 (- y-start 55)
	 (list-ref '("A" "B" "C" "D" "E" "F" "G" "H" "I"
			 "J" "K" "L" "M" "N" "O" "P" "R" "S" "T")
		   i))
	(plot-string 
	 (- x-start 80)
	 (+ y-start -5 (* i del-y))
	 (list-ref '("1" "2" "3" "4" "5" "6" "7" "8" "9" "10"
			 "11" "12" "13" "14" "15" "16" "17" "18" "19")
		   i))
	
	(do ((row 0 (+ row 1)))
	    ((>= row size))
	  (do ((col 0 (+ col 1)))
	      ((>= col size))
	    (if (equal?
		 (board-ref board row col)
		 HUMAN)
		(plot-solid-ellipse 
		 (- (+ x-start (* col del-x)) (/ del-x 2))
		 (+ (+ y-start (* row del-y)) (/ del-x 2))
		 (+ (+ x-start (* col del-x)) (/ del-x 2))
		 (- (+ y-start (* row del-y)) (/ del-x 2))
		 "gray"))
	    (if (equal?
		 (board-ref board row col)
		 COMPUTER)
		(plot-solid-ellipse 
		 (- (+ x-start (* col del-x)) (/ del-x 2))
		 (+ (+ y-start (* row del-y)) (/ del-x 2))
		 (+ (+ x-start (* col del-x)) (/ del-x 2))
		 (- (+ y-start (* row del-y)) (/ del-x 2))
		 "black"))))))))
