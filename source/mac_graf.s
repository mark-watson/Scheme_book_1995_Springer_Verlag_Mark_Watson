;; File: GRAPH.SCM
 ;
 ; Description: This file contains an implementation of
 ;              the protable Scheme graphics library for
 ;              for the Macintosh MacGambit Scheme system.
 ;;


(define local-window #f)

(define Y-MAX-VALUE 250)
(define PLOT-SCALE 1)

(define (open-gr)
  (if (equal? local-window #f)
    (set! local-window 
          (mac#newwindow
           (mac#rect 40 10 (+ Y-MAX-VALUE 100) 300)
           "Portable Scheme Graphics"
           #t    ; visible
           4     ; nogrowdoc
           -1    ; in front of all windows
           #t)) ; goawaybox
    (clear-plot)))

(define (clear-plot)
  (mac#eraserect local-window (mac#rect 0 0 512 512)))

(define (set-color color-string)
 #f) ;; color support is (apparently) not available

(define (plot-line x1 y1 x2 y2)
  (set! x1 (/ x1 PLOT-SCALE))
  (set! y1 (/ y1 PLOT-SCALE))
  (set! x2 (/ x2 PLOT-SCALE))
  (set! y2 (/ y2 PLOT-SCALE))
  (mac#moveto local-window x1 (- Y-MAX-VALUE y1))
  (mac#lineto local-window x2 (- Y-MAX-VALUE y2)))

(define (plot-string x y str)
  (set! x (/ x PLOT-SCALE))
  (set! y (/ y PLOT-SCALE))
  (mac#moveto local-window x (- Y-MAX-VALUE y))
  (mac#drawstring local-window str))

(define (plot-fill-rect x y xsize ysize color-string)
  (set! x (/ x PLOT-SCALE))
  (set! y (/ y PLOT-SCALE))
  (set! xsize (/ xsize 2))
  (set! ysize (/ ysize 2))
  (set-color color-string)
  (mac#paintrect 
   local-window 
   (mac#rect (- Y-MAX-VALUE y) x (+ (- Y-MAX-VALUE y) ysize) (+ x xsize))))
  

(define (plot-ellipse left top right bottom color-string)
  (set! top (/ top PLOT-SCALE))
  (set! left (/ left PLOT-SCALE))
  (set! bottom (/ bottom PLOT-SCALE))
  (set! right (/ right PLOT-SCALE))
  (set-color color-string)
  (mac#paintoval 
   local-window 
   (mac#rect (- Y-MAX-VALUE top) left (- Y-MAX-VALUE bottom) right)))

(define (pen-width width)
  (mac#pensize local-window width width))

;; test function

(define (test)
  (open-gr)
  (plot-string 50 200 "Test of Portable Scheme graphics for MacGambit")
  (plot-line 10 10 100 200)
  (plot-fill-rect 50 100 330 220 "red")
  (pen-width 10)
  (plot-line 200 200 190 190)
  (pen-width 1)
  (plot-ellipse 150 150 200 100 "blue"))

