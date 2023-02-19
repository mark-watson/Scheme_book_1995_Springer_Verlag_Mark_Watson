;; File: graph.s
;;
;; Description: This file module contains a library for
;;              plotting graph data structures.
;;
;; Public interface functions:
;;
;;   (open-gr) - initializes a new graphics window
;;
;;   (close-gr) - closes a graphics window
;;
;;   (clear-plot) - clears the graphics area
;;
;;   (plot-line x1 y1 x2 y2)
;;
;;   (plot-string x y str)
;;
;;   (plot-fill-rect x y xsize ysize color-string)
;;
;;   (plot-fill-rect-gray-scale x y xsize ysize int255)
;;
;;   (plot-ellipse left top right bottom color-string)
;;
;;   (pen-width width)
;;
;;
;;  Copyright 1995, Mark Watson. All source code rights
;;  reserved: you may not redistribute this source file.
;;  No binary rights reserved: you may use this softeware
;;  in compiled form without restriction.
;;

(define g-c #f)

(define open-gr
 (lambda ()
  (if (null? g-c)
      (begin
        (set! g-c (make-graphics-device #f))
        (graphics-set-coordinate-limits g-c 0 0 1024 1024))
      (begin
        (clear-plot)))))

(define close-gr
 (lambda ()
  (clear-plot)))

(define clear-plot
 (lambda ()
  (graphics-clear g-c)))

(define plot-line
 (lambda (x1 y1 x2 y2)
  (graphics-draw-line g-c x1 y1 x2 y2)))

(define plot-string
 (lambda (x y str)
  (graphics-draw-text
   g-c
   (inexact->exact (floor x))
   (inexact->exact (floor y))
   str)))

(define plot-ellipse
 (lambda (left top right bottom color-string)
  (graphics-operation g-c 'set-foreground-color color-string)
  (graphics-operation g-c 'draw-ellipse left top right bottom)
  (graphics-operation g-c 'set-foreground-color "black")))

(define graph_temp_vec (vector 0 0 1 1 2 2 3 3 4 4))

(define plot-fill-rect
 (lambda (x y xsize ysize color-string)
  (graphics-operation g-c 'set-foreground-color color-string)
  (let ((x2 (+ x xsize))
        (y2 (+ y ysize)))
    (vector-set! graph_temp_vec 0 x)
    (vector-set! graph_temp_vec 1 y)
    (vector-set! graph_temp_vec 2 x)
    (vector-set! graph_temp_vec 3 y2)
    (vector-set! graph_temp_vec 4 x2)
    (vector-set! graph_temp_vec 5 y2)
    (vector-set! graph_temp_vec 6 x2)
    (vector-set! graph_temp_vec 7 y)
    (vector-set! graph_temp_vec 8 x)
    (vector-set! graph_temp_vec 9 y)
    (graphics-operation g-c 'fill-polygon graph_temp_vec))
  (graphics-operation g-c 'set-foreground-color "black")))

(define graph_temp_vec_3 (vector 0 0 0))

(define plot-fill-rect-gray-scale
 (lambda (x y xsize ysize int255)
  (let ((color (inexact->exact (floor int255))))
    (vector-set! graph_temp_vec_3 0 color)
    (vector-set! graph_temp_vec_3 1 color)
    (vector-set! graph_temp_vec_3 2 color)
    (plot-fill-rect
     (inexact->exact x)
     (inexact->exact y) xsize ysize graph_temp_vec_3))))

(define pen-width
 (lambda (w)
  (graphics-operation g-c 'set-line-width w)
  1)
)
  
(define test
 (lambda ()
  (plot-fill-rect 200 100 300 300 "blue")
  (let ((xx '(0 25 50 75 100 125 150 175 200 225 250)))
    (do ((x xx (cdr x)))
        ((null? x))
      (plot-fill-rect-gray-scale
         (car x) (car x) 25 25 (car x))))
  (plot-line 500 400 300 600)
  (plot-ellipse 50 200 100 120 "red")
  (plot-string 50 800 
               "This is a test of the Scheme Graphics Portability Library")))


; (open-gr)
; (test)
; (close-gr)

