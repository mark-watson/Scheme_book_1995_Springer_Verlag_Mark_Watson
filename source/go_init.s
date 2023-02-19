;; File: go_init.s
;
; Description: Building a Go library interactively in Scheme.
;              This file contains Scheme functions to initialize
;              the data structures for a new Go game.
;
; Copyright 1995, Mark Watson. All source code rights reserved.
; The contents of this file can be used in compiled form
; without restriction.
;
;;

(define COMPUTER 1)
(define HUMAN 2)

(define make-board
  (lambda  (size)
    (let ((v (make-vector size)))
      (do ((i 0 (+ i 1)))
	  ((> i (- size 1)))
	(vector-set! v i (make-vector size 0))) ;; we must loop (see text)
      v)))

(define board-ref
  (lambda (brd row column)
    (vector-ref (vector-ref brd row) column)))

(define board-set!
  (lambda (brd row column value)
    (vector-set! (vector-ref brd row) column value)))

;; make a new Go game object:

(define (make-Go-game size . number-of-handicap-stones)
  (let ((g (vector
	    size
	    ;; this is the actual board:
	    (make-board size) 
	    ;; this 2D array holds the group ID indices
	    ;; at each board location:
	    (make-board size) 
	    ;; this 2D array holds the current
	    ;; liberty count at each board location:
	    (make-board size) 
	    ;; this 2D array holds move values ranging
	    ;; from zero for "do not move here", to
	    ;; large positive integer values for good moves:
	    (make-board size)
	    ;; counter for creating new groups:
	    0)))
    (set-Joseki g) ;; set the good move table
    g))


(define copy-Go-object
  (lambda (old-Go-game)
    (let* ((size (game-size old-Go-game))
	   (new-Go-game (make-Go-Game size))
	   (old-board (game-board old-Go-game))
	   (new-board (game-board new-Go-game))
	   (old-groups (game-groups old-Go-game))
	   (new-groups (game-groups new-Go-game))
	   (old-liberties (game-liberties old-Go-game))
	   (new-liberties (game-liberties new-Go-game))
	   (old-move-values (game-move-values old-Go-game))
	   (new-move-values (game-move-values new-Go-game)))
      (game-group-counter-set! 
       new-Go-game
       (game-group-counter old-Go-Game))
      (do ((row 0 (+ row 1)))
	  ((>= row size))
	(do ((col 0 (+ col 1)))
	    ((>= col size))
	  (board-set! new-board row col (board-ref old-board row col))
	  (board-set! new-groups row col (board-ref old-groups row col))
	  (board-set! new-liberties row col (board-ref old-liberties row col))
	  (board-set! new-move-values row col (board-ref old-move-values row col))))
      new-Go-game)))

;; Utilities for accessing the data structures in a game object:

(define (game-size game)                     (vector-ref game 0))
(define (game-board game)                    (vector-ref game 1))
(define (game-groups game)                   (vector-ref game 2))
(define (game-liberties game)                (vector-ref game 3))
(define (game-move-values game)              (vector-ref game 4))
(define (game-group-counter game)            (vector-ref game 5))
(define (game-group-counter-set! game value) (vector-set! game 5 value))


(define set-Joseki
  (lambda (game)
    (let ((size (vector-ref game 0)))
      (if (equal? size 5)
	  (vector-set! 
	   game
	   4
	   (vector
	    (vector  0 0 0 0 0)
	    (vector  0 1 1 1 0)
	    (vector  0 1 4 1 0)
	    (vector  0 1 1 1 0)
	    (vector  0 0 0 0 0))))
      (if (equal? size 6)
	  (vector-set! 
	   game
	   4
	   (vector
	    (vector  0 0 0 0 0 0)
	    (vector  0 1 1 1 1 0)
	    (vector  0 1 4 4 1 0)
	    (vector  0 1 4 4 1 0)
	    (vector  0 1 1 1 1 0)
	    (vector  0 0 0 0 0 0))))
      (if (equal? size 7)
	  (vector-set! 
	   game
	   4
	   (vector
	    (vector  0 0 0 0 0 0 0)
	    (vector  0 1 1 0 1 1 0)
	    (vector  0 1 3 4 3 1 0)
	    (vector  0 0 4 3 4 0 0)
	    (vector  0 1 3 4 3 1 0)
	    (vector  0 1 1 0 1 1 0)
	    (vector  0 0 0 0 0 0 0))))
      (if (equal? size 8)
	  (vector-set! 
	   game
	   4
	   (vector
	    (vector  0 0 0 0 0 0 0 0)
	    (vector  0 1 0 0 0 0 1 0)
	    (vector  0 0 3 4 4 3 0 0)
	    (vector  0 2 4 4 4 4 2 0)
	    (vector  0 2 4 4 4 4 2 0)
	    (vector  0 0 3 4 4 3 0 0)
	    (vector  0 1 0 0 0 0 1 0)
	    (vector  0 0 0 0 0 0 0 0))))
      (if (equal? size 9)
	  (vector-set! 
	   game
	   4
	   (vector
	    (vector  0 0 0 0 0 0 0 0 0)
	    (vector  0 1 0 0 0 0 0 1 0)
	    (vector  0 0 3 4 2 4 3 0 0)
	    (vector  0 0 4 4 3 4 4 0 0)
	    (vector  0 0 3 2 2 2 3 0 0)
	    (vector  0 0 4 4 3 4 4 0 0)
	    (vector  0 0 3 4 2 4 3 0 0)
	    (vector  0 1 0 0 0 0 0 1 0)
	    (vector  0 0 0 0 0 0 0 0 0))))
      
      (if (equal? size 11)
	  (vector-set! 
	   game
	   4
	   (vector
	    (vector  0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 1 0 0 0 0 0)
	    (vector  0 0 3 3 0 0 0 0 3 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 2 0 0 0 0 0 2 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 2 0 0 0 0 3 0 0)
	    (vector  0 0 2 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0))))
      
      (if (equal? size 13)
	  (vector-set! 
	   game
	   4
	   (vector
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 3 0 2 0 0 0 3 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 2 0 0 0 0 0 2 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 3 0 0 2 0 0 3 0 0 0)
	    (vector  0 0 0 0 0 0 2 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0))))
      
      (if (equal? size 15)
	  (vector-set! 
	   game
	   4
	   (vector
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 2 0 0 0 0 0 0 0)
	    (vector  0 0 0 3 0 0 0 0 0 0 0 3 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 2 0 0 0 0 0 0 0 0 0 2 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 3 0 0 0 0 0 0 0 3 0 0 0)
	    (vector  0 0 0 0 0 0 0 2 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
      
      (if (equal? size 19)
	  (vector-set! 
	   game
	   4
	   (vector
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0)
	    (vector  0 0 0 3 0 0 0 0 0 2 0 0 0 0 0 3 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 3 0 0 0 0 0 2 0 0 0 0 0 3 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (vector  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))))))


