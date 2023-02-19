;; File: GO_STRAT.S
;
; Description: This file module contains functions that
;              perform the strategical analysis of a given
;              game situation, and add suggested strategic
;              moves in the move values array in the game
;              data structure.
;
; Copyright 1995, Mark Watson. All source code rights reserved.
; The contents of this file can be used in compiled form
; without restriction.
;
; File module dependencies:
;
;      Files GO_INIT.S, GO_DATA.S, and GO_PLAY.S must be loaded
;      before this file.
;
;;


;; Override the GO-debug function defined in file GO_PLAY.S.  This
;; function can be commented out after we have unit tested the
;; functions in this file module.

(define Go-debug
  (lambda (game)
    (pp game)
    (newline)))


(define strategic-update
  (lambda (game)
    (let* ((size (game-size game))
	   (human-influence (make-board size))
	   (computer-influence (make-board size))
	   (board (game-board game)))
      ;; Fill in both computer and human "influence" arrays:
      (do ((row 2 (+ row 1)))
	  ((>= row (- size 2)))
	(do ((col 2 (+ col 1)))
	    ((>= col (- size 2)))
	  
	  (if (equal?
	       (board-ref board row col)
	       HUMAN)
	      (begin
		(board-set! 
		 human-influence (- row 2) col
		 (+ 1 (board-ref human-influence (- row 2) col)))
		(board-set! 
		 human-influence (+ row 2) col
		 (+ 1 (board-ref human-influence (+ row 2) col)))
		(board-set! 
		 human-influence row (- col 2)
		 (+ 1 (board-ref human-influence row (- col 2))))
		(board-set! 
		 human-influence row (+ col 2)
		 (+ 1 (board-ref human-influence row (+ col 2))))
		(board-set! 
		 human-influence (- row 1) col
		 (+ 2 (board-ref human-influence (- row 1) col)))
		(board-set! 
		 human-influence (+ row 1) col
		 (+ 2 (board-ref human-influence (+ row 1) col)))
		(board-set! 
		 human-influence row (- col 1)
		 (+ 2 (board-ref human-influence row (- col 1))))
		(board-set! 
		 human-influence row (+ col 1)
		 (+ 2 (board-ref human-influence row (+ col 1))))))
	  
	  (if (equal?
	       (board-ref board row col)
	       COMPUTER)
	      (begin
		(board-set! 
		 computer-influence (- row 2) col
		 (+ 1 (board-ref computer-influence (- row 2) col)))
		(board-set! 
		 computer-influence (+ row 2) col
		 (+ 1 (board-ref computer-influence (+ row 2) col)))
		(board-set! 
		 computer-influence row (- col 2)
		 (+ 1 (board-ref computer-influence row (- col 2))))
		(board-set! 
		 computer-influence row (+ col 2)
		 (+ 1 (board-ref computer-influence row (+ col 2))))
		(board-set! 
		 computer-influence (- row 1) col
		 (+ 2 (board-ref computer-influence (- row 1) col)))
		(board-set! 
		 computer-influence (+ row 1) col
		 (+ 2 (board-ref computer-influence (+ row 1) col)))
		(board-set! 
		 computer-influence row (- col 1)
		 (+ 2 (board-ref computer-influence row (- col 1))))
		(board-set! 
		 computer-influence row (+ col 1)
		 (+ 2 (board-ref computer-influence row (+ col 1))))))))
      
      (look-for-empty-territory game computer-influence human-influence)
      (connect-groups game)
      (approach-enemy-stones game computer-influence human-influence))))

(define look-for-empty-territory
  (lambda (game computer-influence human-influence)
    (let ((size (game-size game))
	  (move-values (game-move-values game))
	  (sum 0))
      (do ((row 1 (+ row 1)))
	  ((>= row (- size 1)))
	(do ((col 1 (+ col 1)))
	    ((>= col (- size 1)))
	  (set!
	   sum
	   (+
	    (board-ref computer-influence row col)
	    (board-ref human-influence row col)))
	  (board-set! move-values row col
		      (+
		       (board-ref move-values row col)
		       (* 0.2 (- 3 sum)))))))))



(define connect-groups
  (lambda (game)
    #f))

(define approach-enemy-stones
  (lambda (game computer-influence human-influence)
    (let ((size (game-size game))
	  (board (game-board game))
	  (move-values (game-move-values game)))
      (do ((row 1 (+ row 1)))
	  ((>= row (- size 1)))
	(do ((col 1 (+ col 1)))
	    ((>= col (- size 1)))
	  (if (and
	       (equal? 0 (board-ref board row col))
	       (equal?
		(board-ref computer-influence row col)
		1)
	       (equal?
		(board-ref human-influence row col)
		1))
	      (begin
		(board-set!
		 move-values row col
		 (+
		  (board-ref move-values row col)
		  1.5))
		(display "Approach move considered at ")
		(print-move row col)
		(newline))))))))
