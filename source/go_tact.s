;; File: GO_TACT.S
;
; Description: This file module contains functions that
;              perform the tactical analysis of a given
;              game situation, and add suggested tactical
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

;; Overide the dummied out tactical analysis function defined
;; in file GO_PLAY.S:

(define tactical-update
  (lambda (game)
    (check-for-capture game)
    (check-for-atari-human game)
    (escape-from-atari game)))

(define check-for-capture
  (lambda (game)
    (let ((size (game-size game))
	  (groups (game-groups game))
	  (board (game-board game))
	  (move-values (game-move-values game))
	  (group-liberties (game-liberties game))
	  (groups-processed '()))
      (do ((row 0 (+ row 1)))
	  ((>= row size))
	(do ((col 0 (+ col 1)))
	    ((>= col size))
	  (if (and
	       (equal? HUMAN (board-ref board row col))
	       (equal? 1 (board-ref group-liberties row col)))
	      (if (not (member (board-ref groups row col) groups-processed))
		  (let
		      ;; We have found a group with only one liberty
		      ;; that has not already been processed this turn:
		      ((atari-list 
			(group-attachment
			 game
			 (board-ref groups row col))))
		    (if (> (length atari-list) 0)
			(let* ((attack-row (caar atari-list))
			       (attack-col (cadar atari-list))
			       (current-score
				(board-ref move-values attack-row attack-col)))
			  (display "possible to capture HUMAN group #")
			  (display (board-ref groups row col))
			  (newline)
			  (board-set!
			   move-values
			   attack-row attack-col
			   (+ current-score 
			      (* 2 (group-count game (board-ref groups row col)))))
			  (set! groups-processed
				(cons
				 (board-ref groups row col)
				 groups-processed))))))))))))

(define check-for-atari-human
  (lambda (game)
    (let ((size (game-size game))
	  (groups (game-groups game))
	  (board (game-board game))
	  (move-values (game-move-values game))
	  (group-liberties (game-liberties game))
	  (groups-processed '()))
      (do ((row 0 (+ row 1)))
	  ((>= row size))
	(do ((col 0 (+ col 1)))
	    ((>= col size))
	  (if (and
	       (equal? HUMAN (board-ref board row col))
	       (equal? 2 (board-ref group-liberties row col)))
	      (if (not (member (board-ref groups row col) groups-processed))
		  (let
		      ;; We have found a group with only two liberties
		      ;; that has not already been processed this turn:
		      ((atari-list 
			(group-attachment
			 game
			 (board-ref groups row col))))
		    (if (> (length atari-list) 0)
			(begin
			  (do ((atari atari-list (cdr atari)))
			      ((null? atari))
			    (let* ((attack-row (caar atari))
				   (attack-col (cadar atari))
				   (current-score
				    (board-ref move-values attack-row attack-col)))
			      (display "possible to atari HUMAN group #")
			      (display (board-ref groups row col))
			      (newline)
			      (board-set!
			       move-values
			       attack-row attack-col
			       (+ current-score 
				  (*
				   0.7
				   (group-count
				    game
				    (board-ref groups row col)))))))
			  (set! groups-processed
				(cons
				 (board-ref groups row col)
				 groups-processed))))))))))))

(define escape-from-atari
  (lambda (game)
    (let ((size (game-size game))
	  (groups (game-groups game))
	  (board (game-board game))
	  (move-values (game-move-values game))
	  (group-liberties (game-liberties game))
	  (groups-processed '()))
      (do ((row 0 (+ row 1)))
	  ((>= row size))
	(do ((col 0 (+ col 1)))
	    ((>= col size))
	  (if (and
	       (equal? COMPUTER (board-ref board row col))
	       (equal? 1 (board-ref group-liberties row col)))
	      (if (not (member (board-ref groups row col) groups-processed))
		  (let
		      ;; We have found a group with only one liberty
		      ;; that has not already been processed this turn:
		      ((atari-list 
			(group-attachment
			 game
			 (board-ref groups row col))))
		    (if (> (length atari-list) 0)
			(let* ((defend-row (caar atari-list))
			       (defend-col (cadar atari-list))
			       (current-score
				(board-ref move-values defend-row defend-col)))
			  (display "computer group subject to capture: #")
			  (display (board-ref groups row col))
			  (newline)
			  (board-set!
			   move-values
			   defend-row defend-col
			   (+ current-score 
			      (* 4 (group-count game (board-ref groups row col)))))
			  (set! groups-processed
				(cons
				 (board-ref groups row col)
				 groups-processed))))))))))))