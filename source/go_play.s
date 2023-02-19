;; File: GO_PLAY.S
;
; Description: This file contains the logic to play
;              the game of Go.
;
; File Module Dependencies: load GO_INIT.S, GO_DATA.S before
;                           loading this file.
;;

(define make-player-move
  (lambda (game)
    (display "Enter move (e.g., D2 or f5, or a single character for debug) : ")
    (newline)
    (let* ((response (string-capitalize (symbol->string (read))))
	   (col (- (char-code (string-ref response 0)) (char-code #\A)))
	   (row 0))
      (if (< (string-length response) 2)
	  (begin
	    (Go-debug game)
	    (make-player-move game))
	  (begin
	    (set! row (- (char->digit (string-ref response 1)) 1))
	    (if (>
		 (string-length response)
		 2)
		(set!
		 row
		 (+
		  (* 10 (+ row 1))
		  (- (char->digit (string-ref response 2)) 1))))
	    (display "Move: column=")
	    (display col)
	    (display ", row=")
	    (display row)
	    (newline)
	    (add-stone game HUMAN row col)
	    (board-set! (game-move-values game) row col 0))))))

(define best-computer-move
  (lambda (game)
    (let ((board (game-board game))
	  (size (game-size game))
	  (groups (game-groups game))
	  (move-values (game-move-values game))
	  (best-move-val 0)
	  (best-row -1)
	  (best-col 0))
      
      ;; Update move values data before selecting a move:
      (tactical-update game)
      (strategic-update game)
      
      ;; remove illegal moves from the move values array:
      
      (do ((row 0 (+ row 1)))
	  ((>= row size))
	(do ((col 0 (+ col 1)))
	    ((>= col size))
	  (if
	   (> 0 (board-ref move-values row col))
	   (if (not (equal? 0 (board-ref board row col)))
	       (begin
		 ;;	       (display "Removing illegal move from move values array at ")
		 ;;	       (print-move row col)
		 ;;	       (newline)
		 (board-set! move-values row col -5))))))
      
      ;; Choose the move with the highest value:
      
      (do ((row 0 (+ row 1)))
	  ((>= row size))
	(do ((col 0 (+ col 1)))
	    ((>= col size))
	  (if
	   (<
	    best-move-val
	    (board-ref move-values row col))
	   (begin
	     (set! best-row row)
	     (set! best-col col)
	     (set! best-move-val (board-ref move-values row col))))))
      (if (< best-row 0)
	  (begin
	    (display "Computer passes")
	    (newline))
	  (begin
	    (add-stone game COMPUTER best-row best-col)
	    (display "Computer move: ")
	    (display (list-ref
		      '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K"
			    "L" "M" "N" "O" "P" "Q")
		      best-col))
	    (display (+ best-row 1))
	    (newline)
	    (board-set! move-values best-row best-col 0))))))


;; 
;  The following function plots the Go board in a graphics window.
;  This version does nothing.  This function can be re-defined in
;  a plotting File Module.
;;

(define (plot-board game)
  #f)


;;
;  The following function provides debug printout.  This version
;  does nothing.  This function can be re-defined in another
;  File Module to provide specific debug output for testing 
;  new software added to the Go program.
;;

(define Go-debug
  (lambda (game)
    (display "Entered dummy Go-debug function.")
    (newline)))

;;
; The following two functions are stubbed out so that the
; file modules GO_INIT.S, GO_DATA.S, and GO_PLAY.S can be
; unit tested without the tactical and strategic modules:
;;

(define (tactical-update game)
  #f)
(define (strategic-update game)
  #f)


(define (go . size)
  (let ((game #f))
    (if (null? size)
	(set! game (make-go-game 9))
	(set! game (make-go-game (car size))))
    (do ((count 0 (+ count 1)))
	((> count 1000))
      (best-computer-move game)
      (print-board game)
      (plot-board game)
      (make-player-move game))))

