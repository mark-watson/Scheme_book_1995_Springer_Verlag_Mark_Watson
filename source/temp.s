;; File: GO_DATA.S
;
; Description: Building a Go library for maintaining
;              Go data structures
;
; Copyright 1995, Mark Watson. All source code rights reserved.
; The contents of this file can be used in compiled form
; without restriction.
;
; File module dependencies:
;
;      File GO_INIT.S must be loaded before this file.
;
;;


(define touched-enemy-stones
  (lambda (game side-to-play row column)
    (touched-friend-stones game (- 3 side-to-play) row column)))

(define touched-friend-stones
  (lambda (game side-to-play row column)
    (let ((ret '())
	  (size (game-size game))
	  (board (game-board game)))
      (if (> row 1)
	  (if (equal? side-to-play (board-ref board (- row 1) column))
	      (set! 
	       ret
	       (cons (list (- row 1) column) ret))))
      (if (< row (- size 1))
	  (if (equal? side-to-play (board-ref board (+ row 1) column))
	      (set! 
	       ret
	       (cons (list (+ row 1) column) ret))))
      (if (> column 1)
	  (if (equal? side-to-play (board-ref board row (- column 1)))
	      (set! 
	       ret
	       (cons (list row (- column 1)) ret))))
      (if (< column (- size 1))
	  (if (equal? side-to-play (board-ref board row (+ column 1)))
	      (set! 
	       ret
	       (cons (list row (+ column 1)) ret))))
      ret)))

(define change-group-ID
  (lambda (game old-ID new-ID)
    (let ((groups (game-groups game))
	  (size (game-size game)))
      (do ((row 0 (+ row 1)))
	  ((> row (- size 1)))
	(do ((col 0 (+ col 1)))
	    ((> col (- size 1)))
	  (if (equal?
	       (board-ref groups row col)
	       old-ID)
	      (board-set! groups row col new-ID))))
      (update-liberties game new-ID))))


(define update-liberties
  (lambda (game group-num)
    (let* ((size (game-size game))
	   (scratch (make-board size))
	   (groups (game-groups game))
	   (board (game-board game))
	   (group-liberties (game-liberties game))
	   (num-lib 0))
      (do ((i 0 (+ i 1)))
	  ((>= i size))
	(do ((j 0 (+ j 1)))
	    ((>= j size))
	  (if (equal?
	       group-num
	       (board-ref groups i j))
	      (begin
		(do ((ii 0 (+ ii 1)))
		    ((>= ii 2))
		  (let ((iii (+ i 1 (* -2 ii))))
		    (if (and
			 (> iii -1)
			 (< iii size))
			(if (and
			     (equal?
			      (board-ref board iii j)
			      0)
			     (equal?
			      (board-ref scratch iii j)
			      0))
			    (begin
			      (board-set! scratch iii j 1)
			      (set! num-lib (+ num-lib 1))))))
		  (let ((jjj (+ j 1 (* -2 ii))))
		    (if (and
			 (> jjj -1)
			 (< jjj size))
			(if (and
			     (equal?
			      (board-ref board i jjj)
			      0)
			     (equal?
			      (board-ref scratch i jjj)
			      0))
			    (begin
			      (board-set! scratch i jjj 1)
			      (set! num-lib (+ num-lib 1)))))))))))
      (if (> num-lib 0)
	  (do ((i 0 (+ i 1)))
	      ((>= i size))
	    (do ((j 0 (+ j 1)))
		((>= j size))
	      (if
	       (equal?
		group-num
		(board-ref groups i j))
	       (board-set! group-liberties i j num-lib))))
	  (begin
	    (display "Group ")
	    (display group-num)
	    (display " is dead.")
	    (newline)
	    (remove-group game group-num))))))


(define add-stone
  (lambda (game side-to-play row column)
    (let ((touched-friends 
	   (touched-friend-stones game side-to-play row column))
	  (board (game-board game))
	  (groups (game-groups game))
	  (group-num (+ (game-group-counter game) 1)))
      (game-group-counter-set! game group-num)
      (board-set! (game-groups game) row column group-num)
      (board-set! (game-board game) row column side-to-play)
      ;; loop over all friendly touched stones, changing all
      ;; group IDs of touched stones to 'group-num':
      (do ((grp touched-friends (cdr grp)))
	  ((null? grp))
	(let ((a-row (caar grp))
	      (a-col (cadar grp)))
	  (let ((a-grp-num (board-ref groups a-row a-col)))
	    (change-group-ID game a-grp-num group-num))))
      (let ((touched-stones
	     (append
	      (touched-friend-stones game COMPUTER row column)
	      (touched-enemy-stones game COMPUTER row column)))
	    (touched-groups '()))
	(set! touched-groups
	      (map
	       (lambda (grid)
		 (let ((row (car grid))
		       (col (cadr grid)))
		   (board-ref groups row col)))
	       touched-stones))
	(display "touched groups: ")
	(display touched-groups)
	(newline)
	;; note: should remove duplicates in list here
	(do ((group touched-groups (cdr group)))
	    ((null? group))
	  (update-liberties game (car group))))
      (update-liberties game (board-ref groups row column)))))

(define remove-group
  (lambda (game group-num)
    (let ((size (game-size game))
	  (groups (game-groups game))
	  (board (game-board game))
	  (group-liberties (game-liberties game)))
      (do ((i 0 (+ i 1)))
	  ((>= i size))
	(do ((j 0 (+ j 1)))
	    ((>= j size))
	  (if
	   (equal?
	    group-num
	    (board-ref groups i j))
	   (begin
	     (board-set! group-liberties i j 0)
	     (board-set! groups i j 0)
	     (board-set! board i j 0))))))))

;; Return a list of all empty grid points on the Go board that are
;; touching a specified group:

(define group-attachment
  (lambda (game group-num)
    (let* ((size (game-size game))
	   (scratch (make-board size))
	   (groups (game-groups game))
	   (board (game-board game))
	   (ret-list '()))
      (do ((i 0 (+ i 1)))
	  ((>= i size))
	(do ((j 0 (+ j 1)))
	    ((>= j size))
	  (if (equal?
	       group-num
	       (board-ref groups i j))
	      (begin
		(do ((ii 0 (+ ii 1)))
		    ((>= ii 2))
		  (let ((iii (+ i 1 (* -2 ii))))
		    (if (and
			 (> iii -1)
			 (< iii size))
			(if (and
			     (equal?
			      (board-ref board iii j)
			      0)
			     (equal?
			      (board-ref scratch iii j)
			      0))
			    (begin
			      (board-set! scratch iii j 1)))))
		  (let ((jjj (+ j 1 (* -2 ii))))
		    (if (and
			 (> jjj -1)
			 (< jjj size))
			(if (and
			     (equal?
			      (board-ref board i jjj)
			      0)
			     (equal?
			      (board-ref scratch i jjj)
			      0))
			    (begin
			      (board-set! scratch i jjj 1))))))))))
      
      (do ((i 0 (+ i 1)))
	  ((>= i size))
	(do ((j 0 (+ j 1)))
	    ((>= j size))
	  (if
	   (not
	    (equal?
	     0
	     (board-ref scratch i j)))
	   (set! ret-list (cons (list i j) ret-list)))))
      ret-list)))

;; Return the number of stones in a specified group:

(define group-count
  (lambda (game group-num)
    (let ((size (game-size game))
	  (groups (game-groups game))
	  (ret-count 0))
      (do ((row 0 (+ row 1)))
	  ((>= row size))
	(do ((col 0 (+ col 1)))
	    ((>= col size))
	  (if (equal?
	       group-num
	       (board-ref groups row col))
	      (set! ret-count (+ ret-count 1)))))
      ret-count)))

(define print-move
  (lambda (row col)
    (let ((col-names '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J"
			   "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T"))
	  (row-names '("1" "2" "3" "4" "5" "6" "7" "8"
			   "9" "10" "11" "12" "13" "14" "15" "16"
			   "17" "18" "19" "20")))
      (display (list-ref col-names col))
      (display (list-ref row-names row)))))

(define print-board
  (lambda (game)
    (let ((col-names '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J"
			   "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T"))
	  (row-names '(" 1" " 2" " 3" " 4" " 5" " 6" " 7" " 8"
			    " 9" "10" "11" "12" "13" "14" "15" "16"
			    "17" "18" "19" "20"))
	  (size (game-size game))
	  (board (game-board game)))
      (newline)
      (do ((row 0 (+ row 1)))
	  ((>= row size))
	(newline)
	(display (list-ref row-names (- size row 1)))
	(display ":  ")
	(do ((col 0 (+ col 1)))
	    ((>= col size))
	  (if (equal?
	       (board-ref board (- size row 1) col)
	       0)
	      (display "-|-"))
	  (if (equal?
	       (board-ref board (- size row 1) col)
	       COMPUTER)
	      (display "-C-"))
	  (if (equal?
	       (board-ref board (- size row 1) col)
	       HUMAN)
	      (display "-H-")))
	(newline))
      (newline)
      (display "     ")
      (do ((col 0 (+ col 1)))
	  ((>= col size))
	(begin
	  (display " ")
	  (display (list-ref col-names col))
	  (display " ")))
      (newline)
      (newline))))
