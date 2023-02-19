;;
;  Chess Program by Mark Watson
;
; Bug list: (does not handle en passant capture)
;
; Copyright 1990 by Mark Watson
;;

;;
; Define global variables:
;;

(define *wking-moved* #F)
(define *wrook1-moved* #F)
(define *wrook2-moved* #F)
(define *bking-moved* #F)
(define *brook1-moved* #F)
(define *brook2-moved* #F)
(define *move-num* #F)
(define *board* #F)
(define *human-square-control* #F)
(define *computer-square-control* #F)
(define *index* #F)
(define *piece* #F)
(define *value* #F)
(define *debug* #F)
(define *old-board* #F)
(define *old-moves* #F)
(define *moves-for-current-piece* #F)

(set! *board* (make-vector 120 0)) ;; board
(set! *computer-square-control* (make-vector 120 0))
(set! *human-square-control* (make-vector 120 0))
(set! *moves-for-current-piece* #f)
;; piece index into move table:
(set! *index* '(0 12 15 10 1 6 0 0 0 6))
;; piece move table:
(set! *piece* '(0 -1 1 10 -10 0 1 -1 10 -10 -9 
		  -11 9 11 0 8 -8 12 -12
		  19 -19 21 -21 0 10 20 0 0 0 0 0))

;;
; Set value of the pieces: pawn, knight, bishop,
; rook, queen, and king:
;;
(set! *value* '(0 1 3 3 5 9 0 0 0 25))

;;
;  Turn on debug output:
;;
(set! *debug* #t)

;;
; Remember last board state to allow replaying the
; last move when modifying the program:
;;
(set! *old-board* (make-vector 120 0))
(set! *old-moves* 0)

;;
;  Use constants to define piece values:
;;
(define PAWN 1)
(define KNIGHT 2)
(define BISHOP 3)
(define ROOK 4)
(define QUEEN 5)
(define KING 9)

;;
; Format of the board (e.g., square A1 is square 22 and
; square H8 is square 99 in the indexing scheme used
; for the board squares):
;;
; Square index:            Board with pieces:
; -------------------      -------------------------
;
;
; 92 93 94 95 96 97 98 99  BR  BN  BB  BQ  BK  BB  BN  BR
; 82 83 84 85 86 87 88 89  BP  BP  BP  BP  BP  BP  BP  BP
; 72 73 74 75 76 77 78 79  .   X   .   X   .   X   .   X
; 62 63 64 65 66 67 68 69  X   .   X   .   X   .   X   .  
; 52 53 54 55 56 57 58 59  .   X   .   X   .   X   .   X 
; 42 43 45 45 46 47 48 49  X   .   X   .   X   .   X   . 
; 32 33 34 35 36 37 38 39  WP  WP  WP  WP  WP  WP  WP  WP
; 22 23 24 25 26 27 28 29  WR  WN  WB  WQ  WK  WB  WN  WR

;; Note: Human (white) pieces are positive numbers and
;; computer (black pieces) are negative numbers.  For
;; example, if (vector-ref *board* 55) equals -2, then square
;; number 55 (D4 or Queen 4 in chess notation) has a
;; black knight. If it equals 2, then the piece on square
;; 55 is a white knight.

;;
; Initialize the board:
;;

(define initChess
  (lambda ()
    (set! *wking-moved* #f)
    (set! *wrook1-moved* #f)
    (set! *wrook2-moved* #f)
    (set! *bking-moved* #f)
    (set! *brook1-moved* #f)
    (set! *brook2-moved* #f)
    (set! *move-num* 0)
    (do ((i 0 (+ i 1)))
	((equal? i 120))
      (vector-set!
       *board* i
       (vector-ref
	(vector
	 7 7 7 7 7 7 7 7 7 7 7  ; empty squares around outside of board
	 7 7 7 7 7 7 7 7 7 7 7  ; empty squares around outside of board
	 4 2 3 5 9 3 2 4 7 7  ; white pieces
	 1 1 1 1 1 1 1 1 7 7  ; white pawns
	 0 0 0 0 0 0 0 0 7 7  ; 8 blank squares and 2 empty squares 
	 0 0 0 0 0 0 0 0 7 7  ; 8 blank squares and 2 empty squares 
	 0 0 0 0 0 0 0 0 7 7  ; 8 blank squares and 2 empty squares 
	 0 0 0 0 0 0 0 0 7 7  ; 8 blank squares and 2 empty squares 
	 -1 -1 -1 -1 -1 -1 -1 -1 7 7     ; black pawns
	 -4 -2 -3 -5 -9 -3 -2 -4 7 7     ; black pieces
	 7 7 7 7 7 7 7 7 7 7 7 7 7 7  ; empty squares  
	 7 7 7 7 7 7 7 7 7 7 7)       ; empty squares 
	i)))))

(define neq 
  (lambda (a b)
    (not (equal? a b))))

;;
;  Print a piece.  If the piece happens to be an
;  empty square then print the representation for
;  a black or white square on the board:
;;

(define printPiece
  (lambda (piece blackSquare?)
    (if (zero? piece)
	(if blackSquare? (display " X ") (display " . "))
	(begin
	  (if (< piece 0)
	      (display " B")
	      (display " W"))
	  (display (list-ref
		    '("P" "N" "B" "R" "Q" "" "" "" "K")
		    (- (abs piece) 1)))))))

;;
;  Print out the entire board:
;;

(define printBoard
  (lambda ()
    (let ((startColumnList '(92 82 72 62 52 42 32 22)))
      (do ((i 0 (+ i 1)))
	  ((equal? i 8))
	(newline)
	(do ((j 0 (+ j 1)))
	    ((equal? j 8))
	  (let* ((boardPos (+ (list-ref startColumnList i) j))
		 (blackSquare?
		  (member boardPos
			  '(22 24 26 28 33 35 37 39 42 44 46 48
			       53 55 57 59 62 64 66 68 73 75 77 79
			       82 84 86 88 93 95 97 99))))
	    (printPiece 
	     (vector-ref *board* boardPos)
	     blackSquare?))))
      (newline))))

;;
; Calculate all possible moves from all squares:
;;

(define posib
  (lambda ()
    (let ((returnedMoveList '()))
      (do ((ii 0 (+ ii 1)))
	  ((equal? ii 78))
	(let* ((i (+ ii 22))
	       (boardVal (vector-ref *board* i)))
	  (if (< boardVal 0) ;; valid piece to move?
	      ;; collect all squares to which piece on
	      ;; square i can move to:
	      (let ((move-list (goto i #t))
		    (aMove #f))
		(do ((m move-list (cdr m)))
		    ((null? m))
		  (set! aMove (car m))
		  (if (and
		       ;; check for either an empty space
		       ;; or opponent piece:
		       (>= (vector-ref *board* (cadr aMove)) 0)
		       (neq 7 (vector-ref *board* (cadr aMove))))
		      (set! returnedMoveList
			    (cons aMove returnedMoveList))))))))
      (if (and
	   (not *bking-moved*)
	   (not *brook2-moved*)
	   (equal? (vector-ref *board* 97) 0)
	   (equal? (vector-ref *board* 98) 0)
	   (< (vector-ref *human-square-control* 96) 1)
	   (< (vector-ref *human-square-control* 97) 1)
	   (< (vector-ref *human-square-control* 98) 1))
	  (set! returnedMoveList (cons 'oo returnedMoveList)))
      (if (and
	   (not *bking-moved*)
	   (not *brook1-moved*)
	   (equal? (vector-ref *board* 95) 0)
	   (equal? (vector-ref *board* 94) 0)
	   (equal? (vector-ref *board* 93) 0)
	   (< (vector-ref *human-square-control* 96) 1)
	   (< (vector-ref *human-square-control* 95) 1)
	   (< (vector-ref *human-square-control* 94) 1))
	  (set! returnedMoveList (cons 'ooo returnedMoveList)))
      returnedMoveList)))

;;
; For a given square, return a list of all moves the
; piece on that square can move to:
;;

(define goto
  (lambda (squareNum captureFlag)
    (let* ((piece (vector-ref *board* squareNum))
	   (retList '())
	   (ival '(8 0 3))
	   (pieceType (abs piece))
	   (pieceIndex 0)
	   (pieceMovementIndex 0))
      (set! pieceIndex (list-ref *index* pieceType))
      (set! pieceMovementIndex (list-ref *piece* pieceIndex))
      (if
       (not (equal? piece 0)) ; make sure that there is
       ; a piece on square
       (case pieceType
	 ((1)  ; PAWN
	  (let ((sideIndex (if (< piece 0) -1 +1)))
	    (do ((cd '(11 9) (cdr cd)))
		((null? cd))
	      ;; check for diagonal captures:
	      (let ((captureDelta (car cd))) 
		(let*
		    ((movementOffsetInBoard
		      (+ squareNum (* sideIndex captureDelta)))
		     (targetPiece
		      (vector-ref *board* movementOffsetInBoard)))
		  (if (or
		       (and
			(<= targetPiece -1) ; enemy piece --> legal capture
			(neq targetPiece 7) ; not off of board
			(> piece 0))        ; computer piece moving
		       (and
			(>= targetPiece 1)  ; computer piece
			(neq targetPiece 7) ; not off of board
			(< piece 0)))       ; player piece moving
		      (set! retList (cons
				     (list 
				      squareNum
				      (+
				       squareNum
				       (* sideIndex captureDelta)))
				     retList))))))
	    ;; Check for initial pawn move of two squares forward:
	    (let* ((movementOffsetInBoard
		    (+ squareNum (* sideIndex 20))))
	      (if (and
		   captureFlag
		   ; move-to sq empty?:
		   (equal? (vector-ref *board* movementOffsetInBoard) 0)
		   (equal? (truncate (/ squareNum 10))
			   (if (< piece 0) 8 3))
		   (if (< piece 0)
		       (equal? (vector-ref *board* (- squareNum 10)) 0)
		       (equal? (vector-ref *board* (+ squareNum 10)) 0)))
		  (set!
		   retList
		   (cons (list squareNum
			       (+ squareNum (* sideIndex 20)))
			 retList))))
	    (let*
		((movementOffsetInBoard
		  (+ squareNum (* sideIndex 10))))
	      (if (and
		   captureFlag
		   (equal?
		    ; move-to sq empty?
		    (vector-ref *board* movementOffsetInBoard) 0))
		  (set!
		   retList
		   (cons (list squareNum
			       (+ squareNum (* sideIndex 10)))
			 retList))))))
	 
	 ((2 3 4 5 9)  ;;  KNIGHT BISHOP ROOK QUEEN KING
	  (let* ((pieceType (abs piece))
		 (movementTableIndex (list-ref *index* pieceType))
		 (nextToSquare (+ squareNum (list-ref *piece* movementTableIndex))))
	    (do ((keep-going-outer-loop #t)) ;; over movement indices
		((not keep-going-outer-loop))
	      (do ((keep-going #t))
		  ;; break out of loop if OFF OF BOARD:
		  ((or
		    (equal? keep-going #f)
		    (> nextToSquare 99)
		    (< nextToSquare 22)
		    (equal? (vector-ref *board* nextToSquare) 7)))
		(set! retList (cons (list squareNum nextToSquare ) retList))
		(if (neq (vector-ref *board* nextToSquare) 0) ; last move was a capture,
		    (set! keep-going #f)) ;; so break out of the inner loop.
		(if (and
		     (equal? pieceType 1)
		     (equal? (truncate (/ squareNum 10)) 3))
		    (set! keep-going #f))  ;; break out of inner loop
		(if (or
		     (equal? pieceType KNIGHT)
		     (equal? pieceType KING))
		    (set! keep-going #f))  ;; break out of inner loop
		(set! nextToSquare
		      (+ nextToSquare
			 (list-ref *piece* movementTableIndex))))
	      (set! movementTableIndex (+ movementTableIndex 1))
	      ;; Lack of further  move segments is indicated
	      ;; by a zero in the next element of the move
	      ;; index table:
	      (if (equal? (list-ref *piece* movementTableIndex) 0)
		  (set! keep-going-outer-loop #f)) ;; no more move segments
	      (set! nextToSquare
		    (+ squareNum
		       (list-ref *piece* movementTableIndex))))))))
      (if (equal? retList #t) '() retList))))



;;
; Return the static evaluation value for a given board position:
;;

(define value
  (lambda (toSq)
    (let ((retVal 0.0))
      (do ((i 0 (+ i 1)))
	  ((equal? i 120))
	(vector-set! *computer-square-control* i 0)
	(vector-set! *human-square-control* i 0))
      ;; Calculate the number of times the computer's
      ;; pieces control each board square:
      (do ((ii 0 (+ ii 1)))
	  ((equal? ii 78))
	(let ((i (+ ii 22)))
	  (if (< (vector-ref *board* i) 0) ;; computer piece
	      (let ((moveList (goto i #f))  
		    (pawnFudge 0)
		    (move '()))
		(do ((m moveList (cdr m)))
		    ((null? m))
		  (set! move (car m))
		  (if (equal? (abs (vector-ref *board* (car move))) 1)
		      (set! pawnFudge 1.15)
		      (set! pawnFudge 1))
		  (vector-set!
		   *computer-square-control*
		   (cadr move)
		   (+
		    (vector-ref
		     *computer-square-control*
		     (cadr move))
		    pawnFudge)))))))
      
      ;; Calculate the number of times the player's
      ;; pieces control each square:
      (do ((ii 0 (+ ii 1)))
	  ((equal? ii 78))
	(let ((i (+ ii 22)))
	  (if (> (vector-ref *board* i) 0) ;; computer piece
	      (let ((moveList (goto i #f)) ;; generate moves from square # i
		    (pawnFudge 0)
		    (move '()))
		(do ((m moveList (cdr m)))
		    ((null? m))
		  (set! move (car m))  ;; ?? 3/25/95: this was cdr
		  (if (equal? (abs (vector-ref *board* (car move))) 1)
		      (set! pawnFudge 1.25)
		      (set! pawnFudge 1))
		  (vector-set! *human-square-control* (cadr move)
			       (+
				(vector-ref *human-square-control* (cadr move))
				pawnFudge)))))))
      
      
      ;; Subtract 1 from the control array
      ;; for the square being moved to:
      (vector-set! *computer-square-control* toSq
		   (max
		    0
		    (- (vector-ref *computer-square-control* toSq) 1)))
      ;; Set initial value based on board control:
      (do ((ii 0 (+ ii 1)))
	  ((equal? ii 78))
	(let ((i (+ ii 22)))
	  (set! retVal
		(+
		 retVal
		 (*
		  0.1
		  (- (vector-ref *computer-square-control* i)
		     (vector-ref *human-square-control* i)))))))
      
      ;; Modify the value based on material advantage,
      ;; square control, and center control:
      (do ((ii 0 (+ ii 1)))
	  ((equal? ii 79))
	(let ((i (+ ii 22)))
	  (if (and
	       ;; not off of the board:
	       (neq 7 (vector-ref *board* i))
	       ;; not a blank square:
	       (neq 0 (vector-ref *board* i)))
	      (let ((control
		     (- (vector-ref *computer-square-control* i)
			(vector-ref *human-square-control* i))))
		;; Piece value:
		(if (< (vector-ref *board* i) 0)
		    (set!
		     retVal  ;; computer piece
		     (+ retVal
			(*
			 (list-ref 
			  *value*
			  (abs (vector-ref *board* i)))
			 8)))
		    (set!
		     retVal  ;; human piece
		     (- retVal
			(*
			 (list-ref
			  *value*
			  (abs (vector-ref *board* i)))
			 8))))
		;; Check for black piece on white
		;; controlled square:
		(if (and 
		     (< control 0)
		     (< (vector-ref *board* i) 0))
		    (set!
		     retVal 
		     (- retVal
			(* 14
			   (list-ref
			    *value*
			    (abs (vector-ref *board* i)))))))
		;; Check for white piece on black
		;; controlled square:
		(if (and
		     (> control 0)
		     (> (vector-ref *board* i) 0))
		    (set!
		     retVal
		     (+ retVal
			(* 2
			   (min ; limit value of attacked piece to 3
			    3   ; points since this side to move next
			    (list-ref
			     *value*
			     (abs (vector-ref *board* i))))))))
		;; King attacked: 
		(if (and
		     (> (vector-ref *human-square-control* i) 0)
		     (equal? (vector-ref *board* i) -9))
		    (set! retVal (- retVal 5000)))
		;; Queen attacked:
		(if (and
		     (> (vector-ref *human-square-control* i) 0)
		     (equal? (vector-ref *board* i) -5))
		    (set! retVal (- retVal 50)))))))
      
      ;; Pawn placement heuristics:
      ;; (loop over central four squares)
      (do ((sq '(55 56 65 66) (cdr sq)))
	  ((null? sq))
	(if (equal? (vector-ref *board* (car sq)) -1)
	    (set! retVal (+ retVal 2))) ; black pawn
	(if (equal? (vector-ref *board* (car sq))  1)
	    (set! retVal (- retVal 2)))) ; white pawn
      ; Loop over central 16 squares:
      
      (do ((sq 
	    '(44 45 46 47 54 55 56 57 64 65 66 67 74 75 76 77)
	    (cdr sq)))
	  ((null? sq))
	(if (equal? (vector-ref *board* (car sq)) -1) (set! retVal (+ retVal 1))) ; black pawn
	(if (equal? (vector-ref *board* (car sq))  1) (set! retVal (- retVal 1)))) ; white pawn
      ;; Decrease value of moving queen in the first five moves:
      (if (and (< *move-num* 5) (equal? (vector-ref *board* toSq) -5))
	  (set! retVal (- retVal 10)))
      ;; Decrease value of moving king in the first 15 moves:
      (if (and (< *move-num* 15) (equal? (vector-ref *board* toSq) -9))
	  (set! retVal (- retVal 20)))
      (+ retVal (random 2)))))

;;
; Convert internal square number to algrabraic notation:
;;

(define board-pr
  (lambda (sq)
    (let* ((rank (truncate (/ sq 10)))
	   (file (- sq (* rank 10))))
      (set! file (- file 2))
      (set! rank (- rank 1))
      (display (list-ref '("A" "B" "C" "D" "E" "F" "G" "H") file))
      (display rank))))

;;
; Return a list containing the algrabraic notation for a square.
;  For example, square number 22 would be converted to (A 1).
;;

(define board-sq
  (lambda (sq)
    (let* ((rank (truncate (/ sq 10)))
	   (file (- sq (* rank 10))))
      (set! file (- file 2))
      (set! rank (- rank 1))
      (list (list-ref '("A" "B" "C" "D" "E" "F" "G" "H") file)
	    rank))))


;;
; Find the "best" move:
;;

(define sort-func
  (lambda (x y)
    (> (cadr x) (cadr y))))

(define Mover
  (lambda ()
    (set! *move-num* (+ *move-num* 1))
    (let ((possibleMoves (posib))
	  (bestMove '())
	  (bestValue -100000)
	  (to '())
	  (moveValues '()) ;; for debug output only
	  (tosave '())
	  (fromsave '())
	  (pm '())
	  (newVal 0))
      (do ((pm-list possibleMoves (cdr pm-list)))
	  ((null? pm-list))
	(set! pm (car pm-list))
	(set! tosave 0)
	(if (equal? pm 'oo)
	    (begin
	      (vector-set! *board* 96 0)
	      (vector-set! *board* 97 -4)
	      (vector-set! *board* 98 -9)
	      (vector-set! *board* 99 0)
	      (set! to 10)) ; off of board
	    (if (equal? pm 'ooo)
		(begin
		  (vector-set! *board* 96 0)
		  (vector-set! *board* 95 -4)
		  (vector-set! *board* 94 -9)
		  (set! to 10) ; off of board
		  (vector-set! *board* 92 0))
		(begin
		  (set! fromsave (vector-ref *board* (car pm)))
		  (set! tosave (vector-ref *board* (cadr pm)))
		  (vector-set!
		   *board*
		   (cadr pm) 
		   (vector-ref *board* (car pm)))
		  (vector-set!  *board* (car pm) 0)
		  (set! to (cadr pm)))))
	
	
	;; Call value to calculate a numeric score for
	;; how "good" this board position is for the computer:
	(set! newVal (value to))
	
	;; increase the score slightly for captures:
	(if (> tosave 0)
	    (set! newVal
		  (+ newVal 12 (* 11 (list-ref *value* tosave)))))
	(if (member pm '(oo ooo)) (set! newVal (+ newVal 10)))
	(if *debug*
	    (if (member pm '(oo ooo))
		(set! moveValues
		      (cons
		       (list (list pm) newVal)
		       moveValues))
		(set! moveValues
		      (cons
		       (list 
			(append 
			 (board-sq (car pm)) '(" to ")
			 (board-sq (cadr pm)))
			newVal)
		       moveValues))))
	(if (> newVal bestValue)
	    (begin
	      (set! bestValue newVal)
	      (set! bestMove pm)))
	(if (equal? pm 'oo)
	    (begin
	      (vector-set! *board* 96 -9)
	      (vector-set! *board* 97 0)
	      (vector-set! *board* 98 0)
	      (vector-set! *board* 99 -4))
	    (if (equal? pm 'ooo)
		(begin
		  (vector-set! *board* 96 -9)
		  (vector-set! *board* 95 0)
		  (vector-set! *board* 94 0)
		  (vector-set! *board* 92 -4))
		(begin
		  (vector-set! *board* (car pm) fromsave)
		  (vector-set! *board* (cadr pm) tosave)))))
      (if *debug*
	  (let ((m-values (sort moveValues sort-func)))
	    (do ((x m-values (cdr x)))
		((null? x))
	      (newline)
	      (do ((y (caar x) (cdr y)))
		  ((null? y))
		(display (car y)))
	      (display " : ")
	      (display (truncate (cadar x))))
	    (newline)))
      (if (< bestValue -1000)
	  'checkmate
	  bestMove))))


;;
; Main driver program:
;;
(define (chess . restart)
  (if (null? restart) (initChess))
  (if (equal? restart '(backup))  ;; debug option to back up one move on restart
      (begin
	(display "Backing up the game by one move")
	(newline)
	(dotimes (i 120)
		 (vector-set! *board* i 
			      (vector-ref *old-board* i)))
	(set! *moves-for-current-piece* *old-moves*)))
  (printBoard)
  (let ((keep-going #t))
    (do ()
	((not keep-going))
      (display "Enter your move (e.g., d2-d4) : ")
      (let* ((response (symbol->string (read)))
	     (moved? #f))
	(if (or (equal? response "oo") (equal? response "OO"))
	    (if
	     (and  ;; let us be sure that casting king side is legal
	      (not *wking-moved*)
	      (not *wrook2-moved*)
	      (equal? (vector-ref *board* 27) 0)
	      (equal? (vector-ref *board* 28) 0)
	      (< (vector-ref *computer-square-control* 26) 1)
	      (< (vector-ref *computer-square-control* 27) 1)
	      (< (vector-ref *computer-square-control* 27) 1))
	     (begin
	       (set! moved? #t)
	       (display "Castle King side") (newline)
	       (vector-set! *board* 26 0) ; blank old king square
	       (vector-set! *board* 29 0) ; blank old rook square
	       (vector-set! *board* 27 4) ; rook
	       (vector-set! *board* 28 9))) ; king
	    (if (or (equal? response "ooo") (equal? response "OOO"))
		(if
		 (and ;; be sure that a queen side castle is legal
		  (not *wking-moved*)
		  (not *wrook1-moved*)
		  (equal? (vector-ref *board* 23) 0)
		  (equal? (vector-ref *board* 24) 0)
		  (equal? (vector-ref *board* 25) 0)
		  (< (vector-ref *computer-square-control* 26) 1)
		  (< (vector-ref *computer-square-control* 25) 1)
		  (< (vector-ref *computer-square-control* 24) 1))
		 (begin
		   (set! moved? #t)
		   (display "Castle Queen side") (newline)
		   (vector-set! *board* 26 0) ; blank old king square
		   (vector-set! *board* 22 0) ; blank old rook square
		   (vector-set! *board* 25 4) ; rook
		   (vector-set! *board* 24 9))))) ; king
	(if (equal? (string-length response) 5)
	    (let* 
		((fromCol (cadr (assoc
				 (string (string-ref response 0))
				 '(("a" 0) ("b" 1) ("c" 2) ("d" 3)
					   ("e" 4) ("f" 5) ("g" 6) ("h" 7)))))
		 (fromRow (+ 1 (string->number (string (string-ref response 1)))))
		 (toCol (cadr (assoc
			       (string (string-ref response 3))
			       '(("a" 0) ("b" 1) ("c" 2) ("d" 3)
					 ("e" 4) ("f" 5) ("g" 6) ("h" 7)))))
		 (toRow (+ 1 (string->number (string (string-ref response 4)))))
		 (from (+ (* fromRow 10) fromCol 2))
		 (to (+ (* toRow 10) toCol 2)))
	      (set! moved? #t)
	      (vector-set! *board* to (vector-ref *board* from))
	      (vector-set! *board* from 0)))
	(if (not moved?)
	    (display "What???")
	    (begin
	      (printBoard)
	      ;; Remember last state of the board to allow backing up
	      ;; one move for debug by running: (chess 'backup):
	      (do ((i 0 (+ i 1)))
		  ((equal? i 120))
		(vector-set! *old-board* i (vector-ref *board* i)))
	      (set! *old-moves* *moves-for-current-piece*)
	      ;; Calculate the "best" computer move:
	      (let ((bestMove (Mover)))
		(if (equal? bestMove 'checkmate)
		    (begin
		      (display "Checkmate!!")
		      (newline)
		      (set! keep-going #f)))
		(if (equal? bestMove 'oo)
		    (begin
		      (vector-set! *board* 96 0)
		      (vector-set! *board* 97 -4)
		      (vector-set! *board* 98 -9)
		      (vector-set! *board* 99 0)
		      (set! *bking-moved* #t)
		      (set! *brook2-moved* #t)
		      (newline) (display "OO") (newline))
		    (if (equal? bestMove 'ooo)
			(begin
			  (vector-set! *board* 96 0)
			  (vector-set! *board* 95 -4)
			  (vector-set! *board* 94 -9)
			  (vector-set! *board* 92 0)
			  (set! *bking-moved* #t)
			  (set! *brook1-moved* #t)
			  (newline) (display "OOO") (newline))
			(begin
			  (vector-set!
			   *board*
			   (cadr bestMove)
			   (vector-ref *board* (car bestMove)))
			  (vector-set! *board* (car bestMove) 0)
			  (newline)  (display "Computer move : ")
			  (board-pr (car bestMove))
			  (display "-")
			  (board-pr (cadr bestMove))
			  (newline)))))
	      (printBoard)))))))

