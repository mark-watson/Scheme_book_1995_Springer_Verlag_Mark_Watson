;; File: breadth.s
;
; Description: This program was developed from the simple depth
;              first search program in the file DEPTH.S.  Here,
;              we store all possible nodes to search, and "fan
;              out" the search in parallel from each step in
;              the search.
;
; Copyright 1995, Mark Watson. All source code rights reserved.
; This program may be used in compiled form without restrictions.
;;

(define Y-coord (lambda (x) (truncate (cadr x))))
(define X-coord (lambda (x) (truncate (car x))))

(define nodes
  '(
    (n1 (120 804))
    (n2 (100 620))
    (n3 (220 120))
    (n4 (440 750))
    (n5 (385 440))
    (n6 (520 88))
    (n7 (610 600))
    (n8 (695 808))
    (n9 (702 515))
    (n10 (800 220))
    (n11 (830 808))))

(define paths
  '(
    (n1 n2) (n2 n3) (n3 n5) (n3 n6) (n6 n10)
    (n9 n10) (n7 n9) (n1 n4) (n4 n2) (n5 n8)
    (n8 n4) (n7 n11)))

(define init-lengths
  (lambda (pathlist)
    (let ((new-path-list '())
	  (pathlength 0)
	  (path-with-length '()))
      (do ((path pathlist (cdr path)))
	  ((null? path))
	(set! pathlength (slow-path-length (car path)))
	(set! path-with-length (append (car path) (list pathlength)))
	(set! new-path-list (cons path-with-length new-path-list)))
      new-path-list)))


;;
; "As the crow flies" distance between 
;  the starting and ending nodes on a path:
;;

(define slow-path-length
  (lambda (path)
    (let ((node1 (car path))
	  (node2 (cadr path)))
      (let ((n1 (assoc node1 nodes))
	    (n2 (assoc node2 nodes)))
	(dist-between-points (cadr n1) (cadr n2))))))

;;
; Calculate the Cartesian distance between points:
;;

(define dist-between-points
  (lambda (point1 point2)
	  (let ((x-dif (- (X-coord point2) (X-coord point1)))
		(y-dif (- (Y-coord point2) (Y-coord point1))))
	    (sqrt (+ (* x-dif x-dif)  (* y-dif y-dif))))))

; (pp paths)

;;  Change the global path list to include distance between
;;  adjacent nodes:

(set! paths (init-lengths paths))

; (pp paths)

(define find-connected-nodes
  (lambda (a-node)
    (let ((ret-list '()))
      (do ((l paths (cdr l)))
	  ((null? l))
	(let ((path (car l)))  ; (node1 node2 distance)=path
	  (if (equal? a-node (car path))
	      (set! ret-list (cons (cadr path) ret-list)))
	  (if (equal? a-node (cadr path))
	      (set! ret-list (cons (car path) ret-list)))))
      ret-list)))

; (find-connected-nodes 'n2)

(define search 
  (lambda (start-node goal-node)	
    (let* ((a-good-path '())
	   (visited-list (list start-node))
	   (search-list
	    (list (list start-node start-node 0.0)))
	   (search-func
	    (letrec
		((next
		  (lambda (s-list)
		    (let ((new-s-list '()))
		      (do ((l s-list (cdr l)))
			  ((null? l))
			(let ((path (car l)))
			  (let ((last-node (car (last-pair (except-last-pair path)))))
			    (let ((connected-nodes
				   (find-connected-nodes last-node)))
			      (do ((n connected-nodes (cdr n)))
				  ((null? n))
				(if (not (member (car n) visited-list))
				    (begin
				      (set!
				       new-s-list
				       (cons
					(append
					 (except-last-pair path)
					 (list (car n))
					 (list
					  (+
					   (car (last-pair path)) ; old distance
					   (slow-path-length
					    (list 
					     (car
					      (last-pair
					       (except-last-pair path)))
					     (car n))))))
					new-s-list))
				      (set!
				       visited-list
				       (cons (car n) visited-list)))))))))
		      new-s-list)))
		 
		 (found-goal-node?
		  (lambda ()
		    (let ((good-path '()))
		      (do ((l search-list (cdr l)))
			  ((null? l)
			   (not (null? good-path)))
			(if (member goal-node (car l))
			    (begin
			      (set!
			       good-path
			       (car l))
			      (display "Found a good path:")
			      (pp good-path)
			      (newline))))
		      good-path)))
		 
		 (search-1
		  (lambda ()
		    (do ((iter 0 (+ iter 1)))
			((or 
			  (equal? iter (length nodes))
			  (not (null? a-good-path))))
		      (set!
		       search-list
		       (next search-list))
		      (newline)
		      (display "search level=") (display iter) (newline)
		      (display "current visited list:") (newline)
		      (pp visited-list) (newline)
		      (display "current search list:") (newline)
		      (pp search-list) (newline)
		      (set! a-good-path (found-goal-node?)))
		    (cdr a-good-path))))
	      search-1)))
      (search-func)
      a-good-path)))


; (search 'n1 'n11)


;;;;;;;;;;;;;;;; Test code with graphics support:


(define test1 
  (lambda ()
    (open-gr)
    (pen-width 1)
    (do ((p paths (cdr p)))
	((null? p))
      (display "(car p)=") (display (car p)) (newline)
      (let ((from (cadr (assoc (caar p) nodes)))
	    (to (cadr (assoc (cadar p) nodes))))
	(plot-line
	 (x-coord from) 
	 (y-coord from) 
	 (x-coord to) 
	 (y-coord to))))
    (do ((n nodes (cdr n)))
	((null? n))
      (let ((n-val (cadar n)))
	(plot-string 
	 (+ 2 (x-coord n-val)) 
	 (y-coord n-val) 
	 (symbol->string (caar n)))))))

(define test2
  (lambda ()
    (define draw-path (lambda (pl)
			(pen-width 3)
			(let ((node1 (cadr (assoc (car pl) nodes))))
			  (set! pl (cdr pl))
			  (do ((p pl (cdr p)))
			      ((null? p))
			    (plot-line (x-coord node1)
				       (y-coord node1)
				       (x-coord (cadr (assoc (car p) nodes)))
				       (y-coord (cadr (assoc (car p) nodes))))
			    (set! node1 (cadr (assoc (car p) nodes)))))))
    (draw-path (except-last-pair (search 'n1 'n11)))))


; (test1)
; (test2)