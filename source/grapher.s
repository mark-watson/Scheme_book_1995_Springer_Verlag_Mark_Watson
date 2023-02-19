;; File: grapher.s
;;
;; Description: This file module contains a library for
;;              plotting graph data structures.
;;
;; Public interface functions:
;;
;;  (make-grapher root-node-name) -> a 'grapher' object
;;     This function opens a graphics window and
;;     returns a 'grapher' object.  All other public
;;     functions in this file module require a
;;     'grapher' object as their first argument.
;; 
;;  (add-node a-grapher name parent-name)
;;     This function creates a new node as a child
;;     node to an existing node.
;;
;;  (name-to-id a-grapher name) -> integer node ID
;;
;;  (id-to-name a-grapher id) -> string name of node
;;
;;  (get-node-id-from-position a-grapher x y)
;;     -> integer node ID of closest node to the
;;        specified screen coordinates x and y.
;;
;;  (get-node-info a-grapher node)
;;     -> a vector containing elements:
;;            0) x coordinate in virtual screen units
;;            1) y coordinate in virtual screen units
;;            2) name
;;            3) parent ID (integer)
;;            4) node selection flag (integer 0 or 1)
;;            5) arbitrary data
;;       The node argument can be an integer (in which
;;       case it is interpreted as a node ID), or a
;;       string (in which case it is interpreted as
;;       a name).
;;
;;  (make-node x y name) -> a new node
;;
;;  (get-node-x a-node) -> x virtual screen coordinate
;;
;;  (get-node-y a-node) -> y virtual screen coordinate
;;
;;  (get-node-name a-node) -> node name
;;
;;  (get-parent-id a-node) -> integer parent ID
;;
;;  (set-node-selection-flag a-node flag)
;;  (get-node-selection-flag a-node)
;;       -> integer selection flag value
;;
;;  (set-node-data a-node any-data-at-all)
;;  (get-node-data a-node)
;;       -> any Scheme data structure stored in node
;;
;;  (layout-graph)
;;
;;  Copyright 1995, Mark Watson. All source code rights
;;  reserved: you may not redistribute this source file.
;;  No binary rights reserved: you may use this softeware
;;  in compiled form without restriction.
;;

;; "constants" for ccessing node data fields:
(define _X-COORD 0)
(define _Y-COORD 1)
(define _NAME 2)
(define _PARENT-ID 3)
(define _SELECTION-FLAG 4)
(define _DATA 5)

;; "constants" for accessing grapher data fields:
(define _NUM-NODES 0)
(define _TREE 1)
(define _ROOT-NAME 2)
(define _MAX-NODES 3)
(define _LAST-Y 4)

;; Miscelanious "constants":
(define _Y-SPACING 90)
(define _X-SPACING 300)
(define _CHAR-WIDTH 18) ;; width of character of screen in pixels

(define make-grapher
  (lambda (root-node-name)
    (if (not (string? root-node-name))
	(begin
	  (write "Error: node names must be trings.")
	  (newline))
	;; Name OK, so create a ne grapher object:
	(let ((max-nodes 7)
	      (num-nodes 1)
	      (last-y 0) ;; used for layout calculations
	      (new-node (make-vector 6))
	      (tree #f))
	  ;; create the root node:
	  (set! tree (make-vector max-nodes))
	  (vector-set! new-node 0 0) ; x coordinate
	  (vector-set! new-node 1 0) ; y coordinate
	  (vector-set! new-node 2 root-node-name)
	  (vector-set! new-node 3 -1)
	  (vector-set! new-node 4 0) ; selection flag
	  (vector-set! new-node 5 #f) ;; data for node
	  ;; insert the root node into the tree:
	  (vector-set! tree 0 new-node)
	  ;; create the list which is a new grapher object:
	  (vector num-nodes tree root-node-name max-nodes last-y)))))

;; Private utility functions for accessing fields in
;; a grapher object:

(define get-num-nodes
  (lambda (a-grapher)
    (vector-ref a-grapher _NUM-NODES)))

(define get-tree
  (lambda (a-grapher) (vector-ref a-grapher _TREE)))

(define get-node
  (lambda (a-grapher node)
    (let ((tree (get-tree a-grapher))
	  (num (get-num-nodes a-grapher))
	  (node-index -1))
      ;; node can either be a string name of a node,
      ;; or an integer index:
      (if (string? node)
	  (do ((i 0 (+ i 1)))
	      ((equal? i num))
	    (if (string=?
		 node
		 (vector-ref (vector-ref tree i) _ROOT_NAME))
		(set! node-index i)))
	  (set! node-index node))
      (if (and
	   (> node-index -1)
	   (< node-index (vector-ref a-grapher _MAX-NODES)))
	  (vector-ref tree node-index) ;; found the node
	  ;; node not in grapher object, so return #f:
	  #f))))

(define get-node-index
  (lambda (a-grapher node)
    (let ((tree (get-tree a-grapher))
	  (num (get-num-nodes a-grapher))
	  (node-index -1))
      ;; node can either be a string name of a node,
      ;; or an integer index:
      (if (string? node)
	  (do ((i 0 (+ i 1)))
	      ((equal? i num))
	    (if (string=?
		 node
		 (vector-ref (vector-ref tree i) _ROOT-NAME))
		(set! node-index i)))
	  (set! node-index node))
      node-index)))


(define add-node
  (lambda (a-grapher node-name parent-node data)
    (let ((tree (get-tree a-grapher))
	  (parent-index (get-node-index a-grapher parent-node))
	  (num (get-num-nodes a-grapher))
	  (new-node (make-vector 6)))
      ;; make sure that there is enough space for a new node:
      (if (> (+ num 2) (vector-ref a-grapher _MAX-NODES))
	  (begin
	    (display "No space for a new node.")
	    (newline)
	    #f)
	  (begin
	    (vector-set! new-node _X-COORD 0) ; x coordinate
	    (vector-set! new-node _Y-COORD 0) ; y coordinate
	    (vector-set! new-node _NAME node-name)
	    (vector-set! new-node _PARENT-ID parent-index)
	    (vector-set! new-node _SELECTION-FLAG 0)
	    (vector-set! new-node _DATA data)
	    (vector-set! tree num new-node)
	    (vector-set! a-grapher _NUM-NODES (+ num 1))
	    new-node)))))


(define do-layout
  (lambda (a-grapher)
    (let ((tree (get-tree a-grapher))
	  (parent-index (get-node-index a-grapher _PARENT-ID))
	  (num (get-num-nodes a-grapher)))
      (do ((i 0 (+ i 1)))
	  ((equal? i num))
	(let ((node (vector-ref tree i)))
	  (vector-set! node _X-COORD 0)
	  (vector-set! node _Y-COORD 0)))
      (vector-set! a-grapher _LAST-Y 0)
      (y-layout a-grapher 0 0)
      (x-layout a-grapher 0 0))))

(define y-layout
  (lambda (a-grapher node-id level)
    (let ((tree (get-tree a-grapher))
	  (parent-index (get-node-index a-grapher _PARENT-ID))
	  (average-y 0)
	  (last-y (vector-ref a-grapher _LAST-Y))
	  (num (get-num-nodes a-grapher)))
      (let ((node (vector-ref tree node-id)))
	(if (equal? (vector-ref node _Y-COORD) 0) ;; check y coord.
	    (let ((child-nodes (get-children a-grapher node-id)))
	      (if (null? child-nodes)
		  (let ((new-y-value 
			 (+ (vector-ref node _Y-COORD) _Y-SPACING)))
		    (vector-set! node _Y-COORD
				 (+ last-y _Y-SPACING))
		    (display "last-y=") (display last-y) (newline)
		    (vector-set! a-grapher _LAST-Y new-y-value))
		  (let ((copy-child-nodes (list-copy child-nodes))
			(len (length child-nodes)))
		    (do ((child-nodes child-nodes (cdr child-nodes)))
			((null? child-nodes))
		      (y-layout a-grapher (car child-nodes) (+ level 1)))
		    (set! average-y 0)
		    (do ((copy-child-nodes copy-child-nodes 
					   (cdr copy-child-nodes)))
			((null? copy-child-nodes))
		      (set! average-y
			    (+ average-y
			       (vector-ref
				(vector-ref tree (car copy-child-nodes))
				_Y-COORD ))))
		    (set! average-y (/ average-y len))
		    (vector-set! node _Y-COORD average-y)))
	      (vector-set! node _X-COORD 0)))))))

(define x-layout
  (lambda (a-grapher node-id level)
    (let ((tree (get-tree a-grapher))
	  (num (get-num-nodes a-grapher)))
      (if (equal? level 0)
	  ;; top level root node:
	  (let ((node (vector-ref tree node-id))
		(child-nodes (get-children a-grapher node-id)))
	    (vector-set! node _X-COORD _X-SPACING)
	    (do ((child-nodes child-nodes (cdr child-nodes)))
		((null? child-nodes))
	      (x-layout a-grapher (car child-nodes) (+ level 1))))
	  ;; not the root node, so calculate the spacing
	  ;; based on the number of characters in the
	  ;; parent node's name:
	  (let ((node (vector-ref tree node-id))
		(child-nodes (get-children a-grapher node-id)))
	    (let ((parent-id (vector-ref node _PARENT-ID)))
	      (let ((parent (vector-ref tree parent-id)))
		(let ((len (string-length (vector-ref parent _NAME))))
		  (vector-set!
		   node _X-COORD
		   (+
		    (vector-ref parent _X-COORD)
		    (* len _CHAR-WIDTH)))
		  ;; layout any unprocessed child nodes:
		  (do ((child-nodes child-nodes (cdr child-nodes)))
		      ((null? child-nodes))
		    (x-layout
		     a-grapher
		     (car child-nodes)
		     (+ level 1)))))))))))



(define get-children
  (lambda (a-grapher node-id)
    (let ((tree (get-tree a-grapher))
	  (num (get-num-nodes a-grapher))
	  (count 0)
	  (return-list '()))
      (do ((i 0 (+ i 1)))
	  ((equal? i num))
	(if (equal? node-id (vector-ref (vector-ref tree i) _PARENT-ID))
	    (set! return-list (cons i return-list))))
      return-list)))


;; The following plotting code requires the file "graph.s"
;; to be loaded in order to run:

(define draw
  (lambda (a-grapher)
    (let ((tree (get-tree a-grapher))
	  (num (get-num-nodes a-grapher)))
      (do ((i 0 (+ i 1)))
	  ((equal? i num))
	(let ((node (vector-ref tree i)))
	  (let ((parent-id (vector-ref node _PARENT-ID)))
	    (if (> parent-id -1)
		(let ((parent (vector-ref tree parent-id))
		      (y_half (truncate (/ _Y-SPACING 8))))
		  (plot-line 
		   (+
		    (vector-ref parent _X-COORD)
		    (* _CHAR-WIDTH 
		       (string-length
			(vector-ref parent _NAME)))
		    -20)
		   (+
		    (vector-ref parent _Y-COORD)
		    y_half)
		   (vector-ref node _X-COORD)
		   (+
		    (vector-ref node _Y-COORD)
		    y_half)))))))
      (do ((i 0 (+ i 1)))
	  ((equal? i num))
	(let ((node (vector-ref tree i)))
	  (plot-string
	   (vector-ref node _X-COORD)
	   (vector-ref node _Y-COORD)
	   (vector-ref node _NAME)))))))



;;;; test code:


;(define g)
;(set! g (make-grapher "root"))
;(display g)
;(pp g)
;(add-node g "node-1" "root" '(this is data 1))
;(add-node g "node-2" "root" '(this is data 2))
;(add-node g "node-1-1" "node-1" '(this is data 1 1 1))
;(get-children g 0)
;(get-children g 1)
;(get-children g 3)
;(do-layout g)
;(pp g)
;(open-gr)
;(clear-plot)
;(draw g)
;(close-gr)