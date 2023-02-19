;; File: CD.S
 ;
 ; Description: This file contains a parser for Roger Schank's
 ;              and Chris Riesbeck's Conceptual Dependency (CD) theory.
 ;
 ; Copyright 1995, Mark Watson
 ;;

;; Definition of a CD frame data structure:

;     actor
;     action
;     object
;     time  (units of hours)
;     place
;     recipient

(define make-frame
  (lambda ()
    (make-vector 6 #f)))

(define actor-set!
  (lambda (frame actor)
    (vector-set! frame 0 actor)))

(define actor-ref
  (lambda (frame)
    (vector-ref frame 0)))

(define action-set!
  (lambda (frame action)
    (vector-set! frame 1 action)))

(define action-ref
  (lambda (frame)
    (vector-ref frame 1)))

(define object-set!
  (lambda (frame object)
    (vector-set! frame 2 object)))

(define object-ref
  (lambda (frame)
    (vector-ref frame 2)))

(define time-set! 
  (lambda (frame time)
    (vector-set! frame 3 time)))

(define time-ref 
  (lambda (frame)
    (vector-ref frame 3)))

(define place-set!
  (lambda (frame place)
    (vector-set! frame 4 place)))

(define place-ref 
  (lambda (frame)
    (vector-ref frame 4)))

(define recipient-set!
  (lambda (frame recipient)
    (vector-set! frame 5 recipient)))

(define recipient-ref
  (lambda (frame)
    (vector-ref frame 5)))

(define human-list '(Mark Carol))
(define object-list '(car boat book))

;; Format of verb-list items:

;    english word
;    CD primative
;    actor <--> recipient reversal flag


(define verb-list '((gave ATRANS #f)
		    (give ATRANS #f)
		    (received ATRANS #t)
		    (move PTRANS #f)
		    (tell MTRANS #f)))

;; Time:

(define time-list '(((yesterday) -24) ((last week) -168) ((tommorrow) 24)))

(define noun-list '(car book boat street))

(define parse-human-name
  (lambda (current-frame rest-of-sentence)
    (let ((word (car rest-of-sentence)))
      (if (member word human-list)
	  (if (equal? (actor-ref current-frame) #f)
	      (actor-set! current-frame word)
	      (if (equal? (recipient-ref current-frame) #f)
		  (recipient-set! current-frame word)))))))

(define parse-verbs
  (lambda (current-frame rest-of-sentence)
    (let ((action-item (assoc (car rest-of-sentence) verb-list)))
      (if (not (null? action-item))
	  (let ((verb (list-ref action-item 0))
		(cd-primitive (list-ref action-item 1))
		(actor-recipient-reversal-flag (list-ref action-item 2)))
	    (action-set! current-frame cd-primitive)
	    (if actor-recipient-reversal-flag
		(let ((temp (actor-ref current-frame)))
		  (actor-set! current-frame (recipient-ref current-frame))
		  (recipient-set! current-frame temp))))))))

(define parse-nouns
  (lambda (current-frame rest-of-sentence)
    (let ((word (car rest-of-sentence)))
      (if (member word noun-list)
	  (if (equal? (object-ref current-frame) #f)
	      (object-set! current-frame word))))))

(define parse-time 
  (lambda (current-frame rest-of-sentence)
    (let ((time-item (assoc (car rest-of-sentence) time-list)))
      (if (not (null? time-item))
	  (time-set! current-frame (cadr time-item))))
    (if (> (length rest-of-sentence) 1)
	(let ((time-item 
	       (assoc
		(list (car rest-of-sentence) (cadr rest-of-sentence))
		time-list)))
	  (if (not (null? time-item))
	      (time-set! current-frame (cadr time-item)))))))

(define parse-all
  (lambda (current-frame rest-of-sentence)
    (parse-human-name current-frame rest-of-sentence)
    (parse-verbs current-frame rest-of-sentence)
    (parse-nouns current-frame rest-of-sentence)
    (parse-time current-frame rest-of-sentence)))


;; test code:

(define sentence '(Mark gave Carol a book last week))

(define test
  (lambda ()
    (let ((f (make-frame)))
      (do ((sen sentence (cdr sen)))
	  ((null? sen))
	(display "Sentence: ")
	(display sen)
	(newline)
	(parse-all f sen)
	(display "Parse: ")
	(display f)
	(newline)))))
