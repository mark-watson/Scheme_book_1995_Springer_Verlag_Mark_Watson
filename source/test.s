;; test out some ideas for 'dotimes' replacement

(define (tt num) ;; sum integers up to num
  (do ((sum 0)
       (i 0 (+ i 1)))
      ((= i num) sum)
    (set! sum (+ sum i))))

(define (pl l) ;; print each element of a list
  (do ((x l (cdr x))
       )
      ((null? x) (display 'done) (newline))
    (display (car x))
    (newline)))

(define (pl2 l) ;; print each element of a list
  (do ((x l (cdr x)))
      ((null? x))
    (display (car x))
    (newline)))
