;; File: hand.s
;;
;; Desciption: This file contains an example character
;;             recognition program that uses the neural
;;             network library in file "nn.s" and the
;;             2D array library in file "array.s".  The
;;             characters are encoded as a list of strings.
;;             For example:
;;
;;              (define example-1
;;                (list
;;                      "  xx  "
;;                      " x  x "
;;                      "  xx  "
;;                      " x  x "
;;                      " x  x "
;;                      "  xx  "))
;;             might encode an "8".  This example is set
;;             to work with 6x6 bit images of characters,
;;             but you can easily change this to finer
;;             resolution by changing the two constants
;;             P-WIDTH and P-HEIGHT, and changing the training
;;             and testing data.
;;

(define P-WIDTH 6)
(define P-HEIGHT 6)

(define training-data #f)

(set! training-data
      (list
       (list
        (list 0 0 0)  ;; 0 in binary
        (list 
         "  xx  "
         " x  x "
         "x    x"
         "x    x"
         " x  x "
         "  xx  "))
       (list 
        (list 0 0 1)  ;; 1 in binary
        (list 
         "   x  "
         "   x  "
         "   x  "
         "   x  "
         "   x  "
         "   x  "))
      (list 
       (list 0 0 1)  ;; 1 in binary
       (list 
        "   x  "
        "  xx  "
        "   x  "
        "   x  "
        "   x  "
        "  xxx "))
      (list 
       (list 0 1 0)  ;; 2 in binary
       (list 
        " xxxx "
        "x    x"
        "    x "
        "   x  "
        "  x   "
        " xxxxx"))
      (list 
       (list 0 1 0)  ;; 2 in binary
       (list 
        "  xxx "
        " x   x"
        "    x "
        "   x  "
        " x    "
        "xxxxxx"))
      (list 
       (list 0 1 1)  ;; 3 in binary
       (list 
        " xxx  "
        "x   x "
        " x    "
        "  x   "
        " x  x "
        "xxxx  "))
      (list 
       (list 0 1 1)  ;; 3 in binary
       (list 
        " xxx  "
        "x   x "
        "x     "
        " xx   "
        "x   x "
        "xxxx  "))
      (list 
       (list 1 0 0)  ;; 4 in binary
       (list 
        "x     "
        "x  x  "
        "x  x  "
        "xxxxxx"
        "   x  "
        "   x  "))
      (list 
       (list 1 0 0)  ;; 4 in binary
       (list 
        "   x   "
        "  xx  "
        " x x  "
        "xxxxxx"
        "   x  "
        "   x  "))))



(define convert-training-data
 (lambda (input)
  (let ((output-pattern (car input))
        (str-list (cadr input))
        (input-list '()))
    (do ((a-str str-list (cdr a-str)))
        ((null? a-str))
      (do ((i 0 (+ i 1)))
          ((equal? i P-WIDTH))
        (if (equal? (string-ref (car a-str) i) #\  )
            (set! input-list (cons 0 input-list))
            (set! input-list (cons 1 input-list)))))
    (list (reverse input-list) output-pattern))))
            
;; test:           
; (pp (convert-training-data (cadr training-data)))


(define train
 (lambda ()
  ;; create the training data:
  (let ((t-data (map convert-training-data training-data))
        (a-network (NewDeltaNetwork '(36 4 3)))
        (error 0.0))
    (do ((i 0 (+ i 1)))
        ((equal? i 500))
      (set!
       error
       (DeltaLearn a-network t-data))
      ;; print out every 4 cycles:
      (if (equal? (modulo i 4) 0) 
          (begin
            (display "....training cycle \#")
            (display i)
            (display "  error = ")
            (display error)
            (newline))))
    a-network)))


; (define test-net)
; (set! test-net (train))


(define test-data
  (list
   (list 
    "  xx  "
    " x  x "
    "x    x"
    "x    x"
    " x  x "
    "  xx  ")
   (list 
    "   x  "
    "   x  "
    "   x  "
    "   x  "
    "   x  "
    "   x  ")
   (list 
    "   x  "
    "  xx  "
    "   x  "
    "   x  "
    "   x  "
    "  xxx ")
   (list 
    " xxxx "
    "x    x"
    "    x "
    "   x  "
    "  x   "
    " xxxxx")
   (list 
    "  xxx "
    " x   x"
    "    x "
    "   x  "
    " x    "
    "xxxxxx")
   (list 
    " xxx  "
    "x   x "
    " x    "
    "  x   "
    " x  x "
    "xxxx  ")
   (list 
    " xxx  "
    "x   x "
    "x     "
    " xx   "
    "x   x "
    "xxxx  ")
   (list 
    "x     "
    "x  x  "
    "x  x  "
    "xxxxxx"
    "   x  "
    "   x  ")
   (list 
    "   x   "
    "  xx  "
    " x x  "
    "xxxxxx"
    "   x  "
        "   x  ")))



(define convert-test-data
 (lambda (input)
  (let ((input-list '()))
    (do ((a-str input (cdr a-str)))
        ((null? a-str))
      (do ((i 0 (+ i 1)))
          ((equal? i P-WIDTH))
        (if (equal? (string-ref (car a-str) i) #\  )
            (set! input-list (cons 0 input-list))
            (set! input-list (cons 1 input-list)))))
    (reverse input-list))))
            
;; test:           
; (pp (convert-test-data (cadr test-data)))

(define test
 (lambda (a-network)
  ;; create the training data:
  (let ((data (map convert-test-data test-data))
        (outputs #f))
    (define test-helper
     (lambda (inputs)
      (set! outputs (DeltaRecall a-network inputs))
      (let ((count 0))
        (do ((i 0 (+ i 1)))
            ((equal? i P-HEIGHT))
          (newline)
          (do ((j 0 (+ j 1)))
              ((equal? j P-WIDTH))
            (display (list-ref inputs count))
            (set! count (+ count 1)))))
      (newline)
      (display outputs)
      (newline)))
    (map test-helper data))))

;; You need to run 'train' test before this:

; (test test-net)