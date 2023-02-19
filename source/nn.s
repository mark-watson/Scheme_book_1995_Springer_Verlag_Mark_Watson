;; File NN.s
;;
;; Description: This file contains a library for Back Propagation
;;              (also known as "Delta Rule") neural networks.
;;              Any number of neuron layers (with momentum) are
;;              supported.
;;
;; This software can be used in compiled form without
;; restriction.  The source code can not be distributed
;; without the author's permission.
;;
;; Copyright 1995 by Mark Watson

;;  MODULE DEPENDENCIES:
;
;      Load array.s (or array.com) for 2D array support
;      (required)
;
;      Load graph.s (or graph.com) for graphics support
;      (optional)
;


;; Externally (callable) functions in this file:

; (NewDeltaNetwork sizeList)
;   Args: sizeList = list of sizes of slabs. This also defines 
;                    the number of slabs in the network.
;                    (e.g.,  '(10 5 4) ==> a 3-slab network
;                    with 10 input neurons, 5 hidden
;                    neurons, and 4 output neurons).
;
;   Returned value = a list describing the network:
;      (nLayers sizeList 
;       (activation-array[1] .. activation-array[nLayers])
;       (sum-of-products[2] .. sum-of-products[nLayers[nLayers])
;       (weight-array[2] .. weight-array[nLayers])
;       (delta-weight-array[2] .. delta-weight-array[nLayers])
;       (back-prop-error[2] .. back-prop-error[nLayers]))
;       (old-delta-weights[2] .. for momentum term

; (DeltaLearn networkList trainingList . plotFlag)
;   Args: networkList  = list returned from function
;                        NewDeltaNetwork
;         trainingList = a list of lists of training exemplars.
;                        For example, a list might be:
;                        (((0 1) (1 0))  ; first exemplar
;                         ((1 0) (0 1))) ; second exemplar
;                        Note: the inner sub-lists can
;                        also be arrays.
;         nIterations  = number of complete training iterations
;
;   Returned value = average error at output neurons for last
;                    training cycle.

; (DeltaRecall networkList inputList)
;   Args: networkList  = list returned from function
;                        'NewDeltaNetwork'
;         inputList    = list OR array of input activation values
;
;   Returned value = list of output neuron values
;
; (DeltaPlot networkList) ==> plots a network. Must call
;                             '(open-gr) first.
;
; (WriteDeltaNetwork fileName networkList) ==> saves a
;                                         network to disk
;
; (ReadDeltaNetwork fileName) ==> returns a network list



;; Define default learning rates for each layer of neurons:

(define defaultEidaList '(0.5 0.4 0.3 0.2 0.08 0.07))

;; Define the default noise to add to each input neuron:

(define *delta-default-input-noise-value* 0.08)
(define *delta-rule-debug-flag* '())

;; Local utility: generate floating point random numbers:

(define frandom
 (lambda (lower upper)
  (+ lower
     (random (- upper lower)))))

;;
; Create a new delta network:
;;

;; alpha = coefficient for new weight change
;; beta  = coefficient for adding in last weight change

(define alpha 0.2)
(define beta 0.8)

(define eidaList '())

; (NewDeltaNetwork '(2 2))

(define NewDeltaNetwork
 (lambda (sizeList)
  (let ((numLayers (length sizeList))
        (w-list '())        ; weights
        (dw-list '())       ; delta weights
        (old-dw-list '())   ; old delta weights for
                            ; momentum terms
        (a-list '())        ; activation values
        (s-list '())        ; sum of products
        (d-list '()))       ; back propagated deltas
    
    (set! eidaList defaultEidaList)
    
    ;;
    ; Initialize storage for activation energy for all slabs:
    ;;
    (set! a-list
          (map
           (lambda (size) (make-vector size 0.0))
           sizeList))
    
    ;;
    ; Initialize storage for sum of products arrays:
    ;;
    (set! s-list
          (map
           (lambda (size) (make-vector size 0.0))
           (cdr sizeList)))
    
    ;;
    ; Initialize storage for delta arrays:
    ;;
    (set! d-list
          (map
           (lambda (size) (make-vector size 0.0))
           (cdr sizeList)))
    
    ;;
    ; Initialize storage for the weights:
    ;;
    (do ((i 0 (+ i 1)))
        ((equal? i (- numLayers 1)))
      (set!
       w-list
       (cons 
        (list
         (list-ref sizeList i)
         (list-ref sizeList (+ i 1)))
        w-list)))
    
    (set! w-list
          (map
           (lambda (size)
             (make-2D-array (car size) (cadr size)))
           (reverse w-list)))
    
    ;;
    ; Initialize the storage for delta weights:
    ;;
    (do ((i 0 (+ i 1)))
        ((equal? i (- numLayers 1)))
      (set!
       dw-list
       (cons 
        (list (list-ref sizeList i)
              (list-ref sizeList (+ i 1))) dw-list)))
    (set! dw-list
          (map
           (lambda (size)
             (make-2D-array (car size) (cadr size)))
           (reverse dw-list)))
    
    ;;
    ; Initialize the storage for old delta weights:
    ;;
    (do ((i 0 (+ i 1)))
        ((equal? i (- numLayers 1)))
      (set!
       old-dw-list
       (cons 
        (list (list-ref sizeList i)
              (list-ref sizeList (+ i 1))) old-dw-list)))
    (set! old-dw-list
          (map
           (lambda (size)
             (make-2D-array (car size) (cadr size)))
           (reverse old-dw-list)))
    
    ;;
    ;  Initialize values for all activations:
    ;;
    (map
     (lambda (x)
       (let ((num (vector-length x)))
         (do ((n 0 (+ n 1)))
             ((equal? n num))
           (vector-set! x n (frandom 0.01 0.1)))))
     a-list)
    
    ;;
    ;  Initialize values for all weights:
    ;;
    (map
     (lambda (x)
       (let ((numI (2D-array-length x 0))
             (numJ (2D-array-length x 1)))
         (do ((j 0 (+ j 1)))
             ((equal? j numJ))
           (do ((i 0 (+ i 1)))
               ((equal? i numI))
             (2D-array-set! x i j (frandom -0.5 0.5))))))
     w-list)
    (list numLayers sizeList a-list s-list w-list dw-list
          d-list old-dw-list alpha beta))))

;;
;  Utility function for training a delta rule neural network.
;  The first argument is a network definition (as returned from
;  NewDeltaNetwork), the second argument is a list of training
;  data cases (see the example test functions at the end of this
;  file for examples, the third (optional) argument is a flag for
;  automatic plotting of the state of the network.  Call
;  DeltaLearn with a third argument equal to #t for plotiing.
;;


(define (DeltaLearn netList trainList . plotFlag)
  (let ((nLayers (car netList))
        (sizeList (cadr netList))
        (activationList (caddr netList))
        (sumOfProductsList (car (cdddr netList)))
        (weightList (cadr (cdddr netList)))
        (deltaWeightList (caddr (cdddr netList)))
        (deltaList (cadddr (cdddr netList)))
        (oldDeltaWeightList (cadddr (cdddr (cdr netList))))
        (alpha (cadddr (cdddr (cddr netList))))
        (beta (cadddr (cdddr (cdddr netList))))
        (inputs '())
        (targetOutputs '())
        (iDimension '())
        (jDimension '())
        (iActivationVector '())
        (jActivationVector '())
        (weightArray '())
        (sumOfProductsArray '())
        (iDeltaVector '())
        (jDeltaVector '())
        (deltaWeightArray '())
        (oldDeltaWeightArray '())
        (sum '())
        (iSumOfProductsArray '())
        (error '())
        (outputError 0)
        (delta '())
        (eida '())
        (inputNoise 0)
        (n 0))
    
    ;;
    ; Zero out deltas:
    ;;
    (do ((n 0 (+ n 1)))
        ((equal? n (- nLayers 1)))
      (let* ((dw (list-ref deltaList n))
             (len1 (vector-length dw)))
        (do ((i 0 (+ i 1)))
            ((equal? i len1))
          (vector-set! dw i 0.0))))
    
    ;;
    ; Zero out delta weights:
    ;;
    (do ((n 0 (+ n 1)))
        ((equal? n (- nLayers 1)))
      (let* ((dw (list-ref deltaWeightList n))
             (len1 (2D-array-length dw 0))
             (len2 (2D-array-length dw 1)))
        (do ((i 0 (+ i 1)))
            ((equal? i len1))
          (do ((j 0 (+ j 1)))
              ((equal? j len2))
            (2D-array-set! dw i j 0.0)))))
    
    (set! inputNoise *delta-default-input-noise-value*)
    
    ;;
    ;  Main loop on training examples:
    ;;
    
    (do ((tl-list trainList (cdr tl-list)))
        ((null? tl-list))
      (let ((tl (car tl-list)))
        (set! inputs (car tl))
        (set! targetOutputs (cadr tl))
        
        (if *delta-rule-debug-flag*
            (display (list "Current targets:" targetOutputs)))

        ; get the size of the input slab:
        (set! iDimension (car sizeList)) 

        ; get array of input activations:
        (set! iActivationVector (car activationList))

        ; copy training inputs to input slab:
        (do ((i 0 (+ i 1)))
            ((equal? i iDimension))
          (vector-set!
           iActivationVector
           i
           (+ 
            (list-ref inputs i)
            (frandom (- inputNoise) inputNoise))))
        
        ;;
        ; Propagate activation through all of the slabs:
        ;;
        ; update layer i to layer flowing to layer j
        (do ((n-1 0 (+ n-1 1)))
            ((equal? n-1 (- nLayers 1)))
          (set! n (+ n-1 1))
          ; get the size of the j'th layer:
          (set!
           jDimension
           (list-ref sizeList n))
          ; activation array for slab j:
          (set!
           jActivationVector
           (list-ref activationList n))
          (set! weightArray (list-ref weightList n-1))
          (set!
           sumOfProductsArray
           (list-ref sumOfProductsList n-1))
          ; process each neuron in slab j:
          (do ((j 0 (+ j 1)))
              ((equal? j jDimension))
            (set! sum 0.0) ; init sum of products to zero
            ; to get activation from each neuron in
            ; previous slab:
            (do ((i 0 (+ i 1)))
                ((equal? i iDimension))
              (set!
               sum
               (+
                sum
                (* (2D-array-ref weightArray i j)
                   (vector-ref iActivationVector i)))))
            ; save sum of products:
            (vector-set! sumOfProductsArray j sum)
            (vector-set! jActivationVector j (Sigmoid sum)))
          ; reset index for next slab pair:
          (set! iDimension jDimension)
          (set! iActivationVector jActivationVector))
        ;;
        ; Activation is  spread through the network
        ; and sum of products calculated.
        ; Now modify the weights in the network
        ; using back error propagation. Start
        ; by calculating the error signal for each
        ; neuron in the output layer:
        ;;
        ; size of last layer:
        (set! jDimension (list-ref sizeList (- nLayers 1)))
        (set!
         jActivationVector
         (list-ref activationList (- nLayers 1)))
        (set! jDeltaVector (list-ref deltaList (- nLayers 2)))
        (set!
         sumOfProductsArray
         (list-ref sumOfProductsList (- nLayers 2)))
        (set! outputError 0)
        (do ((j 0 (+ j 1)))
            ((equal? j jDimension))
          (set!
           delta
           (-
            (list-ref targetOutputs j)
            (vector-ref jActivationVector j)))
          (set! outputError (+ outputError (abs delta)))
          (vector-set!
           jDeltaVector
           j
           (+
            (vector-ref jDeltaVector j)
            (*
             delta
             (dSigmoid (vector-ref sumOfProductsArray j))))))
        ;;
        ; Now calculate the backpropagated error signal
        ; for all hidden slabs:
        ;;
        (do ((nn 0 (+ nn 1)))
            ((equal? nn (- nLayers 2)))
          (set! n (- nLayers 3 nn))
          (set! iDimension (list-ref sizeList (+ n 1)))
          (set!
           iSumOfProductsArray
           (list-ref sumOfProductsList n))
          (set! iDeltaVector (list-ref deltaList n))
          (do ((i 0 (+ i 1)))
              ((equal? i iDimension))
            (vector-set! iDeltaVector i 0.0))
          (set! weightArray (list-ref weightList (+ n 1)))
          (do ((i 0 (+ i 1)))
              ((equal? i iDimension))
            (set! error 0.0)
            (do ((j 0 (+ j 1)))
                ((equal? j jDimension))
              (set!
               error
               (+
                error
                (*
                 (vector-ref jDeltaVector j)
                 (2D-array-ref weightArray i j)))))
            (vector-set! 
             iDeltaVector 
             i
             (+
              (vector-ref iDeltaVector i)
              (*
               error
               (dSigmoid
                (vector-ref iSumOfProductsArray i))))))
          (set! jDimension iDimension)
          (set! jDeltaVector iDeltaVector))
        
        ;;
        ; Update all delta weights in the network:
        ;;
        (set! iDimension (car sizeList))
        (do ((n 0 (+ n 1)))
            ((equal? n (- nLayers 1)))
          (set! iActivationVector (list-ref activationList n))
          (set! jDimension (list-ref sizeList (+ n 1)))
          (set! jDeltaVector (list-ref deltaList n))
          (set! deltaWeightArray (list-ref deltaWeightList n))
          (set! weightArray (list-ref weightList n))
          (set! eida (list-ref eidaList n))
          ;; new stuff:
          (set! iDimension (vector-length iActivationVector))
          (set! jDimension (vector-length jDeltaVector))
          
          (do ((j 0 (+ j 1)))
              ((equal? j jDimension))
            (do ((i 0 (+ i 1)))
                ((equal? i iDimension))
              (set!
               delta
               (*
                eida
                (vector-ref jDeltaVector j)
                (vector-ref iActivationVector i)))
              (2D-array-set!
               DeltaWeightArray i j
               (+
                (2D-array-ref DeltaWeightArray i j)
                delta)))) ; remember delta weight change
          
          (set! iDimension jDimension))
        
        (if (equal? plotFlag '(#t))
            (DeltaPlot netList)))
      
      ;;
      ; Update all weights in the network:
      ;;
      (set! iDimension (car sizeList))
      (do ((n 0 (+ n 1)))
          ((equal? n (- nLayers 1)))
        (set! iActivationVector (list-ref activationList n))
        (set! jDimension (list-ref sizeList (+ n 1)))
        (set! jDeltaVector (list-ref deltaList n))
        (set! deltaWeightArray (list-ref deltaWeightList n))
        (set!
         oldDeltaWeightArray
         (list-ref oldDeltaWeightList n))
        (set! weightArray (list-ref weightList n))
        (do ((j 0 (+ j 1)))
            ((equal? j jDimension))
          (do ((i 0 (+ i 1)))
              ((equal? i iDimension))
            (2D-array-set!
             weightArray i j
             (+ (2D-array-ref weightArray i j)
                (*
                 alpha
                 (2D-array-ref deltaWeightArray i j))
                (*
                 beta
                 (2D-array-ref oldDeltaWeightArray i j))))
            ; save current delta weights for next
            ; momentum term:
            (2D-array-set!
             oldDeltaWeightArray i j
             (2D-array-ref deltaWeightArray i j))))
        (set! iDimension jDimension)))
    ;; return the average error of the output neurons:
    (/ outputError jDimension)))

;;
 ;  Utility for using a trained neural network in the
 ;  recall mode. The first argument to this function
 ;  is a network definition (as returned from NewDeltaNetwork)
 ;  and the second argument is a list of input neuron
 ;  activation values to drive through the network.
 ;;
(define DeltaRecall
 (lambda (netList inputs)
  (let ((nLayers (car netList))
        (sizeList (cadr netList))
        (activationList (caddr netList))
        (weightList (cadr (cdddr netList)))
        (iDimension '())
        (jDimension '())
        (iActivationVector '())
        (jActivationVector '())
        (n '())
        (weightArray '())
        (returnList '())
        (sum '()))
    ; get the size of the input slab:
    (set! iDimension (car sizeList))
    ; get array of input activations:
    (set! iActivationVector (car activationList))
    ; copy training inputs to input slab:
    (do ((i 0 (+ i 1)))
        ((equal? i iDimension))
      (vector-set! iActivationVector i (list-ref inputs i)))
    ; update layer j to layer i
    (do ((n-1 0 (+ n-1 1)))
        ((equal? n-1 (- nLayers 1)))
      (set! n (+ n-1 1))
      ; get the size of the j'th layer:
      (set! jDimension (list-ref sizeList n))
      ; activation array for slab j:
      (set! jActivationVector (list-ref activationList n))
      (set! weightArray (list-ref weightList n-1))
      ; process each neuron in slab j:
      (do ((j 0 (+ j 1)))
          ((equal? j jDimension))
        (set! sum 0.0) ; init sum of products to zero
        ; get activation from each neuron in previous slab:
        (do ((i 0 (+ i 1)))
            ((equal? i iDimension))
          (set!
           sum
           (+
            sum
            (*
             (2D-array-ref weightArray i j)
             (vector-ref iActivationVector i)))))
        (if *delta-rule-debug-flag* 
            (display (list "sum=" sum)))
        (vector-set! jActivationVector j (Sigmoid sum)))
      ; get ready for next slab pair:
      (set! iDimension jDimension)
      (set! iActivationVector jActivationVector))
    (do ((j 0 (+ j 1)))
        ((equal? j jDimension))
      (set!
       returnList
       (append
        returnList
        (list (vector-ref jActivationVector j)))))
    returnList)))

;;
 ; Utilities to plot a network
 ;;

(define plotActivations
 (lambda (title x y data dmin dmax)
  (let ((size (vector-length data))
        (ypos 0) (xpos x))
    (plot-string x (- y 60) title)
    (do ((i 0 (+ i 1)))
        ((equal? i size))
      (if (< size 20)
          (begin
            (set! ypos y)
            (set! xpos (+ x (* i 29))))
          (if (< i (/ size 2))
              (begin
                (set! ypos (- y 7))
                (set! xpos (+ x (* i 29))))
              (begin
                (set! ypos (+ y 2))
                (set! xpos (+ x (* (- i (/ size 2)) 29))))))
      (plot-fill-rect-gray-scale
       (truncate xpos)
       (truncate ypos)
       28 28
       (truncate
        (*
         (/
          (- (vector-ref data i) dmin)
          (- dmax dmin))
         255)))))))

(define plotWeights
 (lambda (title x y data dmin dmax deltaWeights)
  (let ((Xsize (2D-array-length data 0))
        (YSize (2D-array-length data 1)))
    ; don't try to plot very large weight sets:
    (if (< (* Xsize Ysize) 200)
        (begin
          (plot-string (+ x 20) (- y 60) title)
          (do ((i 0 (+ i 1)))
              ((equal? i xSize))
            (do ((j 0 (+ j 1)))
                ((equal? j Ysize))
              (plot-fill-rect-gray-scale
               (+ x (* i 29)) (+ y (* j 29)) 28 28
               (truncate
                (*
                 (/
                  (- (2D-array-ref data i j) dmin)
                  (- dmax dmin)) 255)))
              (plot-fill-rect-gray-scale
               (+ (* Xsize 28) 30 x (* i 29))
               (+ y (* j 29))
               28 28
               (truncate
                (*
                 (/
                  (- (2D-array-ref deltaWeights i j) -.05)
                  (- .05 -.05))
                 255))))))))))

(define DeltaPlot
 (lambda (netList)
  (let ((nLayers (car netList))
        (sizeList (cadr netList))
        (activationList (caddr netList))
        (sumOfProductsList (car (cdddr netList)))
        (weightList (cadr (cdddr netList)))
        (deltaWeightList (caddr (cdddr netList)))
        (minScale -0.3)
        (maxScale 0.3)
        (n 0)
        (y-start 0))
    (plot-string 100 960 "Delta Network")
    (set! y-start 100)
    (plotActivations "slab1"
                     100
                     y-start
                     (list-ref ActivationList 0)
                     -0.5
                     0.5)
    (do ((n-1 0 (+ n-1 1)))
        ((equal? n-1 (- nLayers 1)))
      (set! n (+ n-1 1))
      (if (equal? n (- nLayers 1))
          (begin
            (set! minScale -0.2)
            (set! maxScale 0.2))) ; scale up output display
      (plotActivations
       (list-ref '("slab1" "slab2" "slab3" "slab4" "slab5") n)
       100 ; x location for subplot
       (+ y-start (* n 400)) ; y location for subplot
       ; data to plot as gray scale:
       (list-ref ActivationList n)
       minScale
       maxScale))
    (if (< nLayers 4)
        (begin
          (set! y-start -140)
          (do ((n 0 (+ n 1)))
              ((equal? n (- nLayers 1)))
            (set! y-start (+ y-start 310))
            (plot-string 100 y-start "Weights and Delta Weights")
            (set! y-start (+ y-start 90))     
            (plotWeights
             (list-ref
              '("slab1 -> slab2"  "slab2 -> slab3"
                "slab3 -> slab4")
              n)
             100 y-start ; x,y position of subplot
             (list-ref WeightList n)
             -1.0 1.0
                 (list-ref deltaWeightList n))))))))
;;
 ; Calculate Sigmoid and derivative of Sigmoid functions:
 ;;

(define Sigmoid
 (lambda (x)
  (/ 1.0 (+ 1.0 (exp (- x))))))

(define dSigmoid
 (lambda (x)
  (let ((temp (Sigmoid x)))
    (* temp (- 1.0 temp)))))


;;
; Save a Delta Network to a disk file:
;;

(define WriteDeltaNetwork
 (lambda (fileName netList)
  (let ((fileStream (open-output-file fileName))
        (nLayers (car netList))
        (sizeList (cadr netList))
        (activationList (caddr netList))
        (weightList (cadr (cdddr netList)))
        (deltaWeightList (caddr (cdddr netList)))
        (oldDeltaWeightList (car (cddddr (cdddr netList))))
        (alpha (cadr (cddddr (cdddr netList))))
        (beta (caddr (cddddr (cdddr netList)))))
    ;;
    ; Write out header:
    ;;
    (write-string ";; number of layers" fileStream)
    (newline fileStream)
    (write nLayers fileStream)
    (newline fileStream)
    (write-string ";; number of neurons in each layer" fileStream)
    (newline fileStream)
    (write sizeList fileStream)
    (newline fileStream)

    ;;
    ; Write out activations:
    ;;

    (write-string ";; activation values" fileStream)
    (newline fileStream)

    (do ((n 0 (+ n 1)))
        ((equal? n (- nLayers 1)))
      (do ((i 0 (+ i 1)))
          ((equal? i (list-ref sizeList n)))
        (write
         (vector-ref
          (list-ref activationList n) i)
         fileStream)
        (newline fileStream)))
    ;;
    ; Write out weights:
    ;;

    (write-string ";; weights" fileStream)
    (newline fileStream)

    (do ((n 0 (+ n 1)))
        ((equal? n (- nLayers 1)))
      (let ((w (list-ref weightList n)))
        (do ((i 0 (+ i 1)))
            ((equal? i (2D-array-length w 0)))
          (do ((j 0 (+ j 1)))
              ((equal? j (2D-array-length w 1)))
            (write (2D-array-ref w i j) fileStream)
            (newline fileStream)))))
    ;;
    ; Write out delta weights:
    ;;

    (write-string ";; delta weights" fileStream)
    (newline fileStream)

    (do ((n 0 (+ n 1)))
        ((equal? n (- nLayers 1)))
      (let ((w (list-ref deltaWeightList n)))
        (do ((i 0 (+ i 1)))
            ((equal? i (2D-array-length w 0)))
          (do ((j 0 (+ j 1)))
              ((equal? j (2D-array-length w 1)))
            (write (2D-array-ref w i j) fileStream)
            (newline fileStream)))))

    ;;
    ; Write out old delta weights:
    ;;

    (write-string ";; old delta weights" fileStream)
    (newline fileStream)

    (do ((n 0 (+ n 1)))
        ((equal? n (- nLayers 1)))
      (let ((w (list-ref oldDeltaWeightList n)))
        (do ((i 0 (+ i 1)))
            ((equal? i (2D-array-length w 0)))
          (do ((j 0 (+ j 1)))
              ((equal? j (2D-array-length w 1)))
            (write (2D-array-ref w i j) fileStream)
            (newline fileStream)))))
    ;;
    ; Write alpha, beta terms (used for momentum):
    ;;

    (write-string ";; alpha" fileStream)
    (newline fileStream)

    (write alpha fileStream)
    (newline fileStream)

    (write-string ";; beta" fileStream)
    (newline fileStream)

    (write beta fileStream)
    (newline fileStream)

    
    (close-output-port fileStream))))

;;                     
; Read a delta network from a disk file:
;;

(define ReadDeltaNetwork
 (lambda (fileName)
  (let ((fileStream (open-input-file fileName))
        (numLayers '())
        (sizeList '())
        (a-list '())
        (s-list '())
        (w-list '())
        (dw-list '())
        (old-dw-list '())
        (d-list '())
        (alpha 0.0)
        (beta 0.0))
    
    ;;
    ; Read in header:
    ;;
    (set! numLayers (read fileStream))
    (set! sizeList (read fileStream))
    
    ;;
    ; Allocate array storage:
    ;;
    (set! a-list
          (map
           (lambda (size) (make-vector size 0.0))
           sizeList))
    (set! s-list
          (map
           (lambda (size) (make-vector size 0.0))
           (cdr sizeList)))
    (set! d-list
          (map
           (lambda (size) (make-vector size 0.0))
           (cdr sizeList)))
    (do ((i 0 (+ i 1)))
        ((equal? i (- numLayers 1)))
      (set!
       w-list
       (cons 
        (list (list-ref sizeList i)
              (list-ref sizeList (+ i 1))) w-list)))
    (set! w-list
          (map
           (lambda (size) (make-2D-array (car size) (cadr size)))
           (reverse w-list)))
    
    (do ((i 0 (+ i 1)))
        ((equal? i (- numLayers 1)))
      (set!
       dw-list
       (cons 
        (list
         (list-ref sizeList i)
         (list-ref sizeList (+ i 1)))
        dw-list)))
    
    (do ((i 0 (+ i 1)))
        ((equal? i (- numLayers 1)))
      (set!
       old-dw-list
       (cons
        (list
         (list-ref sizeList i)
         (list-ref sizeList (+ i 1)))
        old-dw-list)))
    
    (set! dw-list
          (map
           (lambda (size) (make-2D-array (car size) (cadr size)))
           (reverse dw-list)))
    
    (set! old-dw-list
          (map
           (lambda (size) (make-2D-array (car size) (cadr size)))
           (reverse old-dw-list)))
    
    ;;
    ; Read in activations:
    ;;
    (do ((n 0 (+ n 1)))
        ((equal? n (- numLayers 1)))
      (do ((i 0 (+ i 1)))
          ((equal? i (list-ref sizeList n)))
        (vector-set!
         (list-ref a-list n) i (read fileStream))))
    
    ;;
    ; Read in weights:
    ;;
    (do ((n 0 (+ n 1)))
        ((equal? n (- numLayers 1)))
      (let ((w (list-ref w-list n)))
        (do ((i 0 (+ i 1)))
            ((equal? i (2D-array-length w 0)))
          (do ((j 0 (+ j 1)))
              ((equal? j (2D-array-length w 1)))
            (2D-array-set! w i j (read fileStream))))))
    
    ;;
    ; Read in delta weights:
    ;;
    (do ((n 0 (+ n 1)))
        ((equal? n (- numLayers 1)))
      (let ((w (list-ref dw-list n)))
        (do ((i 0 (+ i 1)))
            ((equal? i (2D-array-length w 0)))
          (do ((j 0 (+ j 1)))
              ((equal? j (2D-array-length w 1)))
            (2D-array-set! w i j (read fileStream))))))
    
    ;;
    ; Read in old delta weights:
    ;;
    (do ((n 0 (+ n 1)))
        ((equal? n (- numLayers 1)))
      (let ((w (list-ref old-dw-list n)))
        (do ((i 0 (+ i 1)))
            ((equal? i (2D-array-length w 0)))
          (do ((j 0 (+ j 1)))
              ((equal? j (2D-array-length w 1)))
            (2D-array-set! w i j (read fileStream))))))
    
    (set! alpha (read fileStream))
    (set! beta (read fileStream))
    
    (close-input-port fileStream)
    (list numLayers sizeList a-list s-list
          w-list dw-list d-list
          old-dw-list alpha beta))))

;;
 ; Throw away test functions for two, three and
 ;  four layer networks:
 ;;

(define (test2 . old-network)
  (let ((RMSerror 0.0))
    (if (null?  old-network)
        (begin
          (display "Creating a new network")
          (newline)
          ; specify a two layer network (2x2)
          (set! old-network (newdeltanetwork '(2 2))))
        (begin
          (display "Using old network:")
          (newline)
          (set! old-network (car old-network))
          (pp old-network)
          (newline)))
    (do ((ii 0 (+ ii 1)))
        ((equal? ii 20))
      (set!
       RMSerror
       (deltalearn
        old-network
        '(((1 0) (0 1))
          ((0 1) (1 0)))))
      (if (equal? (modulo ii 5) 0) ;; print out every 5 cycles
          (begin
            (display "....training cycle \#")
            (display ii)
            (display " RMS error = ")
            (display RMSerror)
            (newline))))
    old-network))

; (define save-net)
; (set! save-net (test2))
; (pp save-net)
; (test2 save-net)  ;; try restarting the learning process

(define (test3 . old-network)
  (let ((RMSerror 0.0))
    (if (null? old-network)
        (begin
          (display "Creating a new network")
          (newline)
          ; specify a two layer network (2x2)
          (set! old-network (newdeltanetwork '(5 4 5))))
        (begin
          (display "Using old network:")
          (newline)
          (set! old-network (car old-network))
          (pp old-network)
          (newline)))
    (do ((ii 0 (+ ii 1)))
        ((equal? ii 1000))
      (set!
       RMSerror
       (deltalearn
        old-network
        '(((1 0 0 0 0) (0 1 0 0 0))
          ((0 1 0 0 0) (0 0 1 0 0))
          ((0 0 1 0 0) (0 0 0 1 0))
          ((0 0 0 1 0) (0 0 0 0 1))
          ((0 0 0 0 1) (1 0 0 0 0)))))
      (if (equal? (modulo ii 5) 0) ;; print out every 5 cycles
          (begin
            (display "....training cycle \#")
            (display ii)
            (display " RMS error = ")
            (display RMSerror)
            (newline))))
    temp))

; (test3)

(define (test4 . old-network)
  (let ((RMSerror 0.0))
    (if (null?  old-network)
        (begin
          (display "Creating a new network")
          (newline)
          ; specify a two layer network (2x2)
          (set! old-network (newdeltanetwork '(4 5 5 4))))
        (begin
          (display "Using old network:")
          (newline)
          (set! old-network (car old-network))
          (pp old-network)
          (newline)))
    (do ((ii 0 (+ ii 1)))
        ((equal? ii 1200))
      (set!
       RMSerror
       (deltalearn
        old-network
        '(((1 0 0 0) (0 1 0 0))
          ((0 1 0 0) (0 0 1 0))
          ((0 0 1 0) (0 0 0 1))
          ((0 0 0 1) (1 0 0 0)))))
      (if (equal? (modulo ii 5) 0) ;; print out every 5 cycles
          (begin
            (display "....training cycle \#")
            (display ii)
            (display " RMS error = ")
            (display RMSerror)
            (newline))))))
; (test4)

;;
 ; Throw away test functions for graphics support:
 ;;


(define (test-gr . restart)
  (let ((plotFlag '())
        (temp #f))
    (open-gr)
    (clear-plot)
    (let ((RMSerror 0.0))
      (if (null? restart)
          (set! temp (newdeltanetwork '(5 4 5)))
          (set! temp (car restart)))
      (do ((ii 0 (+ ii 1)))
          ((equal? ii 500))
        (set!
         RMSerror
         (deltalearn
          temp
          '(((1 0 0 0 0) (0 1 0 0 0))
            ((0 1 0 0 0) (0 0 1 0 0))
            ((0 0 1 0 0) (0 0 0 1 0))
            ((0 0 0 1 0) (0 0 0 0 1))
            ((0 0 0 0 1) (1 0 0 0 0)))
          plotFlag))
        ;; print out every 5 cycles:
        (if (equal? (modulo ii 10) 0) 
            (begin
              (set! plotFlag #t)
              (display "....training cycle \#")
              (display ii)
              (display " RMS error = ")
              (display RMSerror)
              (newline))
            (set! plotFlag #f))))
    temp))  ;; return neural network object for use in restarting

; (open-gr)  ;; just do this once
; (test-gr)

;;
 ; Throw away test code for testing save/load options:
 ;;

; (define save-net)
; (set! save-net (test2))
; (WriteDeltaNetwork "test.net" save-net)
; (set! save-net (ReadDeltaNetwork "test.net"))
; (pp save-net)
; (test2 save-net)  ;; try restarting the learning process
