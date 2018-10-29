#lang racket/base

(provide vec-
         vec+
         vec*
         vec-dot
         vec-length
         vec-unit)

(define (vec-calc op vec1 vec2)
  (for/vector ([i (in-range (vector-length vec1))])
    (op (vector-ref vec1 i)
        (vector-ref vec2 i))))

(define (vec+ vec1 vec2)
  (vec-calc + vec1 vec2))

(define (vec- vec1 vec2)
  (vec-calc - vec1 vec2))

(define (vec* vec i)
  (case (vector-length vec)
    [(1) (vector (* (vector-ref vec 0) i))]
    [(2) (vector (* (vector-ref vec 0) i)
                 (* (vector-ref vec 1) i))]
    [(3) (vector (* (vector-ref vec 0) i)
                 (* (vector-ref vec 1) i)
                 (* (vector-ref vec 2) i))]
    [else (error "unsupported vector length")]))

(define (vec-dot vec1 vec2)
  (for/sum ([i (in-range (vector-length vec1))])
    (* (vector-ref vec1 i)
       (vector-ref vec2 i))))

(define (vec-length vec)
  (sqrt (vec-dot vec vec)))

(define (vec-unit vec)
  (define vdotv (vec-dot vec vec))
  (if (= 0 vdotv)
      (make-vector (vec-length vec))
      (vec* vec (/ 1 (sqrt vdotv)))))

(module+ test
  (require rackunit
           rackunit/text-ui
           racket/function)

  (run-tests
   (test-suite
    "all tests"
    (check-equal? (vec+ #(1 1 1) #(2 2 2)) #(3 3 3) "vector +")
    (check-equal? (vec- #(3 3 3) #(2 2 2)) #(1 1 1) "vector -")
    (check-equal? (vec* #(3 3 3) 2) #(6 6 6) "vector *")
    (check-equal? (vec+ #(3 3 3) #(2 2 2)) #(5 5 5) "vector +")
    (check-equal? (vec- #(3 3 3) #(2 2 2)) #(1 1 1) "vector -")
    (check-equal? (vec-dot #(1 1 1) #(2 2 2)) 6 "vector dot")
    (check-equal? (vec-dot #(3 3 3) #(2 2 2)) 18 "vector dot")
    (check-equal? (vec-length #(1 0 0)) 1 "vector length")
    (check-equal? (vec-length #(0 0 0)) 0 "vector length")
    (check-equal? (vec-length #(1 1 0)) (sqrt 2) "vector length")
    (check-equal? (vec-length #(1 2 3)) (sqrt 14) "vector length")
    (check-equal? (vec-unit #(1 0 0)) #(1 0 0) "vector unit")
    (check-equal? (vec-unit #(4 0 0)) #(1 0 0) "vector unit")
    (check-equal? (vec-unit #(4 0 0)) #(1 0 0) "vector unit")
    (check-equal? (vec-unit #(4 8))
                  (vector (/ 4 (sqrt 80)) (/ 8 (sqrt 80)))
                  "vector unit")

    (check-exn exn:fail? (thunk (vec* #(1 2 3 4) 4)) "vector *")
    (check-equal? #(4.0 4.0 4.0) (vec*
                                  (vec-unit #(4 4 4))
                                  (vec-length #(4 4 4))) "vector unit"))))
