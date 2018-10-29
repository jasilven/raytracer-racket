#lang racket/base

(require racket/class
         racket/dict
         racket/draw
         racket/math
         "vector.rkt")

(struct sphere (center radius color))
(define *bg-color* (bytes 255 255 255 255))
(define *light* #(-15 15 20))

;; return average color for all pixels (4 bytes/pixel, argb)
(define (avg-color pixels)
  (define count (/ (bytes-length pixels) 4))
  (define-values (r g b) (values 0 0 0))
  (for ([i (in-range 0 (* 4 count) 4)])
    (set! r (+ r (bytes-ref pixels (+ 1 i))))
    (set! g (+ g (bytes-ref pixels (+ 2 i))))
    (set! b (+ b (bytes-ref pixels (+ 3 i)))))
  (bytes 255
         (inexact->exact (round (/ r count)))
         (inexact->exact (round (/ g count)))
         (inexact->exact (round (/ b count)))))

;; return vector of hitpoint in sphere s or #f if no hit
(define (hit-point ray s)
  (define s-center (sphere-center s))
  (define proj (vec-dot ray s-center))
  (cond
    [(negative? proj) #f]
    [else
     (define dist (sqrt (- (expt (vec-length s-center) 2)
                           (expt proj 2))))
     (if (> dist (sphere-radius s))
         #f
         (vec* ray (- proj
                      (sqrt (- (expt (sphere-radius s) 2)
                               (expt dist 2))))))]))

;; raytrace the whole scene and return bitmap
(define (raytrace w h fov s)
  (define bm (make-bitmap w h))
  (define tanfov2 (tan (/ (* (/ fov 2) pi) 180)))
  (for ([y (in-range h)])
    (define yy (* (- 1 (* 2 (/ (+ y 0.5) h))) tanfov2))
    (for ([x (in-range w)])
      (define xx (/ (* (- (* 2 (+ x 0.5)) w) tanfov2) h))
      (send bm set-argb-pixels  x y 1 1 (get-pixel xx yy s))))
  bm)

(define (factor cosa)
  (define fac (* 255 (/ (expt 2 (* 10 (/ (add1 cosa) 2))) 1024)))
  (inexact->exact (round fac)))

(define (get-pixel x y s)
  (define hit (hit-point (vec-unit (vector x y -1)) s))
  (cond
    [(not hit) *bg-color*]
    [else
     (define l-sub-hp (vec- *light* hit))
     (define cosa (/ (vec-dot (vec-unit (vec- hit (sphere-center s))) l-sub-hp)
                     (vec-length l-sub-hp)))
     (avg-color (bytes-append (sphere-color s)
                              #"\255"
                              (make-bytes 3 (factor cosa))))]))

;; TODO implement antialias for tracer
;; (define (antialias bm)
;;   (for ([y (in-range (send bm get-height))])
;;     (define yy (* (- 1 (* 2 (/ (+ y 0.5) h))) tanfov2))
;;     (for ([x (in-range (send bm get-width))])
;;       (define xx (/ (* (- (* 2 (+ x 0.5)) w) tanfov2) h))
;;       (send bm set-argb-pixels  x y 1 1 (get-pixel xx yy s)))))

(define (main)
  (define s (sphere #(0 0 -9) 3 #"\255\240\0\0"))
  (define fname "out.png")

  (printf "Raytracing:\n  ")
  (define bm (time (raytrace 640 480 60 s)))
  (printf "Writing image to ~a\n" fname)
  (send bm save-file fname 'png)
  bm)

(main)

;; tests
(module+ test
  (require rackunit
           rackunit/text-ui)

  (run-tests
   (test-suite
    "all tests"
    (test-suite
     "avg-color"
     (check-equal? (avg-color #"\377\2\2\2\377\4\4\4") #"\377\3\3\3")
     (check-equal? (avg-color (bytes 255 3 3 3 255 5 5 5 255 1 1 1)) (bytes 255 3 3 3) )
     (check-equal? (avg-color (bytes 255 3 3 3 255 6 6 6 255 1 1 1)) (bytes 255 3 3 3) )))))
