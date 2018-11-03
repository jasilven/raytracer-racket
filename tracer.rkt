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

;; magic factor
(define (factor cosa)
  (define fac (* 255 (/ (expt 2 (* 10 (/ (add1 cosa) 2))) 1024)))
  (inexact->exact (round fac)))

;; get pixel for final image
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
;; antialias SSAA
(define (antialias bm)
  (define h (send bm get-height))
  (define w (send bm get-width))
  (define bm2 (make-bitmap (/ w 2) (/ h 2)))
  (define pixels (make-bytes 16))
  (for* ([y (in-range 0 h 2)]
         [x (in-range 0 w 2)])
    (send bm get-argb-pixels x y 2 2 pixels)
    (send bm2 set-argb-pixels (/ x 2) (/ y 2) 1 1 (avg-color pixels)))
  bm2)

(define (main)
  (define s (sphere #(0 0 -9) 3 #"\255\240\0\0"))
  (define fname "out.png")

  (printf "Raytracing:\n  ")
  (define bm (time (raytrace 1280 960 60 s)))

  (printf "Antialiasing:\n  ")
  (define bm2 (time (antialias bm)))

  (printf "Writing image to ~a\n" fname)
  (send bm2 save-file fname 'png)
  bm2)

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
