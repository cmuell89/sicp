#lang racket
(define square
  (lambda (x) (* x x)))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s
(define pi 3.14159)

;; Problem 1

(define position
  (lambda (a v u t)
    (+ (* .5 a (square t))
       (* v t)
       u)))

;; you need to complete this procedure, then show some test cases("Problem 1 Tests:")
(position 0 0 0 0)
(position 0 0 20 0)
(position 0 5 10 10)
(position 2 2 2 2)
(position 5 5 5 5)


;; Problem 2
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0000000000000000000001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

;; Implementation Newton's Successive Approximations for Square Root.
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define radicand-check
  (lambda (radicand)
    (if (>= radicand 0)
        true
        false)))

(define qe-radicand
  (lambda (a b c)
    (- (square b) (* 4 a c))))

(define root1
  (lambda (a b c)
    (if (radicand-check (qe-radicand a b c)) 
        (/ (+ (* -1 b)
              (sqrt(-
                    (square b)
                    (* 4 a c))))
            (* 2 a))
        "imaginary value")))
    
(define root2
  (lambda (a b c)
    (if (radicand-check (qe-radicand a b c)) 
        (/ (- (* -1 b)
              (sqrt(-
                    (square b)
                    (* 4 a c))))
            (* 2 a))
        "imaginary value")))

(define bigger-root
  (lambda (root1 root2)
    (if (and (equal? root1 "imaginary value") (equal? root2 "imaginary value"))
        "imaginary roots"
        (if (equal? root1 "imaginary value")
            root2
            (if (equal? root2 "imaginary value")
                root1
                (if (> root1 root2)
                    root1
                    root2))))))

(define quadratic-eq
  (lambda (a b c)
    (bigger-root (root1 a b c) (root2 a b c))))
    
;; complete these procedures and show some test cases

(quadratic-eq 1 0 -1) ; 1
(quadratic-eq -1 0 1) ; -1
(quadratic-eq 5 6 7) ; "imaginary roots"

;; Problem 3

(define time-to-impact
  (lambda (vertical-velocity elevation)
    (bigger-root (root1 (* .5 -9.8) vertical-velocity elevation)
                 (root2 (* .5 -9.8) vertical-velocity elevation))))



(time-to-impact 0 4.9) ; should be 1 for 1 second
(time-to-impact 0 0); approximately zero

;; Note that if we want to know when the ball drops to a particular height r 
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (time-to-impact vertical-velocity (- elevation target-elevation))))

(time-to-height 0 4.9 0); should be 1 for 1 second

