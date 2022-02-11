(define (square x) (* x x))

(define (max x y)
 (if (> x y) x y))

(define (sqrt-iter guess x)
 (if (good-enough? guess x)
  guess
  (sqrt-iter (improve guess x)
   x)))

(define (improve guess x)
 (average guess (/ x guess)))

(define (average x y)
 (/ (+ x y) 2))

(define (good-enough? guess x)
 (< (abs (- (square guess) x)) (min (/ x 1000) 0.001)))

(define (good-enough? guess x)
 (= (improve guess x) guess))

(define (sqrt x)
 (sqrt-iter 1.0 x))

(define (improve3 y x)
 (/ (+ (/ x (square y)) (* 2 y)) 3))

(define (good-enough3? guess x)
 (= (improve3 guess x) guess))

(define (cube-root-iter guess x)
 (if (good-enough3? guess x)
  guess
  (cube-root-iter (improve3 guess x)
   x)))

(define (cube-root x)
 (cube-root-iter 1.0 x))

(sqrt 9999999999999)

(cube-root 125)

(good-enough3? (improve3 (improve3 1.0 125) 125) 125)

