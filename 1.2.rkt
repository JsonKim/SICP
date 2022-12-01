#lang sicp

(define (factorial n)
 (if (= n 1)
  1
  (* n (factorial (- n 1)))))

(define (factorial2 n)
 (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
 (if (> counter max-count)
  product
  (fact-iter (* counter product)
             (+ counter 1)
             max-count)))


(define (p a b)
 (if (= a 0)
  b
  (inc (p (dec a) b))))

(define (p a b)
 (if (= a 0)
  b
  (p (dec a) (inc b))))

(define (A x y)
 (cond ((= y 0) 0)
       ((= x 0) (* 2 y))
       ((= y 1) 2)
       (else (A (- x 1)
                (A x (- y 1))))))

(A 1 10)

(A 2 4)

(A 3 3)

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(h 4)

(define (count-change amount)
 (cc amount 5))

(define (cc amount kinds-of-coins)
 (cond ((= amount 0) 1)
       ((or (< amount 0) (= kinds-of-coins 0)) 0)
       (else (+ (cc amount
                    (- kinds-of-coins 1))
                (cc (- amount
                       (first-denomination kinds-of-coins))
                    kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
 (cond ((= kinds-of-coins 1) 1)
       ((= kinds-of-coins 2) 5)
       ((= kinds-of-coins 3) 10)
       ((= kinds-of-coins 4) 25)
       ((= kinds-of-coins 5) 50)))

(count-change 100)


(define (f2 f n d)
 (* d (f (- n d))))

(define (f n)
 (if (< n 3)
     n
     (+ f2 f n 1) (f2 f n 2) (f2 f n 3)))

(f 4)

(define (iter-f x1 x2 x3 x n)
 (if (= x n)
     x3
     (iter-f x2 x3 (+ x3 (* 2 x2) (* 3 x1)) (+ x 1) n)))


(define (f n)
 (cond ((= n 1) 1)
       ((= n 2) 2)
       (else (iter-f 0 1 2 2 n))))

(+ 2 3 4)


(define (pascal row col)
 (cond ((= col 1) 1)
       ((= col row) 1)
       (else (+ (pascal (- row 1) col) (pascal (- row 1) (- col 1))))))

(pascal 5 3)

(define (square x) (* x x))

(define (sqrt-iter guess x)
 (if (good-enough? guess x)
  guess
  (sqrt-iter (improve guess x)
   x)))

(define (sqrt x)
 (sqrt-iter 1.0 x))

(define (improve y x)
 (/ (+ (/ x (square y)) (* 2 y)) 3))

(define (good-enough? guess x)
 (= (improve guess x) guess))

(define (pi) (/ (+ 1 (sqrt 5)) 2))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


(fast-expt 2 8)

;; 1.16
(define (fast-expt2 b n)
  (define (fast-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (fast-iter a (square b) (/ n 2)))
          (else (fast-iter (* a b) b (- n 1)))))

  (fast-iter 1 b n))

(fast-expt2 2 8)

;; 1.17
(define (double n) (+ n n))

(define (halve n) (/ n 2))

(define (mult2 a b)
  (cond ((= b 0) 0)
        ((even? b) (double (mult2 a (halve b))))
        (else (+ a (mult2 a (- b 1))))))

(mult2 3 1)
(mult2 3 8)
(mult2 3 5)

;; 1.18
(define (fast-mult a b)
  (define (iter n a b)
    (cond ((= b 0) n)
          ((even? b) (iter n (double a) (halve b)))
          (else (iter (+ n a) a (- b 1)))))

  (iter 0 a b))

(fast-mult 3 0)
(fast-mult 3 1)
(fast-mult 3 2)
(fast-mult 3 8)
(fast-mult 3 5)

;; 1.19
(define (square n) (* n n))

(define (fib n)
 (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q)) ; p'
                   (+ (* 2 p q) (square q)); q'
                   (/ count 2)))
        (else (fib-iter 
                (+ (* b q) (* a q) (* a p))
                (+ (* b p) (* a q))
                p
                q
                (- count 1)))))

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)

(define (smallest-divisor n)
 (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(find-divisor 25 2)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 127 10)

;; 1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
