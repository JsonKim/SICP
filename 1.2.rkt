(define (factorial n)
 (if (= n 1)
  1
  (* n (factorial (- n 1)))))

(define (factorial n)
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

