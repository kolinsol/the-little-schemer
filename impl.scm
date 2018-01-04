; ---------- CHAPTER 1 ----------

(define (atom? x)
    (and (not (pair? x)) (not (null? x))))

; ---------- CHAPTER 2 ----------

(define (lat? xs)
  (cond
    ((null? xs) #t)
    ((atom? (car xs)) (lat? (cdr xs)))
    (else #f)))

(define (member? x xs)
  (cond
    ((null? xs) #f)
    ((eq? x (car xs)) #t)
    (else (member? x (cdr xs)))))

; ---------- CHAPTER 3 ----------

(define (rember x xs)
  (cond
    ((null? xs) xs)
    ((eq? x (car xs)) (cdr xs))
    (else (cons (car xs) (rember x (cdr xs))))))

(define (firsts xs)
  (cond
    ((null? xs) '())
    (else 
      (cond
        ((null? (car xs)) (cons '() (firsts (cdr xs))))
        (else (cons (car (car xs)) (firsts (cdr xs))))))))

(define (insertr x y xs)
  (cond
    ((null? xs) '())
    ((eq? y (car xs)) (cons y (cons x (cdr xs))))
    (else (cons (car xs) (insertr x y (cdr xs))))))

(define (insertl x y xs)
  (cond
    ((null? xs) '())
    ((eq? y (car xs)) (cons x xs))
    (else (cons (car xs) (insertl x y (cdr xs))))))

(define (subst x y xs)
  (cond
    ((null? xs) '())
    ((eq? y (car xs)) (cons x (cdr xs)))
    (else (cons (car xs) (subst x y (cdr xs))))))

;TODO (->) reimplement using (member?)
(define (subst2 x y1 y2 xs)
  (cond
    ((null? xs) '())
    ((or
      (eq? (car xs) y1)
      (eq? (car xs) y2)) (cons x (cdr xs)))
    (else (cons (car xs) (subst2 x y1 y2 (cdr xs))))))

(define (multirember x xs)
  (cond
    ((null? xs) '())
    ((eq? x (car xs)) (multirember x (cdr xs)))
    (else (cons (car xs) (multirember x (cdr xs))))))

(define (multiinsertr x y xs)
  (cond
    ((null? xs) '())
    ((eq? y (car xs)) (cons y (cons x (multiinsertr x y (cdr xs)))))
    (else (cons (car xs) (multiinsertr x y (cdr xs))))))

(define (multiinsertl x y xs)
  (cond
    ((null? xs) '())
    ((eq? y (car xs)) (cons x (cons y (multiinsertl x y (cdr xs)))))
    (else (cons (car xs) (multiinsertl x y (cdr xs))))))

(define (multisubst x y xs)
  (cond
    ((null? xs) '())
    ((eq? y (car xs)) (cons x (multisubst x y (cdr xs))))
    (else (cons (car xs) (multisubst x y (cdr xs))))))

; ---------- CHAPTER 4 ----------

(define (add1 x)
  (+ x 1))

(define (sub1 x)
  (- x 1))

(define (zero? x)
  (= x 0))

(define (plus x y)
  (cond
    ((zero? y) x)
    (else (plus (add1 x) (sub1 y)))))

(define (minus x y)
  (cond
    ((zero? y) x)
    (else (minus (sub1 x) (sub1 y)))))

(define (addtup xs)
  (cond
    ((null? xs) 0)
    (else (+ (car xs) (addtup (cdr xs))))))

(define (mul x y)
  (cond
    ((zero? y) 0)
    (else (+ x (mul x (sub1 y))))))

; accidentally implementation
(define (tupconcat xs ys)
  (cond
    ((null? xs) ys)
    (else (cons (car xs) (tupconcat (cdr xs) ys)))))

(define (tupsum xs ys)
  (cond
    ((or (null? xs) (null? ys)) '())
    (else (cons (+ (car xs) (car ys)) (tupsum (cdr xs) (cdr ys))))))

(define (gt x y)
  (cond
    ((zero? x) #f)
    ((zero? y) #t)
    (else (gt (sub1 x) (sub1 y)))))

(define (lt x y)
  (cond
    ((zero? y) #f)
    ((zero? x) #t)
    (else (lt (sub1 x) (sub1 y)))))

(define (eq x y)
  (cond
  ((zero? x) (zero? y))
  ((zero? y) #f)
    (else (eq (sub1 x) (sub1 y)))))

; extra implementation
;
; (define (eq x y)
;   (cond
;     ((or
;        (lt x y)
;        (gt x y)) #f)
;     (else #t)))

(define (pow x y)
  (cond
    ((zero? y) 1)
    (else (* x (pow x (sub1 y))))))

(define (div x y)
  (cond
    ((lt x y) 0)
    (else (add1 (div (- x y) y)))))

(define (length xs)
  (cond
    ((null? xs) 0)
    (else (add1 (length (cdr xs))))))

(define (pick x xs)
  (cond
    ((null? xs) '())
    ((one? x) (car xs))
    (else (pick (sub1 x) (cdr xs)))))

(define (rempick x xs)
  (cond
    ((null? xs) '())
    ((one? x) (cdr xs))
    (else (cons (car xs) (rempick (sub1 x) (cdr xs))))))

(define (nonums xs)
  (cond
    ((null? xs) '())
    ((number? (car xs)) (nonums (cdr xs)))
    (else (cons (car xs) (nonums (cdr xs))))))

(define (allnums xs)
  (cond
    ((null? xs) '())
    ((number? (car xs)) (cons (car xs) (allnums (cdr xs))))
    (else (allnums (cdr xs)))))

(define (eqan? x y)
  (cond
    ((and (number? x) (number? y)) (= x y))
    ((or (number? x) (number? y)) #f)
    (else (eq? x y))))

(define (occur x xs)
  (cond
    ((null? xs) 0)
    ((eqan? x (car xs)) (add1 (occur x (cdr xs))))
    (else (occur x (cdr xs)))))

(define (one? x)
  (zero? (sub1 x)))

; ---------- CHAPTER 5 ----------


