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
