(define (atom? x)
    (and (not (pair? x)) (not (null? x))))

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
