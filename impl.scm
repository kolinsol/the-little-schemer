; ---------- chapter 1 ----------

(define (atom? x)
    (and (not (pair? x)) (not (null? x))))

; ---------- chapter 2 ----------

(define (lat? xs)
  (cond
    ((null? xs) #t)
    ((atom? (car xs)) (lat? (cdr xs)))
    (else #f)))

(define (member? x xs)
  (cond
    ((null? xs) #f)
    ((equal? x (car xs)) #t)
    (else (member? x (cdr xs)))))

; ---------- chapter 3 ----------

(define (rember x xs)
  (cond
    ((null? xs) xs)
    ((equal? x (car xs)) (cdr xs))
    (else (cons (car xs) (rember x (cdr xs))))))

(define (fsts xs)
  (cond
    ((null? xs) '())
    (else 
      (cond
        ((null? (car xs)) (cons '() (fsts (cdr xs))))
        (else (cons (car (car xs)) (fsts (cdr xs))))))))

(define (snds xs)
  (cond
    ((null? xs) '())
    ((cons (snd (car xs)) (snds (cdr xs))))))

(define (insertr x y xs)
  (cond
    ((null? xs) '())
    ((equal? y (car xs)) (cons y (cons x (cdr xs))))
    (else (cons (car xs) (insertr x y (cdr xs))))))

(define (insertl x y xs)
  (cond
    ((null? xs) '())
    ((equal? y (car xs)) (cons x xs))
    (else (cons (car xs) (insertl x y (cdr xs))))))

(define (subst x y xs)
  (cond
    ((null? xs) '())
    ((equal? y (car xs)) (cons x (cdr xs)))
    (else (cons (car xs) (subst x y (cdr xs))))))

;todo (->) reimplement using (member?)
(define (subst2 x y1 y2 xs)
  (cond
    ((null? xs) '())
    ((or
      (equal? (car xs) y1)
      (equal? (car xs) y2)) (cons x (cdr xs)))
    (else (cons (car xs) (subst2 x y1 y2 (cdr xs))))))

(define (multirember x xs)
  (cond
    ((null? xs) '())
    ((equal? x (car xs)) (multirember x (cdr xs)))
    (else (cons (car xs) (multirember x (cdr xs))))))

(define (multiinsertr x y xs)
  (cond
    ((null? xs) '())
    ((equal? y (car xs)) (cons y (cons x (multiinsertr x y (cdr xs)))))
    (else (cons (car xs) (multiinsertr x y (cdr xs))))))

(define (multiinsertl x y xs)
  (cond
    ((null? xs) '())
    ((equal? y (car xs)) (cons x (cons y (multiinsertl x y (cdr xs)))))
    (else (cons (car xs) (multiinsertl x y (cdr xs))))))

(define (multisubst x y xs)
  (cond
    ((null? xs) '())
    ((equal? y (car xs)) (cons x (multisubst x y (cdr xs))))
    (else (cons (car xs) (multisubst x y (cdr xs))))))

; ---------- chapter 4 ----------

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

; ---------- chapter 5 ----------

(define (remberrec x xs)
  (cond
    ((null? xs) '())
    ((atom? (car xs))
      (cond
        ((eqan? x (car xs)) (remberrec x (cdr xs)))
        (else (cons (car xs) (remberrec x (cdr xs))))))
    (else (cons (remberrec x (car xs)) (remberrec x (cdr xs))))))

(define (insertrrec x y xs)
  (cond
    ((null? xs) '())
    ((atom? (car xs))
      (cond
        ((eqan? y (car xs)) (cons y (cons x (insertrrec x y (cdr xs)))))
        (else (cons (car xs) (insertrrec x y (cdr xs))))))
    (else (cons (insertrrec x y (car xs)) (insertrrec x y (cdr xs))))))

(define (occurrec x xs)
  (cond
    ((null? xs) 0)
    ((atom? (car xs))
      (cond
        ((eqan? x (car xs)) (add1 (occurrec x (cdr xs))))
        (else (occurrec x (cdr xs)))))
    (else (+ (occurrec x (car xs)) (occurrec x (cdr xs))))))

(define (substrec x y xs)
  (cond
    ((null? xs) '())
    ((atom? (car xs))
      (cond
        ((eqan? y (car xs)) (cons x (substrec x y (cdr xs))))
        (else (cons (car xs) (substrec x y (cdr xs))))))
    (else (cons (substrec x y (car xs)) (substrec x y (cdr xs))))))

(define (insertlrec x y xs)
  (cond
    ((null? xs) '())
    ((atom? (car xs))
      (cond
        ((eqan? y (car xs)) (cons x (cons y (insertlrec x y (cdr xs)))))
        (else (cons (car xs) (insertlrec x y (cdr xs))))))
    (else (cons (insertlrec x y (car xs)) (insertlrec x y (cdr xs))))))

(define (memberrec x xs)
  (cond
    ((null? xs) #f)
    ((atom? (car xs))
      (cond
        ((eqan? x (car xs)) #t)
        (else (memberrec x (cdr xs)))))
    (else (or (memberrec x (car xs)) (memberrec x (cdr xs))))))

(define (leftmost xs)
  (cond
    ((null? xs) '())
    (else
      (cond
        ((atom? (car xs)) (car xs))
        (else (leftmost (car xs)))))))

(define (eqlist? xs ys)
  (cond
    ((null? xs) (null? ys))
    ((atom? (car xs))
      (cond
        ((atom? (car ys)) 
          (and (eqan? (car xs) (car ys)) (eqlist? (cdr xs) (cdr ys))))
        (else #f)))
    (else (and (eqlist? (car xs) (car ys)) (eqlist? (cdr xs) (cdr ys))))))

; rewrote basic functions (like (member?), (renber))
; with this implementation if (equal?)

(define (equal? x y)
  (cond
    ((and (atom? x) (atom? y)) (eqan? x y))
    ((or (atom? x) (atom? y)) #f)
    (else (eqlist? x y))))

; ---------- chapter 6 ----------

(define ^ expt)

(define (numbered? xs)
  (cond
    ((null? xs) #t)
    ((atom? xs) (number? xs))
    ((member? (car (cdr xs)) '(+ * ^))
      (and
        (numbered? (car xs))
        (numbered? (car (cdr (cdr xs))))))
    (else #f)))

(define (value x)
  (cond
    ((atom? x) x)
    ((equal? (expr-op x) '+)
      (+ (value (expr-argl x)) (value (expr-argr x))))
    ((equal? (expr-op x) '*)
      (* (value (expr-argl x)) (value (expr-argr x))))
    ((equal? (expr-op x) '^)
      (^ (value (expr-argl x)) (value (expr-argr x))))))

(define (expr-argl xs)
  (car (cdr xs)))

(define (expr-argr xs)
  (car (cdr (cdr xs))))

(define (expr-op xs)
  (car xs))

(define (parzero? x)
  (null? x))

(define (paradd1 xs)
  (cons '() xs))

(define (parsub1 xs)
  (cdr xs))

(define (parplus xs ys)
  (cond
    ((parzero? xs) ys)
    (else (cons (car xs) (parplus (cdr xs) ys)))))

(define (parlist? xs)
  (cond
    ((parzero? xs) #t)
    ((atom? xs) #f)
    (else (and (parlist? (car xs)) (parlist? (cdr xs))))))

; ---------- CHAPTER 7 ----------

(define (set? xs)
  (cond
    ((null? xs) #t)
    ((member? (car xs) (cdr xs)) #f)
    (else (set? (cdr xs)))))

(define (makeset xs)
  (cond
    ((null? xs) '())
    ((member? (car xs) (cdr xs)) 
      (cons (car xs) (makeset (multirember (car xs) (cdr xs)))))
    (else (cons (car xs) (makeset (cdr xs))))))

(define (subset? xs ys)
  (cond
    ((null? xs) #t)
    (else (and (member? (car xs) ys) (subset? (cdr xs) ys)))))

(define (eqset? xs ys)
  (and (subset? xs ys) (subset? ys xs)))

(define (intersect? xs ys)
  (cond
    ((null? xs) #f)
    (else (or (member? (car xs) ys) (intersect? (cdr xs) ys)))))

(define (intersect xs ys)
  (cond
    ((null? xs) '())
    ((member? (car xs) ys) (cons (car xs) (intersect (cdr xs) ys)))
    (else (intersect (cdr xs) ys))))

(define (union xs ys)
  (cond
    ((null? xs) ys)
    ((member? (car xs) ys)
      (cons (car xs) (union (cdr xs) (rember (car xs) ys))))
    (else (cons (car xs) (union (cdr xs) ys)))))

(define (diffl xs ys)
  (cond
    ((null? xs) '())
    ((member? (car xs) ys) (diffl (cdr xs) ys))
    (else (cons (car xs) (diffl (cdr xs) ys)))))

; just for cinviniencz
(define (diffr xs ys)
  (diffl ys xs))

(define (intersect-all xs)
  (cond
    ((null? xs) '())
    ((null? (cdr xs)) (car xs))
    (else (intersect (car xs) (intersect-all (cdr xs))))))

(define (a-pair? x)
  (cond
    ((atom? x) #f)
    ((null? x) #f)
    ((null? (cdr x)) #f)
    ((null? (cdr (cdr x))) #t)
    (else #f)))

(define (fst p)
  (car p))

(define (snd p)
  (car (cdr p)))

(define (pair a b)
  (cons a (cons b '())))

(define (rel? xs)
  (cond
    ((null? xs) #t)
    (else (and (a-pair? (car xs)) (rel? (cdr xs))))))

(define (funl? xs)
  (and (rel? xs) (set? (fsts xs))))

(define (funr? xs)
  (funl? (rev-rel xs)))

(define (rev-pair ab)
  (pair (snd ab) (fst ab)))

(define (rev-rel xs)
  (cond
    ((null? xs) '())
    (else (cons (rev-pair (car xs)) (rev-rel (cdr xs))))))
