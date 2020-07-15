(require errortrace) ;; 
(require racket/trace) ;; used to trace executions errors

;FIBONACCI SERIES
(define (fibo n)
    (cond
        ((<= n 0) 0)
        ((<= n 2) 1)
        (else (+ (fibo (- n 1)) (fibo (- n 2))))
    )
)

(trace fibo)
(display (fibo 0))
(newline)

;EXPONENTIAL
(define (expo n power)
    (if (<= power 1)
        n
        (* n (expo n (- power 1)))
    )
)

(trace expo)
(display (expo 2 6))
(newline)

;MIN
(define (minimo numbers)
    (cond
        ((null? (cdr numbers)) (car numbers))
        ((< (car numbers) (minimo (cdr numbers))) (car numbers))
        (else (minimo (cdr numbers)))
    )
)
(trace minimo)
(display (minimo '(2 4 8 1 5 6)))
(newline)

;INSERT
(define (inserta n numbers)
    (cond
        ((null? numbers) (list n))
        ((<= n (car numbers)) (cons n numbers))
        (else (cons (car numbers) (inserta n (cdr numbers))))
    )
)
(trace inserta)
(display (inserta 4 '(1 2 3 5 6 7)))
(newline)

;CONCATENATE
(define (concatena l1 l2)
    (cond
        ((null? l1) l2)
        (else (cons (car l1) (concatena (cdr l1) l2)))
    )
)

(trace concatena)
(display (concatena '(a b c) '(a (b c) d e)))
(newline)

;INVERT
(define (invierte l1)
    (cond
        ((null? l1) l1)
        (else (concatena (reverse (cdr l1)) (list (car l1))))
    )
)
(trace invierte)
(display (invierte '(a (b c) d (e f))))
(newline)

;DELETE
(define (elimina element list1)
    (cond
        ((null? list1) list1)
        ((and (null? (cdr list1)) (equal? element (car list1)))
              (elimina element (cdr list1)))
        ((equal? element (car list1)) cons (car (cdr list1)) 
              (elimina element (cdr list1)))
        (else (cons (car list1) (elimina element (cdr list1))))
    )
)
(trace elimina)
(display (elimina '(a b) '(a b (a b) c d (c d))))
(newline)

;DELETE REPEATED
(define (repetidos list1)
    (cond 
         ((null? list1) '())
         ((member (car list1) (cdr list1)) (repetidos (cdr list1)))
         (else (cons (car list1) (repetidos (cdr list1))))
    )
)
(trace repetidos)
(display (repetidos '(a b c d a f g d c c h)))