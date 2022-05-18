#lang racket
(provide (all-defined-out))

;2017027265 Juan Yeo

(define (check_bst ns)
  (cond [(null? ns) #t]
        [(and (null? (car (cdr ns))) (null? (car (cdr (cdr ns))))) #t] ;(n () ())
        [(null? (car (cdr ns))) ;(n () (ns'))
         (if (< (car ns) (car (car (cdr (cdr ns)))))
             (check_bst (car (cdr (cdr ns))))
             #f)]
        [(null? (car (cdr (cdr ns)))) ;(n (ns') ())
         (if (> (car ns) (car (car (cdr ns))))
             (check_bst (car (cdr ns)))
             #f)]
        [(> (car ns) (car (car (cdr ns)))) ;(n (ns') (ns''))
         (if (< (car ns) (car (car (cdr (cdr ns)))))
             (and (check_bst (car (cdr ns))) (check_bst (car (cdr (cdr ns)))))
             #f)]
        [#t #f]))

;(check_bst '(6 (4 () ()) (7 () ())))
;(check_bst '(6 (7 ()()) (8 ()())))
;(check_bst '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))

(define (apply f ns)
  (cond [(null? ns) '()] 
        [(and (null? (car (cdr ns))) (null? (car (cdr (cdr ns))))) ;(n () ())
         (list (f (car ns)) '() '())]
        [(null? (car (cdr ns))) ;(n () (ns'))
         (list (f (car ns)) '() (apply f (car (cdr (cdr ns)))))]
        [(null? (car (cdr (cdr ns)))) ;(n (ns') ())
         (list (f (car ns)) (apply f (car (cdr ns))) '())]
        [#t (list (f (car ns)) (apply f (car (cdr ns))) (apply f (car (cdr (cdr ns)))))]))

;(apply (lambda (v) (+ v 1)) '(7 (6 ()()) (8 ()())))

(define (append_list xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append_list (cdr xs) ys))))

(define (extract_all_elements ns)
  (cond [(null? ns) '()] 
        [(and (null? (car (cdr ns))) (null? (car (cdr (cdr ns))))) ;(n () ())
         (list (car ns))]
        [(null? (car (cdr ns))) ;(n () (ns'))
         (cons (car ns) (extract_all_elements (car (cdr (cdr ns)))))]
        [(null? (car (cdr (cdr ns)))) ;(n (ns') ())
         (cons (car ns) (extract_all_elements (car (cdr ns))))]
        [#t (cons (car ns) (append_list (extract_all_elements (car (cdr ns))) (extract_all_elements (car (cdr (cdr ns))))))]))

(define (equals xs ys)
  (let ([xe (extract_all_elements xs)]
        [ye (extract_all_elements ys)])
    (equal? (sort xe <) (sort ye <))))

;(equals '(7 (6 ()()) (8 ()())) '(6 () (7 () (8 ()()))))
;(equals '(7 (6 ()()) (8 ()())) '(7 (6 ()()) (8 ()(9 () ()))))