#lang racket
(provide (all-defined-out)) ;; exports the defined variables in this file.

;; Juan Yeo (2017027265) Assignment 5 + Extra Requirements

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; Definitions for extra requirements should be here.

(struct glet (var e body) #:transparent) ;; a global binding that overrides any local binding (similar to the following ML code: let var = e in body)

(struct num-array  (size) #:transparent)  ;; a number array  (initialized to zeroes), e.g., (num-array-var 10)
                                                     ;; e.g. (num-array 4)

(struct num-array-at   (e1 e2) #:transparent) ;; e1 evaluates to num-array and e2 evaluates to racket int (index of the value to access) index starts from 0
                                              ;; (num-array-at (num-array 4) 3)
                                              ;; (num-array-at (num-array 4) 4) ;  this should give a nice error message (like "array access out of bound")
                                              ;; (num-array-at (num-array 4) -1) ;  this should give a nice error message (like "array access out of bound")

(struct num-array-set  (e1 e2 e3) #:transparent) ;; e1 evaluates to a num-array value, e2 evaluates to racket int (index of the value to access), and e3 evaluates to a MUPL int
                                              ;; (num-array-set (num-array 4) 0 (int 42))
                                              ;; (num-array-set (num-array 4) 5 (int 42)) ; this should give a nice error message (like "array access out of bound")
                                              ;; (num-array-set (num-array 4) -1 (int 42)) ; this should give a nice error message (like "array access out of bound")

(define (num-array-object? v) ;; hackish implementation for num-array object testing. We assume that if a value is mpair, it is a num-array object.
  (mpair? v))

(define (array-length array)
  (if (eq? (mcdr array) null)
      1
      (+ 1 (array-length (mcdr array)))))

(define (make-array-object length)  
    (if (= length 0)
        null
        (mcons (int 0) (make-array-object (- length 1)))))

(define (set-array-val array index val)
  (if (= index 0)
      (set-mcar! array val)
      (set-array-val (mcdr array) (- index 1) val)))

;; Extra Requirement 1 Helper Functions
(define (check-env vp env)
  (cond [(null? env) #f]
        [else (if (eq? (car vp) (car (car env)))
                  #t
                  (check-env vp (cdr env)))]))

(define (update-env vp env)
  (cond [(null? env) '()]
        [else (if (eq? (car vp) (car (car env)))
                  (cons vp (update-env vp (cdr env)))
                  (cond [(closure? (cdr (car env)))
                         (let ([clf (cdr (car env))])
                           (cons (cons (car (car env))
                                       (closure (update-env vp (closure-env clf)) (closure-fun clf)))
                                 (update-env vp (cdr env))))]
                        [else (cons (car env) (update-env vp (cdr env)))]))]))

;; Extra Requirement 2 Helper Function

(define (get-array-val array index)
  (if (= index 0)
      (mcar array)
      (get-array-val (mcdr array) (- index 1))))

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist rl)
  (cond [(null? rl) (aunit)]
        [else (apair (car rl) (racketlist->mupllist (cdr rl)))]))

(define (mupllist->racketlist ml)
  (cond [(aunit? ml) null]
        [else (cons (apair-e1 ml) (mupllist->racketlist (apair-e2 ml)))]))

; Problem 1 Test
(mupllist->racketlist (racketlist->mupllist '(1, 2, 3)))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(int? e)
         e]
        [(closure? e) e]
        [(aunit? e) e]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(ifgreater? e)
         (letrec ([v1 (eval-under-env (ifgreater-e1 e) env)]
                  [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(fun? e)
         (closure env e)]
        [(call? e)
         (letrec ([v1 (eval-under-env (call-funexp e) env)]
                  [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (eval-under-env (fun-body (closure-fun v1))
                               (append (closure-env v1)
                                       (list (cons (fun-formal (closure-fun v1)) v2)
                                             (if (fun-nameopt (closure-fun v1))
                                                 (cons (fun-nameopt (closure-fun v1)) v1)
                                                 '()))))
               (error "MUPL call applied to non-closure")))]
        [(mlet? e)
         (letrec ([v1 (mlet-var e)]
                  [v2 (eval-under-env (mlet-e e) env)])
           (if (string? v1)
               (eval-under-env (mlet-body e) (cons (cons v1 v2) env))
               (error "MUPL mlet-var applied to non-string")))]
        [(glet? e)
         (letrec ([v1 (glet-var e)]
                  [v2 (eval-under-env (glet-e e) env)])
           (if (string? v1)
               (eval-under-env (glet-body e)
                               (if (check-env (cons v1 v2) env)
                                   (update-env (cons v1 v2) env)
                                   (cons (cons v1 v2) (update-env (cons v1 v2) env))))
               (error "MUPL glet-var applied to non-string")))]
        [(num-array? e)
         (if (>= (num-array-size e) 0)
             (make-array-object (num-array-size e))
             (error "MUPL num-array applied to negative-size"))]
        [(num-array-at? e)
         (letrec ([v1 (eval-under-env (num-array-at-e1 e) env)]
                  [v2 (num-array-at-e2 e)])
           (if (num-array-object? v1)
               (if (and (>= v2 0) (< v2 (array-length v1)))
                   (get-array-val v1 v2)
                   (error "MUPL array access out of bound"))
               (error "MUPL num-array-at not a num-array-object")))]
        [(num-array-set? e)
         (letrec ([v1 (eval-under-env (num-array-set-e1 e) env)]
                  [v2 (num-array-set-e2 e)]
                  [v3 (eval-under-env (num-array-set-e3 e) env)])
           (if (num-array-object? v1)
               (if (and (>= v2 0) (< v2 (array-length v1)))
                   (begin (set-array-val v1 v2 v3) (printf "hello") v3)
                   (error "MUPL array access out of bound"))
               (error "MUPL num-array-set not a num-array-object")))]
        [(apair? e)
         (letrec ([v1 (eval-under-env (apair-e1 e) env)]
                  [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (letrec ([v1 (eval-under-env (fst-e e) env)])
           (if (apair? v1)
               (apair-e1 v1)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (letrec ([v1 (eval-under-env (snd-e e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "MUPL snd applied to non-apair")))]
        [(isaunit? e)
         (letrec ([v1 (eval-under-env (isaunit-e e) env)])
           (if (aunit? v1)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Q: mlet scope
;;(eval-exp (mlet "array-var" (num-array 4) (begin (num-array-set (var "array-var") 1 (int 42)) (var "array-var"))))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (cond
    [(null? lstlst) e2]
    [else (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))]))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun "mupl-map" "f"
       (fun "mupl-map-rec" "l"
            (ifaunit (var "l")
                     (aunit)
                     (apair (call (var "f") (fst (var "l")))
                            (call (var "mupl-map-rec") (snd (var "l"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun "addN" "n"
             (call (var "map") (fun #f "e" (add (var "e") (var "n")))))))

;; TEST Code - from GIST github
(require rackunit)

(define tests
  (test-suite
   "Racket MUPL language"
   
   ;; check racketlist to mupllist with normal list
   (check-equal? (racketlist->mupllist '()) (aunit) "racketlist->mupllist test")
   (check-equal? (racketlist->mupllist (list (int 3))) (apair (int 3) (aunit)) "racketlist->mupllist test")
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "racketlist->mupllist test")
   
   ;; check mupllist to racketlist with normal list
   (check-equal? (mupllist->racketlist (aunit)) '() "racketlist->mupllist test")
   (check-equal? (mupllist->racketlist (apair (int 3) (aunit))) (list (int 3)) "racketlist->mupllist test")
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4)) "racketlist->mupllist test")
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (apair (int 5) (aunit))))) (list (int 3) (int 4) (int 5)) "racketlist->mupllist test")

   ;; test int
   (check-equal? (eval-exp (int 5)) (int 5) "should return int 5")
   
   ;; test add
   (check-equal? (eval-exp (add (int 5) (int 10))) (int 15) "should return int 15 when adding 5 and 10")
   
   ;; tests if ifgreater returns (int 2)
   (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater test")
   (check-equal? (eval-exp (ifgreater (add (int 1) (int 2)) (int 3) (int 1) (int 5))) (int 5) "Should return 5 because is not strictly greater")
   
   ;; mlet test
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")
   (check-equal? (eval-exp (mlet "x" (int 1) (add (var "x") (var "x")))) (int 2) "mlet test")
   
   ;; call test
   (check-equal? (eval-exp (call (fun #f "x" (int 7)) (int 1))) (int 7) "Should return 7")
   (check-equal? (eval-exp (call (fun #f "x" (add (var "x") (int 7))) (int 1))) (int 8) "Should return 8")
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test")
   
   ;; Recursive call not supported yet because a problem in the env :)
   (check-equal? (eval-exp (call (fun "count" "x"
                                      (ifgreater (var "x") (int 5)
                                                 (int 2)
                                                 (call (var "count") (add (var "x") (int 1)))))
                                      (int 1))) (int 2) "Recursive call")
   
   ;; pair test
   (check-equal? (eval-exp (apair (add (int 1) (int 2)) (int 3))) (apair (int 3) (int 3)) "Should return a new pair")
   
   ;; fst test
   (check-equal? (eval-exp (fst (apair (int 1) (int 2)))) (int 1) "fst test")
   
   ;;snd test
   (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd test")
   
   ;; isaunit test
   (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")
   (check-equal? (eval-exp (isaunit (aunit))) (int 1) "isaunit test")
   
   ;; ifaunit test
   (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")
   (check-equal? (eval-exp (ifaunit (aunit) (add (int 2) (int 3)) (int 3))) (int 5) "ifaunit test")
   
   ;; mlet* test
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test")
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (int 1))) (add (var "x") (var "y")))) (int 11) "testing with two vars")
   
   ;; ifeq test
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")
   (check-equal? (eval-exp (ifeq (int 2) (int 2) (int 3) (int 4))) (int 3) "ifeq test")
   (check-equal? (eval-exp (ifeq (int 3) (int 2) (int 3) (int 4))) (int 4) "ifeq test")
   (check-equal? (eval-exp (ifeq (int 2) (int 3) (int 3) (int 4))) (int 4) "ifeq test")
   (check-equal? (eval-exp (ifeq (add (int 3) (int 1)) (add (int 2) (int 2)) (add (int 3) (int 2)) (int 4))) (int 5) "ifeq test")
   
   ;; mupl-map test
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) 
               (apair (int 8) (aunit)) "mupl-map test")

   ;; mlet chain test
   (check-equal? (eval-exp (mlet "x" (int 42) (mlet "fun_a"
                   (fun "funname" "arg1" (add (var "x") (var "arg1")))
                   (mlet "x" (int 10) (call (var "fun_a") (int 1)))))) (int 43) "mlet-chain test")

   ;; glet chain test 1
   (check-equal? (eval-exp (mlet "x" (int 42) (mlet "fun_a"
                   (fun "funname" "arg1" (add (var "x") (var "arg1")))
                   (glet "x" (int 10) (call (var "fun_a") (int 1)))))) (int 11) "glet-chain test")

   ;; glet chain test 1
   (check-equal? (eval-exp (glet "x" (int 42) (mlet "fun_a"
                   (fun "funname" "arg1" (add (var "x") (var "arg1")))
                   (glet "x" (int 10) (call (var "fun_a") (int 1)))))) (int 11) "glet-chain test")
   
   ;; problems 1, 2, and 4 combined test
   ;; (check-equal? (mupllist->racketlist
   ;; (eval-exp (call (call mupl-mapAddN (int 7))
   ;;               (racketlist->mupllist 
   ;;               (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)