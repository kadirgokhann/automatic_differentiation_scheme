; Kadir Gökhan Sezer
; 2018400369
; compiling: yes
; complete: yes

#lang racket

;; given
(struct num (value grad)
    #:property prop:custom-write
    (lambda (num port write?)
        (fprintf port (if write? "(num ~s ~s)" "(num ~a ~a)")
            (num-value num) (num-grad num))))



;;#:transparent
(define (rev l)
    (if (null? (cdr l))
        l
        (append (rev (cdr l)) (list (car l)))))

;;  3.1   ----------------------------------------------------------------------------------------------------------------------------------------------------
(define (get-value x) (
          
        cond[(not(list? x))  (num-value x)  ]
            [ (null? x) '()  ]
            [ (list? x)   (cons (num-value (eval (car x))) (get-value (cdr x )))]
                    
 )
)
;;  3.2   ----------------------------------------------------------------------------------------------------------------------------------------------------
(define (get-grad x) (
          
        cond[(not(list? x))  (num-grad x)  ]
            [ (null? x) '()  ]
            [ (list? x)   (cons (num-grad (eval (car x))) (get-grad (cdr x )))]
                    
 )
)
;;  4.1   ----------------------------------------------------------------------------------------------------------------------------------------------------
(define add (lambda args(
                    cond [(= (length args) 2)     (let (  [val  (apply + (get-value args))] [grad  (apply + (get-grad args))])  (    num val grad))]
                         [else    (add (car args) (apply add (cdr args)))]

                         ) ) )
;;  4.2   ----------------------------------------------------------------------------------------------------------------------------------------------------
(define mul (lambda args(
                    cond [(= (length args) 2)     (let (  [val  (apply * (get-value args))] [grad  (apply + (map * (get-value args) (rev (get-grad args))))   ])  (    num val grad))]
                         [else    (mul (car args) (apply mul (cdr args)))]

                         ) ) )
;;  4.3   ----------------------------------------------------------------------------------------------------------------------------------------------------
(define sub (lambda args(
                    cond [(= (length args) 2)     (let (  [val  (apply - (get-value args))] [grad  (apply - (get-grad args))])  (    num val grad))]
                         [else    (sub (car args) (apply sub (cdr args)))]

                         ) ) )
;;  4.4   ----------------------------------------------------------------------------------------------------------------------------------------------------
(define relu (lambda (x) (if (> (num-value x) 0) x (num 0.0 0.0))))
(define mse (lambda (x y) (mul (sub x y) (sub x y))))
;;  5.1   ----------------------------------------------------------------------------------------------------------------------------------------------------

(define combine-hashes
	(lambda (hashlist)
		(apply hash
			(foldr (lambda (args curr)
						(append curr (list (caar (hash->list args))
									       (cdar (hash->list args))
									 )
						)
				   ) 
			 '()
			 hashlist
			)
		)
    )
)



(define concat(lambda (keys values) (map (lambda (k v ) `(,k,v)) keys values)))
(define convert (lambda (x) (eval x)))
(define create-hash(lambda (l1 l2 b)(  cond[(= 4 (length l1)) (combine-hashes(  map hash l1 (list (num (first l2) (if(equal? b (first l1)) 1.0 0.0)) (num (second l2) (if(equal? b (second l1)) 1.0 0.0)) (num (third l2) (if(equal? b (third l1)) 1.0 0.0)) (num (fourth l2) (if(equal? b (fourth l1)) 1.0 0.0)))))]
                                           [(= 3 (length l1)) (combine-hashes(  map hash l1 (list (num (first l2) (if(equal? b (first l1)) 1.0 0.0)) (num (second l2) (if(equal? b (second l1)) 1.0 0.0)) (num (third l2) (if(equal? b (third l1)) 1.0 0.0)))))]
                                           [(= 2 (length l1)) (combine-hashes(  map hash l1 (list (num (first l2) (if(equal? b (first l1)) 1.0 0.0)) (num (second l2) (if(equal? b (second l1)) 1.0 0.0)))))]                                          
                                           [(= 1 (length l1)) (combine-hashes(  map hash l1 (list (num (first l2) (if(equal? b (first l1)) 1.0 0.0)))))]                                            


                                         )))




;;(define create-hash(lambda (l1 l2 b)(   combine-hashes(  map hash l1 (list (num (first l2) (if(equal? b (first l1)) 1.0 0.0)) (num (second l2) (if(equal? b (second l1)) 1.0 0.0)) (num (third l2) (if(equal? b (third l1)) 1.0 0.0)) (num (fourth l2) (if(equal? b (fourth l1)) 1.0 0.0)))))))


;;  5.2   ----------------------------------------------------------------------------------------------------------------------------------------------------
(define parse(lambda (hash expr) (
                            cond[(null? expr)  '()]
                                [(list? expr)   (cons (parse hash (car expr)) (parse hash (cdr expr)) )]
                                [(equal? expr '+)  'add ]
                                [(equal? expr '*)   'mul]
                                [(equal? expr '-)   'sub]
                                [(equal? expr 'mse)  'mse ]
                                [(equal? expr 'relu)  'relu ]
                                [(number? expr) (num expr 0.0)]
                                [else (hash-ref hash expr)]


                                  )))




;;> (hash-ref (create-hash '(a b c d) '(1 3 3 7) 'c) 'a)
;;(num 1 0.0)

;;  5.3   ----------------------------------------------------------------------------------------------------------------------------------------------------
(define grad(lambda (names values var expr) (
                num-grad  (eval  (parse (create-hash names values var) expr))
             )))

;;  5.4   ----------------------------------------------------------------------------------------------------------------------------------------------------

(define partial-grad(lambda (names values vars expr) (
                                                  cond[(= (length names) 1) (list (grad names values (first vars) expr)) ]
                                                      [(= (length names) 2) (list (if(member (first names) vars)    (grad names values (first names) expr)  0.0 ) (if(member (second names) vars)    (grad names values (second names) expr)  0.0 ))]
                                                      [(= (length names) 3) (list (if(member (first names) vars)    (grad names values (first names) expr)  0.0 ) (if(member (second names) vars)    (grad names values (second names) expr)  0.0 ) (if(member (third names) vars)    (grad names values (third names) expr)  0.0 ) )]
                                                      [(= (length names) 4) (list (if(member (first names) vars)    (grad names values (first names) expr)  0.0 ) (if(member (second names) vars)    (grad names values (second names) expr)  0.0 ) (if(member (third names) vars)    (grad names values (third names) expr)  0.0 ) (if(member (fourth names) vars)    (grad names values (fourth names) expr)  0.0 ) )]
                                                      [(= (length names) 5) (list (if(member (first names) vars)    (grad names values (first names) expr)  0.0 ) (if(member (second names) vars)    (grad names values (second names) expr)  0.0 ) (if(member (third names) vars)    (grad names values (third names) expr)  0.0 ) (if(member (fourth names) vars)    (grad names values (fourth names) expr)  0.0 ) (if(member (fifth names) vars)    (grad names values (fifth names) expr)  0.0 ) )]                                                      
                                                      [(= (length names) 6) (list (if(member (first names) vars)    (grad names values (first names) expr)  0.0 ) (if(member (second names) vars)    (grad names values (second names) expr)  0.0 ) (if(member (third names) vars)    (grad names values (third names) expr)  0.0 ) (if(member (fourth names) vars)    (grad names values (fourth names) expr)  0.0 ) (if(member (fifth names) vars)    (grad names values (fifth names) expr)  0.0 )  (if(member (sixth names) vars)    (grad names values (sixth names) expr)  0.0 ) )] 
                                                      [(= (length names) 7) (list (if(member (first names) vars)    (grad names values (first names) expr)  0.0 ) (if(member (second names) vars)    (grad names values (second names) expr)  0.0 ) (if(member (third names) vars)    (grad names values (third names) expr)  0.0 ) (if(member (fourth names) vars)    (grad names values (fourth names) expr)  0.0 ) (if(member (fifth names) vars)    (grad names values (fifth names) expr)  0.0 )  (if(member (sixth names) vars)    (grad names values (sixth names) expr)  0.0 )  (if(member (seventh names) vars)    (grad names values (seventh names) expr)  0.0 )  )] 
                                                      [(= (length names) 8) (list (if(member (first names) vars)    (grad names values (first names) expr)  0.0 ) (if(member (second names) vars)    (grad names values (second names) expr)  0.0 ) (if(member (third names) vars)    (grad names values (third names) expr)  0.0 ) (if(member (fourth names) vars)    (grad names values (fourth names) expr)  0.0 ) (if(member (fifth names) vars)    (grad names values (fifth names) expr)  0.0 )  (if(member (sixth names) vars)    (grad names values (sixth names) expr)  0.0 )  (if(member (seventh names) vars)    (grad names values (seventh names) expr)  0.0 )   (if(member (eighth names) vars)    (grad names values (eighth names) expr)  0.0 ) )] 

                                                      )))

;;  5.5   ----------------------------------------------------------------------------------------------------------------------------------------------------

(define partial-grad1(lambda (names values vars expr) (  grad names values (first vars) expr) ))


;;(define gradient-descent1(lambda (names values vars lr expr) (
  ;;                                                           cond[(null? names) '()]
    ;;                                                          [(= (length names) 1)(   if(member (first names) vars)
      ;;                                                                                      (eval(- (num-value (hash-ref (create-hash names values vars) (first names)))(* lr (partial-grad1 names values (list (first names)) expr))))
        ;;                                                                                    (+ 0.0 (first values))                                                                   )]

          ;;                                                       [else ( list  (gradient-descent1 (list (car names))  values  vars lr expr) (gradient-descent1 (cdr names) (cdr values) (cdr vars) lr expr)  )]
            ;;                                                 )))




;(define gradient-descent(lambda (names values vars lr expr) (
 ;;cond[(= (length vars) 1)  (if(member (first names) vars) (list (eval(- (first values)(* lr (first (partial-grad names values (list (first names)) expr))))) (+ 0.0 (second values)))   (list (+ 0.0 (first values))   (eval(- (second values)(* lr (second (partial-grad names values (list (second names)) expr)))))    )  )  ]
   ;;  [(= (length vars) 2)  (if(member (first names) vars)
     ;;                          (list (eval(- (first values)(* lr (first (partial-grad names values (list (first names)) expr))))) (+ 0.0 (second values)))
       ;;                        (if(member (second names) vars)
         ;;                          (list (+ 0.0 (first values))   (eval(- (second values)(* lr (second (partial-grad names values (list (second names)) expr))))))
           ;;                          ))]

     

    ;;[else  (list  (first (gradient-descent names values (list (first vars)) lr expr)) (second (gradient-descent names values (cdr vars) lr expr)))]

      ;;                                                      )))




(define gradient-descent(lambda (names values vars lr expr ) (
 cond[(= (length names) 1)    (list (if(member (first names) vars) ( eval(- (first values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (first values))      )  )     ]
     [(= (length names) 2)    ( list (if(member (first names) vars)  (eval(- (first values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (first values)))  (if(member (second names) vars)  (eval(- (second values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (second values))) )  ]
     [(= (length names) 3)    ( list (if(member (first names) vars)  (eval(- (first values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (first values)))  (if(member (second names) vars)  (eval(- (second values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (second values))) (if(member (third names) vars)  (eval(- (third values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (third values))))  ]
     [(= (length names) 4)    ( list (if(member (first names) vars)  (eval(- (first values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (first values)))  (if(member (second names) vars)  (eval(- (second values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (second values))) (if(member (third names) vars)  (eval(- (third values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (third values))) (if(member (fourth names) vars)  (eval(- (fourth values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (fourth values))) )  ]
     [(= (length names) 5)    ( list (if(member (first names) vars)  (eval(- (first values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (first values)))  (if(member (second names) vars)  (eval(- (second values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (second values))) (if(member (third names) vars)  (eval(- (third values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (third values))) (if(member (fourth names) vars)  (eval(- (fourth values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (fourth values))) (if(member (fifth names) vars)  (eval(- (fifth values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (fifth values)))    )  ]
     [(= (length names) 6)    ( list (if(member (first names) vars)  (eval(- (first values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (first values)))  (if(member (second names) vars)  (eval(- (second values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (second values))) (if(member (third names) vars)  (eval(- (third values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (third values))) (if(member (fourth names) vars)  (eval(- (fourth values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (fourth values))) (if(member (fifth names) vars)  (eval(- (fifth values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (fifth values)))  (if(member (sixth names) vars)  (eval(- (sixth values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (sixth values)))      )  ]
     [(= (length names) 7)    ( list (if(member (first names) vars)  (eval(- (first values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (first values)))  (if(member (second names) vars)  (eval(- (second values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (second values))) (if(member (third names) vars)  (eval(- (third values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (third values))) (if(member (fourth names) vars)  (eval(- (fourth values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (fourth values))) (if(member (fifth names) vars)  (eval(- (fifth values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (fifth values)))  (if(member (sixth names) vars)  (eval(- (sixth values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (sixth values)))    (if(member (seventh names) vars)  (eval(- (seventh values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (seventh values)))      )  ]
     [(= (length names) 8)    ( list (if(member (first names) vars)  (eval(- (first values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (first values)))  (if(member (second names) vars)  (eval(- (second values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (second values))) (if(member (third names) vars)  (eval(- (third values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (third values))) (if(member (fourth names) vars)  (eval(- (fourth values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (fourth values))) (if(member (fifth names) vars)  (eval(- (fifth values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (fifth values)))  (if(member (sixth names) vars)  (eval(- (sixth values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (sixth values)))    (if(member (seventh names) vars)  (eval(- (seventh values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (seventh values)))   (if(member (eighth names) vars)  (eval(- (eighth values)(* lr  (partial-grad1 names values names expr))))  (+ 0.0 (eighth values)))     )  ]

     )))


;;  5.6   ----------------------------------------------------------------------------------------------------------------------------------------------------
;;(define optimize1(lambda (names values vars lr k expr) (
 ;;                                                      )(
  ;;                                                                if(= i (- k 1))
   ;;                                                                 (and (display "'")(display (gradient-descent names values vars lr expr)))
    ;;                                                    (set! values (gradient-descent names values vars lr expr))
    ;;                                                    ))) )

(define optimize(lambda (names values vars lr k expr) (
                                                       cond[(<= 2 k)  (and (set! values (gradient-descent names values vars lr expr)) (optimize names values vars lr (- k 1) expr)) ]
                                                           [(= k 1)  (gradient-descent names values vars lr expr) ]
                                                       ))) 
