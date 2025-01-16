;; Name: Hailey Lee
;; G#: G01254390
;; Homework 5: Lisp


#|
	prime-factors
	prime-factors-help
	is-prime
|#
(defun prime-factors (n)
	"return a list of n's prime factors"
	(if (= n 1)
  		NIL
  		(prime-factors-help 2 n)
	)
)

(defun prime-factors-help (start n)
	"helper function for prime-factors" 
 	(cond ((= n 1)                                                                                             NIL)
    	  ((and (= (mod n start) 0) (is-prime 2 start))   (append (list start) (prime-factors-help 2 (/ n start))))    
        (T                                                                    (prime-factors-help (+ start 1) n))        
  	) 
)

(defun is-prime (x y)
	"check if x is prime"
	(cond ((= x y)                              T)
        ((= (mod y x) 0)                    NIL)
        (T                 (is-prime (+ x 1) y))
	)
)


#|
	coprime
	coprime-help
|#
(defun coprime (a b)
	"checks if a and b are co-prime"
  	(if (or (= a 1) (= b 1))    
      	NIL
      	(coprime-help a b (min a b))
  	)
)

(defun coprime-help (a b c)
	"helper function for coprime"
	(cond ((= c 1)                                                          T)
        ((and (= (mod a c) 0) (= (mod b c) 0))                          NIL)
        (T                                       (coprime-help a b (- c 1)))
 	)
)


#|
	trib
	trib-help
|#
(defun trib (n) 
	"calculate the nth tribonnaci number"
  (if (or (= n 0) (= n 1) (= n 2))
     	1
     	(trib-help 1 1 1 n)
  )
)

(defun trib-help (a b c n)
	"helper function for trib"
 	(if (<= (- n 3) 0)
    	(+ a b c)
     	(trib-help b c (+ a b c) (- n 1))
  	)
)


#|
	max-two
	max-two-help
|#
(defun max-two (xs)
	"finds the two largest values from the list xs"
	(cond ((null xs)                                                             NIL)
		    ((= (length xs) 1)                                                      xs)
		    (T                   (max-two-help (car xs) (car (cdr xs)) (cdr (cdr xs))))
	)	
)

(defun max-two-help (f s xs)
	"helper function for max-two"
	(cond ((null xs)                               (append (list (max f s)) (list (min f s))))
		    ((and (<= (car xs) f) (<= (car xs) s))                  (max-two-help f s (cdr xs)))
		    (T                                       (max-two-help (max f s) (car xs) (cdr xs)))
	)
)


#|
	reversed
|#
(defun reversed (xs)
	"reverse the list xs"
  (if (null xs) 
  		NIL
  		(append (reversed (cdr xs)) (list (car xs)))
 	)
)


#|
	clockwise
	clockwise-help-comb
	clockwise-help-ext
|#
(defun clockwise (grid)
	"rotate the grid clockwise"
	(if (null grid) NIL
  		(let (
  	          (row         (length grid))
  	          (col   (length (car grid)))
  	   		 )
       		(clockwise-help-comb row 0 col grid)
  		)
	)
)

(defun clockwise-help-comb (r c cs grid)
	"helper function for clockwise: combine each row"
	(if (= c cs)
		  NIL
		  (append (list (clockwise-help-ext 0 r c grid)) (clockwise-help-comb r (+ c 1) cs grid))
	)
)

(defun clockwise-help-ext (r rs c grid)
	"helper function for clockwise: extract c-th elements from each row"
	(if (= r rs) 
		  NIL
		  (append (clockwise-help-ext (+ r 1) rs c grid) (list(nth c (nth r grid))))
	)
)


#|
	any
|#
(defun any (bs)
	"return T if any of bs is true, otherwise NIL"
  (cond ((null bs)                       NIL)
  	    ((equal (car bs) T)                T)
  	    (T                    (any (cdr bs)))
  )
)


#|
	select
|#
(defun select (p xs)
	"return a list of items from xs that pass the predicate function p"
  (cond ((null xs)                                              NIL)
        ((funcall p (car xs))   (cons (car xs) (select p (cdr xs))))
        (T                                      (select p (cdr xs)))
  )
)


#|
	zip-with
|#
(defun zip-with (f as bs)
	"zip up the two lists, as and bs, with the function f"
	(cond ((null as)                                                                   NIL)
		    ((null bs)                                                                   NIL)
		    (T           (cons (funcall f (car as) (car bs)) (zip-with f (cdr as) (cdr bs)))) 
	)
)


#|
	augdentity
	augdentity-help-comb
	augdentity-help-fill
|#
(defun augdentity (r c)
	"returns 2D list that represents r x c augemented identiy matrix"
	(if (or (= r 0) (= c 0))
    	NIL
    	(augdentity-help-comb 0 r c)
	)
)

(defun augdentity-help-comb (r rs c)
	"helper function for augdentity: combine each row"
	(if (= r rs)
      NIL
      (append (list (augdentity-help-fill r 0 c)) (augdentity-help-comb (+ r 1) rs c))
  	)
)

(defun augdentity-help-fill (r c cs)
	"helper function for augdentity: fill columns"
	(cond ((= c cs)                                                     NIL)
		    ((= r c)    (append (list 1) (augdentity-help-fill r (+ c 1) cs)))
		    (T          (append (list 0) (augdentity-help-fill r (+ c 1) cs)))
	)
)




