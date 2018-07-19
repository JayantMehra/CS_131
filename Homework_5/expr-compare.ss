; will crash if x is not in lst
(define (index-of lst x) 
	(index-of-helper lst x 0)
)

(define (index-of-helper lst x i)
	(if (equal? (car lst) x) i (index-of-helper (cdr lst) x (+ i 1)))
)

; returns a new symbol x!y
(define (create-binding x y)
	(string->symbol (string-append (symbol->string x) "!" (symbol->string y)))
)

; scans let-bindings detailed in x and y 
; returns a: a list of x!y's
;         b: a list of x's 
;         c: a list of y's
(define (let-helper x y a b c)
	(cond 
		; if empty, just return current bindings
		[(and (equal? x '()) (equal? y '())) (list a b c)]
		; first binding not the same
		[(not (equal? (car (car x)) (car (car y))))
		 (let-helper (cdr x) (cdr y) (cons (create-binding (car (car x)) (car (car y))) a) (cons (car (car x)) b) (cons (car (car y)) c))]
		[else (let-helper (cdr x) (cdr y) a b c)]
	)
)

; scans lambda-bindings detailed in x and y
; returns a: a list of x!y's
;         b: a list of x's 
;         c: a list of y's
(define (lambda-helper x y a b c)
	(cond
		; if empty, just return current bindings 
		[(and (equal? x '()) (equal? y '()) ) (list a b c)]
		; first binding not the same
		[(not (equal? (car x) (car y)))
		(lambda-helper (cdr x) (cdr y) (cons (create-binding (car x) (car y)) a) (cons (car x) b) (cons (car y) c))]
		[else (lambda-helper (cdr x) (cdr y) a b c)]
	)
)

; replaces all instances in expr that coincide with the joint symbol x!y
(define (repl expr expr1 x both) 
	(cond
		; recursive base case, just return empty list 
		[(equal? expr '()) '()]
		; if first element of expr is another list, recursively call on the list
		; and attach it to the rest of the expression
		[(list? (car expr)) (cons (repl (car expr) expr1 x both) (repl (cdr expr) expr1 x both))]
		; if the current symbol can be found in the list of symbols to swap out
		[(member (car expr) x) 
		; won't crash because we know (car expr) in x
		; gets the element at corresponding element
		(cons (let ((i (index-of x (car expr)))) (list-ref both i)) (repl (cdr expr) expr1 x both))]
		; not a member, just attach self to rest of recursive call
		; who needs tail recursion anyways
		[else (cons (car expr) (repl (cdr expr) expr1 x both))]
	)
)

; bind is a list of lists
; similar to repl, but replaces only at the first element of each list in bind
(define (repl-first bind x both)
	(cond 
		; recursive base case
		[(equal? bind '()) '()]
		[(member (car (car bind)) x)
		; create a list with first element swapped but all other elements the same
		; then cons it with rest of function
		 (cons (let ((i (index-of x (car (car bind)) ))) (cons (list-ref both i) (cdr (car bind)))) (repl-first (cdr bind) x both ))]
		; the first element of first binding is not in x
		[else (cons (car bind) (repl-first (cdr bind) x both))]
	)
)

(define (expr-compare x y)
	(cond
		; both computable expressions/lists
		[(equal? x y) x]
		; both boolean values
		[(and (boolean? x) (boolean? y)) 
		 (if x (if y #t '%) (if y '(not %) #f)) ]
		; I should probably combine all of these since they all do the same
		; thing anyways but w/e
		; one of them is a list, other is not
		[(not (and (list? x) (list? y))) 
		 (list 'if '% x y)]
		; both lists
		; special case lists are not equal length
		[(not (equal? (length x) (length y))) 
		 (list 'if '% x y)]
		; special case one of first is quote
		[(or (equal? (car x) 'quote) (equal? (car y) 'quote))
		 (list 'if '% x y)]
		; special case first is if, since we can't put it if into a conditional
		[(and (not (equal? (car x) 'if)) (equal? (car y) 'if))
		 (list 'if '% x y)]
		[(and (not (equal? (car y) 'if)) (equal? (car x) 'if))
		 (list 'if '% x y)]
		; if one is a let and other is not, just do the long if
		; theoreticaly could merge, but w/e
		[(and (not (equal? (car x) 'let)) (equal? (car y) 'let))
		 (list 'if '% x y)]
		[(and (not (equal? (car y) 'let)) (equal? (car x) 'let))
		 (list 'if '% x y)]
		; the first thing after the let is a list of bindings
		[(and (equal? (car x) 'let) (equal? (car y) 'let))
		; special case if num bindings are not equal, not really comparable
		; technically could reduce this case more by calling expr-compare on the body
		 (if (not (equal? (length (car (cdr x))) (length (car (cdr y))))) (list 'if '% x y)
		 (let ((bind (let-helper (car (cdr x)) (car (cdr y)) '() '() '()))) 
		 	; call expr-compare on the new modified version
		 	(cons 'let
		 	(expr-compare 
		 	; replace bindings with shared variables
		 	; replace all instances of shared variables in the computed let expressions
		 	(cons 
		 	(repl-first (car (cdr x)) (car (cdr bind)) (car bind))
		 	(repl (cdr (cdr x)) (cdr (cdr y)) (car (cdr bind)) (car bind)))
		 	(cons 
		 	(repl-first (car (cdr y)) (car (cdr (cdr bind))) (car bind))		 	
		 	(repl (cdr (cdr y)) (cdr (cdr x)) (car (cdr (cdr bind))) (car bind)))
	   ))))]
		; one is lambda, and other is not
		[(and (not (equal? (car x) 'lambda)) (equal? (car y) 'lambda))
		 (list 'if '% x y)]
		[(and (not (equal? (car y) 'lambda)) (equal? (car x) 'lambda))
		 (list 'if '% x y)]
		[(and (equal? (car x) 'lambda) (equal? (car y) 'lambda))
		; special case if num bindings are not equal
		 (if (not (equal? (length (car (cdr x))) (length (car (cdr y))))) (list 'if '% x y)
		 	(let ((bind (lambda-helper (car (cdr x)) (car (cdr y)) '() '() '()))) 
		 		(cons 'lambda
		 		(expr-compare
		 		; replace bindings with shared variables
	 			(repl (cdr x) (cdr y) (car (cdr bind)) (car bind)) 
	 			(repl (cdr y) (cdr y) (car (cdr (cdr bind))) (car bind))
 		))))]
		; recursive call if not a special case
		[else (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))]
	)
)

(define (test-expr-compare x y) 
	; literally set % #t in a let binding then compute it with expr-compare lmao
	(and (equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y)))) (equal? (eval y) (eval (list 'let '((% #f)) (expr-compare x y)))))
)

(define test-expr-x
	(list
		; all combinations for true and false
		#t
		#t
		#f
		#f
		; number literals
		12
		12
		; lists
		'(3 4 5)
		'(3 4 5)
		; string literals
		"Hello"
		"Equal"
		; variable literals
		'a
		'c
		; simple procedure calls
		'(+ 4 3)
		'(+ 4 3)
		'(+ a b)
		'(+ a b)
		; slightly more complicated calls
		'(+ 3 (- 4 3))
		'(+ 3 (- 4 3))
		; quote special form
		''(3 4)
		''(3 4)
		'(quote (3 4))
		'(quote (3 4))
		'(quote (3 4))
		; lambda special form
		'(lambda (x y) (+ x y))
		'(lambda (x y) (+ x y))
		'(lambda (x y) (+ x y))
		; let special form
		'(let ((a b)) a)
		'(let ((a b)) (+ a 5))
		; if special form
		'(if x y z)
		'(if x y z)
		'(if x y z)
		'(if x (if y a b) z)
		; other cases (from provided test cases)
		'a
		'(cons a b)
		'(cons a b)
		'(cons (cons a b) (cons b c))
		'(cons a b)
		'(list)
		'(if x y z)
		'(let ((a 1)) (f a))
		'(let ((a c)) a)
		''(let ((a c)) a)
		'(+ #f (let ((a 1) (b 2)) (f a b)))
		'((lambda (a) (f a)) 1)
		'((lambda (a b) (f a b)) 1 2)
		'((lambda (a b) (f a b)) 1 2)
	)

)

; corresponding test cases for y
(define test-expr-y
	(list
		#t
		#f
		#t
		#f
		12
		20
		'(3 4 5)
		'(4 3 5)
		"World"
		"Equal"
		'b
		'c
		'(+ 4 3)
		'(+ 3 4)
		'(+ a b)
		'(+ c d)
		'(+ 3 (- 4 3))
		'(- 5 (- a b))
		''(3 4)
		''(4 3)
		'(quote (3 4))
		'(quote (4 3))
		'asdf
		'(lambda (x y) (+ x y))
		'(lambda (y x) (+ x y))
		'(lambda (y x) (+ y x))
		'(let ((a b)) b)
		'(let ((c b)) (+ c 5))
		'(if x y z)
		'(if x z y)
		'(if z y z)
		'(if x (if y a b) c)
		'(cons a b)
		'(cons a b)
		'(cons a c)
		'(cons (cons a c) (cons a c))
		'(list a b)
		'(list a)
		'(g x y z)
		'(let ((a 2)) (g a))
		'(let ((b d)) b)
		''(let ((b d)) b)
		'(+ #t (let ((a 1) (c 2)) (f a c)))
		'((lambda (a) (g a)) 2)
		'((lambda (a b) (f b a)) 1 2)
		'((lambda (a c) (f c a)) 1 2)
	)
)
