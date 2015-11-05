;;this is some example to show the functions of scheme build-in support of continuations, from book <on lisp> charpter 20.
;;Author: JinQian Chen
;;Email: 2012chenjinqian@gmail.com
;;License: GPL
;(+ 1 2)
;(let ((f (lambda (x) (1+ x))))  (f 2))   ;;no distinction between symbl value and symbol function.
;(f 3);; not work, f is unseen outside.
;(define (f x)  (let ((f (lambda (x) (1+ x))))    (f 2)))  ;; much better now, (f 3) will work now.

;(define foo (lambda (x) (1+ x)))  ;;eq to
;(define (foo x) (1+ x))

;;in common lisp, function arguments are evaluated left-to-right, in scheme, unspecified.
;;scheme have #t and #f, () is true in some implemtation, and false in others.

;;continuation are first-class object, just like function. use call-with-current-continuation  call/cc for short.
;(call-with-current-continuation (lambda (cc)))(define frozen)


;figure 20.3, tree traversal using continuation. Scheme.
(define (dft tree)
    (cond ((null? tree) ())
          ((not (pair? tree)) (write tree))
          (else (dft (car tree))
                (dft (cdr tree)))))

(define *saved* ())

(define (dft-node tree)
    (cond ((null? tree) (restart))
          ((not (pair? tree)) tree)
          (else (call-with-current-continuation
                 (lambda (cc)
                   (set! *saved*
                         (cons (lambda ()
                                 (cc (dft-node (cdr tree))))
                               *saved*))
                   (dft-node (car tree)))))))

(define (restart)
    (if (null? *saved*)
        'done
        (let ((cont (car *saved*)))
          (set! *saved* (cdr *saved*))
          (cont))))

(define (dft2 tree)
    (set! *saved* ())
  (let ((node (dft-node t1)))
    (cond ((eq? node 'done) ())
          (else (write node)
                (restart)))))
;Tree traversal end here.
;;test
(define t1 '(a (b (d h)) (c e (f i) g)))
;;(define t1 ’(a (b (d h)) (c e (f i) g)))  the ' and ’ is no same.
(define t2 '(1 (2 (3 6 7) 4 5)))
(dft-node t1)
(set! *saved* ())
(let ((node1 (dft-node t1)))
  (if (eq? node1 'done)
      'done
      (list node1 (dft-node t2))))
;=> (a 1)
(restart)
;Value 15: (a 2)
(dft t1)
;=> abdhcefig
(dft2 t1)
;=> abdhcefig


;;test success.
