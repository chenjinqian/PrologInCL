;;This is an embeded prolog interpreter implementation, with commom-lisp. This is a practise work after reading the book <on lisp> by Pau Graham.
;;Author: JinQian Chen
;;Email: 2012chenjinqian@gmail.com

;;It should work like this:
;;> (fact painter reynolds)
;;(REYNOLDS)
;;>(fact painter gainsborough)
;;(GAINSBOROUGH)
;;>(with-Answer (painter ?x)  (paint ?x))
;;GAINSBOUGH
;;REYNOLDS
;;NIL

;;Define if-mache, simple version.
(defmacro with-inference (query &body body)
  `(progn
     (setq *paths* nil)
     (=bind (binds) (prove-query ',(rep_ query) nil)
            (let ,(mapcar #'(lambda (v)
                              `(,v (fullbind `,v binds)))
                          (vars-in query #'atom))
              ,@body
              (fail)))))

(defun rep_ (x)
  (if (atom x)
      (if (eq x '_) (gensym "?") x)
      (cons (rep_ (car x)) (rep_ (cdr x)))))
;;what does the _ after rep means?

(defun fullbind (x b)
  (cond ((varsym? x) (aif2 (binding x b)
                           (fullbind it b)
                           (gensym)))
        ((atom x) x)
        (t (cons (fullbind (car x) b)
                 (fullbind (cdr x) b)))))

(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

;;test =defun
(=defun add1 (x)
  (=values   (1+ x)))
(add1 3)   ;;test passed
(=defun bar (x)
  (=values (list  'a (add1 x))))
(bar 8)    ;;test passed
;;the continuation-passing macro to build continuation in common lisp, as scheme supported build-in.
(setq *cont* #'identity)
(defmacro =lambda (parms &body body)
  `#'(lambda (*cont* ,@parms) ,@body))
(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string
                                "=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
         `(,',f *cont* ,,@parms))
       (defun ,f (*cont* ,@parms) ,@body))))
;;(intern (concatenate 'string "=" (symbol-name a)))  (setq a 'aa)
(defmacro =bind (parms expr @body body)
  `(let ((*cont* #'(lambda ,parms ,@body))) ,expr))
(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals))
(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))
(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))

;;interpretation for query
(=defun prove-query (expr binds)
        (case (car expr)
          (and (prove-and (cdr expr) binds))
          (or (prove-or (cdr expr) binds))
          (not (prove-not (cadr expr) binds))
          (t (prove-simple expr binds))))
;; error, prove-query not defind. test =defun, not sure we can use it directly here or not.;;solved, after define =defun.

(=defun prove-and (clauses binds)
        (if (null clauses)
            (=values binds)
            (=bind (binds) (prove-query (car clauses ) binds)
                   (prove-and (cdr clauses ) binds))))

(=defun prove-or (clauses binds)
  (choose-bind c clauses
               (prove-query c binds)))

(=defun prove-not (expr binds)
  (let ((save-paths *paths*))
    (setq *paths* nil)
    (choose (=bind (b) (prove-query expr binds)
                   (setq *paths* save-paths)
                   (fail))
            (progn
              (setq *paths* save-paths)
              (=values binds)))))

(=defun prove-simple (query binds)
  (choose-bind r *rlist*
               (implies r query binds)))

;;code involving rules
(defvar *rlist* nil)
(defmacro <- (con &rest ant)
  (let ((ant (if (= (length ant) 1)
                 (car ant)
                 `(and ,@ant))))
    `(length (conc1f *rlist* (rep_ (cons `,ant `,con))))))

(=defun implies (r query binds)
  (let ((r2 (change-vars r)))
    (aif2 (match query (cdr r2) binds)
          (prove-query (car r2) it)
          (fail))))

(defun change-vars (r)
  (sublis (mapcar #'(lambda (v)
                      (cons v (symb '? (gensym))))
                  (vars-in r #'atom))
          r))
