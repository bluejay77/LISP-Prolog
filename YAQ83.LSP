
;;; -*- Mode: LISP -*-
;;; A strucure copying Prolog interpreter originally written in
;;; Maclisp by Mats Carlsson, UPMAIL, Uppsala University, 1983.
;;;
;;; Written for the Symbolics LISP Machine by Andy Ylikoski 06/11/85
;;; at Nokia Electronics R & D; remodified for Maclisp by AY
;;; 10/05/85. 
;;;
;;; (Partially) converted to Common LISP by Andy Ylikoski 12/13/2009
;;; while carrying out my PhD studies.
;;;
;;;
;;; Syntax:
;;; 
;;; variables are symbols beginning with ?, as
;;;             ?PLANNING-RESTRICTIONS
;;;
;;; terms and predicates are expressed as lists (<functor> . <args>),
;;; as 
;;;             (FATHER ESKO ANTTI)
;;;
;;; assertions are lists (<consequent> . <antecedents>), as
;;;             ((FATHER ?X ?Y) (PARENT ?X ?Y) (MALE ?X))
;;;
;;; top level functions:
;;; (yaq)                       top level read-prove-print loop
;;; (define-predicate <nam> . <assertions>)
;;;                             defines predicates
;;;
;;; builtin predicates:
;;; (cut)                       notorious control primitive
;;; (call ?goal)                Tries to prove ?goal.
;;; (bagof ?t ?goal ?b)         ?b is a bag of instances of
;;;                             ?t such that they satisfy ?goal.
;;; (lisp-predicate ?lispform)  ?lispform is evaluated; succeed iff
;;;                             it returns non-NIL.
;;; (lsp-cmd ?lispform)         ?lispform is evaluated for side
;;;                             effects only; return value ignored.
;;; (lisp-value ?var ?lispform)  ?lispform is evaluated and the return
;;;                             value unified with ?var.
;;; (cl ?clause)                ?clause is instantiated as an
;;;                             assertion in the knowlege base.
;;; (addcl ?clause)             Asserts ?clause into the knowledge
;;;                             base. 
;;; (addcl ?clause ?n)          Asserts ?clause after the ?n'th clause
;;;                             of the procedure it belongs to.
;;; (delcl ?clause)             Retracts clauses matching ?clause.
;;;                             Backtracks.
;;; (delcl ?predicate ?n)       Retracts the ?n'th clause of
;;;                             ?predicate. 
;;;
;;; A number of modifications to convert the program to Common LISP.
;;; Added the (halt) predicate for a better environment for developing
;;; LISP-based Prolog programs.

;;; The only sensible thing to do with this size of a program is to put it in
;;; its own package, to avoid a hyperzillion name conflicts.

(defpackage yaq (:use :cl))
(in-package yaq)

;;; Aux funtions present in MACLISP

(defun add1 (n)
  (+ n 1))

(defun sub1 (n)
  (- n 1))

;;; MACLISP MEMQ is missing in CLISP
;;; but it is present in the Clozure Common LISP
;;; and absent from SBCL

(defun memq1 (x y) (member x y :test #'equal))

;;; ASSQ is missing in the SBCL
;;; but it is there in Clozure CL

(defun assq1 (x y) (assoc x y :test #'equal))

;;; The Maclisp delq is missing in the SBCL

(defun delq1 (x y) (delete x y :test #'equal))


;;; The variable internal-time-units-per-second gives the number of
;;; "ticks" of the clock per a second of real time.
;;;
;;; The function (get-internal-run-time) (no args) returns as an
;;; integer the current run time in internal time units.  The
;;; difference between two consecutive calls is the used run time.

(defun ticks-to-msec (ticks)
  (* 1000.0 (/ (float ticks) internal-time-units-per-second)))


;;;
;;; YAQ code begins.
;;;


(defvar *cells* () "List of cells used while instantiating.")

(defvar *variables* () "List of all syntactic variables seen.")

(defvar *trail* () "The reset stack to record all variable bindings to
  know what to do upon backtrack")

(defvar *inferences* 0 "Inference counter")

(defvar *failure* 32767 "Just a large integer, used in conjunction
  with the (cut)")

;;; Variables are recognized by their first char being a '?'
(defmacro syntactic-variable-p (synt-x)
  `(and (symbolp ,synt-x) (equal '#\? (aref (symbol-name ,synt-x) 0))))

;;; (cell) creates an empty variable cell -- a cons with its CDR
;;; containing the atom DEADBEEF and the atom |var| in the CAR

(defun cell()
  (let ((x (cons '|var| 'DEADBEEF)))
    x)

;;; (defun consp (x) ...) is unnecessary -- it is there in CLTL2

(defun variable-p (x)
  (and (consp x) (equal '|var| (car x))))


;;; variables are unified by setting the CDR of the var cell point to
;;; the variable to unify to, and recording this unification to
;;; *trail*

(defun unify-variable (x y)
  (progn (push x *trail*) (rplacd x y)))


;;; assertions gets all clauses which may be used to prove a given
;;; goal

(defun assertions (goal) (get (car goal) ':assertions))


;;; each assertion in the database is represented as a pair of
;;; functions such that the CAR of the assertion data structure is a
;;; function which constructs its head, and the CDR constructs its
;;; body

(defun head (assertion) (funcall (car assertion)))
(defun body (assertion) (funcall (cdr assertion)))


;;; invoke evaluates continuations

(defun invoke (x) (apply (car x) (cdr x)))

(defmacro defbuiltin (name &rest body)
  (let ((g (intern (format nil "~A-builtin-function" name))))
    `(progn 
	    (defun ,g (goal continuation depth)
	      ,@body)
	    (setf (get ',name ':assertions) '(:builtin . ,g)))))


;;; instatiate generates a copy of a where unbound variables are
;;; handled by generator.
;;;
;;; Here it is necessary to simulate the dynamical scope
;;; of MACLISP with the lexical scope of the Common LISP.
;;;
;;; There are there three ways to do so, basically:
;;;
;;; 1) Create dynamical variables with the (LET (....) ...) form
;;;
;;; 2) Use macros, GENSYM and uninterned symbols, as in Paul Graham's books
;;;
;;; 3) Create a class and make instances of it as the recursive
;;; function is being called
;;;
;;; Hei heipparallaa Helsinki!  Broplem solved
;;;

(defvar *cells-inst* nil "This was required 'cos of the lexical scope")

(defun instantiate (x generator)
  (declare (special *cells-inst*))
  (setf *cells-inst* nil)
  (instantiate-1 x generator))

;;; Oh, golden ol' DEFUN,
;;; oh  golden oldie fun!

(defun instantiate-1 (x generator)
  (declare (special *cells-inst*))
  (cond ((syntactic-variable-p x)
	 (cond ((assq1 x *cells-inst*) (cdr (assq1 x *cells-inst*)))
	       (t (let ((c (funcall generator x)))
		    (push (cons x c) *cells-inst*) c))))
	((consp x) (cons (instantiate-1 (dereference (car x))
					generator)
			 (instantiate-1 (dereference (car x))
					generator)))
	(t x)))



;;; this generator merely makes a new value cell

(defun copycell (ignore)
  (cell))


;;; this generator creates an uninterned symbol named ?0, ?1, ?2, ...

(defun ?cell (ignore)
  (make-symbol (format nil
		       "~A~A"
		       #\? (code-char (+ (char-code #\0)
						    (length *cells*))))))


;;;                  The kernel of the interpreter follows

;;; dereference follows a chain of linked variables

(defun dereference (x)
  (cond ((variable-p x)
	 (cond ((equal (cdr x) 'DEADBEEF) x) ; Is it an empty var cell?
	       (t (dereference (cdr x))))) ; Follow chain
	(t x))) ; Otherwise, return x



;;; Unify two S-expressions
;;; This must be corrected after Peter Norvig


(defun unify (x y)
  (cond ((variable-p x)
	 (unify-variable x y)) ; Is x a variable?  Unify w/ anything
	((variable-p y)
	 (unify-variable y x)) ; Is y a variable?
	((consp x)                            ; uni-x and y are general
	 (and (consp y)                       ; expressions: recurse
	      (unify (dereference (car x)) (dereference (car y)))
	      (unify (dereference (cdr x)) (dereference (cdr y)))))
	((equal x y))))               ; Constants unify
                                              ; whenever EQUAL


;;; Pop all variables up to mark off *trail* and make them empty
;;; variable cells.  Used to undo variable bindings at backrack.

(defun reset (mark)
  (cond ((not (equal mark (car *trail*)))
	 (let ((cell (pop *trail*)))
	   (rplacd cell 'DEADBEEF)
	   (reset mark)))))


;;; ------------------------------------------------------------

;;; The inference engine.

(defun prove (goals continuation depth)
  (print goals)
  (cond ((cdr goals)                          ; Several goals?
	 (resolve (car goals)
		  `(prove ,(cdr goals) ,continuation ,depth)
					; The continuation:
					; prove the rest of the
					; goals
		      depth))
	(goals                          ; One goal?
	 (resolve (car goals)
		  continuation
		  depth))
	(t                              ; No goals left?
	 (invoke continuation))))       ; Then invoke continuation


(defun resolve (goal continuation depth)
  (progn
    (incf *inferences*)                         ; Simple way to count
    (let ((assertions (assertions goal)))
      (cond ((equal ':builtin (car assertions)) ; Is it a builtin pred?
	     (funcall (cdr assertions) goal continuation depth))
	    (t                                  ; No, go on to prove
	     (try-assertions goal assertions *trail* continuation
			     depth))))))

(defun try-assertions (goal assertions mark continuation depth)
  (cond ((cdr assertions)                     ; Several assertions?
	 (let ((msg (try-assertion
		     goal
		     (car assertions)
		     continuation
		     depth)))
	   (cond ((> msg depth)
		  (reset mark)
		  (try-assertions goal
				  (cdr assertions)
				  mark
				  continuation
				  depth))
		 ((= msg depth)
		  *failure*)
		 (t msg))))
	(assertions                           ; One assertion?
	 (try-assertion goal (car assertions) continuation depth))
	(t                                    ; No useful assertions
	 *failure*)))

(defun try-assertion (goal assertion continuation depth)
  (cond ((unify (cdr goal) (funcall (car assertion))) ;Is this
                                              ; assertion usable
                                              ; (head unifies with
					      ; goal?)
	 (prove (funcall (cdr assertion)) continuation (add1 depth)))
                                              ; Yes it was
	                                      ; Prove the body.
	(t                                    ; Wasn't useful.
	 *failure*)))

;;; ------------------------------------------------------------

;;; Here come a list of builtin primitives.
;;; The Maclisp version has builtin primitives such as
;;; |cl-delcl|, whereas the Franzl version calls them
;;; cl-delcl0 etc.  Followed this convention in the
;;; Common LISP version.  AY 12/14/2009.

(defbuiltin |cl-delcl0|
  (let* ((clause (dereference (cadr goal)))
	 (delcl-p (dereference (caddr goal)))
	 (name (caar clause)))
    (|cl-delcl-clauses0| clause
			 delcl-p
			 name
			 *trail*
			 (assertions (car clause))
			 continuation
			 depth)))


(defun |cl-delcl-clauses0| (clause delcl-p name mark assertions
			    continuation depth)
  (cond ((cdr assertions)
	 (let ((msg
		(|cl-delcl-clause0| clause
				    delcl-p
				    name
				    (car assertions)
				    continuation)))
	   (cond ((> msg depth)
		  (reset mark)
		  (|cl-delcl-clauses0| clause
				       delcl-p
				       name
				       mark
				       (cdr assertions)
				       continuation
				       depth))
		 ((= msg depth)
		  *failure*)
		 (t msg))))
	(assertions
	 (|cl-delcl-clause0| clause
			     delcl-p
			     name
			     (car assertions)
			     continuation))
	(t
	 *failure*)))


(defun |cl-delcl-clause0| (clause delcl-p name assertion continuation)
  (cond ((and (unify (cdar clause) (head assertion))
	      (unify (cdr clause) (body assertion)))

	 (cond (delcl-p              ; delcl-p determines 
                                     ; whether really to delete
		(setf (get name ':assertions)
		      (delq1 assertion (get name ':assertions)))
		(invoke continuation))
	(t
	 *failure*)))))


(defbuiltin |delcl-1-0|
  (let* ((name (dereference (cadr goal)))
	 (n (dereference (caddr goal)))
	 (assertions (get name ':assertions)))
    (setf (get name ':assertions)
	  (delq1 (nth (- n 1) assertions) assertions))
    (invoke continuation)))


(defbuiltin |addcl0|
  (let* ((clause (instantiate (dereference (cadr goal)) #'?cell))
	 (n (dereference (caddr goal)))
	 (name (caar clause))
	 (save (get name ':assertions)))
    (eval `(define-predicate ,name ,clause))
    (cond ((and (numberp n) (nthcdr n save))
	   (let ((x (nthcdr n save)))
	     (rplacd x (cons (car x) (cdr x)))
	     (rplaca x (car (get name ':assertions)))
	     (setf (get name ':assertions) save)))
	  (t (setf (get name ':assertions)
		      (nconc save (get name ':assertions)))))
    (invoke continuation)))


(defbuiltin bagof
  (let ((mark *trail*)
	(reslist (list ())))
    (destructuring-bind (?t ?p ?b) (cdr goal)
			(resolve (dereference ?p)
				 `(|bagof-aux| ,?t ,reslist) depth)
			(reset mark)
			(cond ((unify (dereference ?b)
				      (nreverse (car reslist)))
			       (invoke continuation))
			      (t *failure*)))))

(defun bagof-aux (term reslist)
  (push (instantiate
	 (dereference term)
	 'copycell)
	(car reslist))
  *failure*)

;;; ------------------------------------------------------------

;;; The famous (infamous?) control primitives.

(defbuiltin call
    (resolve (dereference (cadr goal)) continuation depth))

(defbuiltin cut (invoke continuation) (sub1 depth))

;;; ------------------------------------------------------------

;;; The procedural attachment.

;;; FUNCTION cannot be applied to the PROG1 macro.
;;; Therefore:

(defun yaq-prog1 (&rest args)
  (car args))

(defbuiltin lisp-predicate
  (cond ((eval (instantiate (cadr goal) #'yaq-prog1))
	 (invoke continuation))
	(t *failure*)))

(defbuiltin lisp-predicate-?vars
    (cond ((eval (instantiate (cadr goal) #'?cell))
	   (invoke continuation))
	  (t *failure*)))

;;; Added by AJY
;;; ------------------------------------------------------------

(defbuiltin halt
  (throw '*halt-exception* nil))

;;; ------------------------------------------------------------

;;; The DEFINE-PREDICATE macro and the definition of the top level --
;;; the (yaq) function.

;;; Here comes a long EVAL-WHEN form: define-predicate calls some
;;; functions which have to be defined at compile time

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; VARIABLES-AND-CONSTRUCTOR returns variables occurring in
;;; x and a form that will create x -- in two VALUES --
;;; Must be fixed, dynamic vs lexical scope

(defvar *variables-7* nil "Hack for dynamic vs lexical scope")

  (defun variables-and-constructor (x)
    (declare (special *variables-7*))
    (setf *variables-7* nil)
    (let* ((code (constructor x)))
      (setf *variables* (append *variables* *variables-7*))
      (values (reverse *variables-7*) code))) ; NOTE this returns 2 values!!!

;;; CONSTRUCTOR returns code that will create x
  (defun constructor (x)
    (declare (special *variables-7*))
    (cond ((syntactic-variable-p x) '(cell))
	  ((variable-p x) 
	   (if (not (memq1 x *variables-7*))
	       (push x *variables-7*))
	   `(quote ,x))
	  ((ground x) `(quote ,x))
	  (t `(cons ,(constructor (car x)) ,(constructor (cdr x))))))


;;; GROUND determines whether x is a ground instance
  (defun ground (x)
    (cond ((syntactic-variable-p x) nil)     ; variables R not ground
	  ((atom x) t)                   ; but constants R
	  ((ground (car x)) (ground (cdr x)))))
                                           ; recurse on CAR & CDR


  (defun diffq (x y)
    (mapcar #'(lambda (x1) (and (not (memq1 x1 y)) (list x1)))
	    x))
  

  (defun make-head-function-name (pred-name)
    (intern (format nil "~A-head-function-~A" pred-name (gensym))))
  
  (defun make-body-function-name (pred-name)
    (intern (format nil "~A-body-function-~A" pred-name (gensym))))

)     ; end  the (EVAL-WHEN ....) form

;;; ------------------------------------------------------------

;;; The main user interface: the DEFINE-PREDICATE

(defmacro define-predicate (name &rest assertions)
  (let ((toplist))
    `(progn
       ,@(mapcan
	  #'(lambda (a)
	      (multiple-value-bind (headvars headcode)
		  (variables-and-constructor (cdar a))
		(multiple-value-bind (bodyvars bodycode)
		    (variables-and-constructor (cdr a))
		  (let ((head-name (make-head-function-name name))
			(body-name (make-body-function-name name)))
		    (push (cons head-name body-name) toplist)
		    `((defun ,head-name ()
			,@(mapcar #'(lambda (v) ; vars to
					; empty
					; cells
				      `(setf ,v (cell))) headvars)
			,headcode)  ; code to generate head
		    (defun ,body-name ()
		      ,@(mapcar #'(lambda (v)
				    `(setf ,v (cell)))
				(set-difference bodyvars headvars))
					; all vars  except those among the head
					; vars set to (cell)
		      ,bodycode))))))  ; code to generate body
	  assertions)  ; MAPCAN this treatment over all assertions
       (setf (get ',name ':assertions) ',(nreverse toplist)))))


;;; Still to do: store pretty-printable representation under :pp-pred
;;; property of the symbol name for the (listing ?x) predicate

;;; The main read-prove-print loop, with an exit to LISP via throwing
;;; the tag *HALT-EXCEPTION*.

;;; Aux function to catch errors.
;;; Antti J Ylikoski 12-28-2010.

(defun recover (conditio)
  (format t "~%Raised condition: ~S" conditio))

(defun RPPL () ; Read-Prove-Print loop!
  (handler-case
      (progn
	(yaq)
	(print 'done))
  (condition (con) (recover con))))

(defun yaq-zero-function () 0)

;;; ------------------------------------------------------------

(defun yaq ()
  (catch '*halt-exception*
    (do ((mark *trail*)) (()) ; loop until thrown *halt-exception*
      (format t "~%| ?- ")
      (let* ((sexpr (read))) ; Input the goal to prove & print
	(multiple-value-bind (sexprvars sexprcode)
	    (variables-and-constructor sexpr)
	  (let*
	      ((cells
		(mapcar #'(lambda (x) (setf x (cell)))
			sexprvars)))
	    (unwind-protect
		 (prove
		  `(,(eval sexprcode)       ; main goal(s)
		     (lisp-predicate-?vars   ; aux predicate to output &
		      (display
		       ',sexprvars   ; initiate backtracking
		       ',cells
		       ',*inferences*
		       ',(get-internal-run-time))))
		  #'yaq-zero-function        ; this continuation will return
					     ; 0 after all subgoals are done,
					     ; thus causing a return to level
					     ; 0
		  0)                         ; depth of top level = zero
	      (reset mark))))))))


(defun display (names cells inf runtim)
  (let* ((ptime1 (get-internal-run-time))
	 (ticks (- ptime1 runtim))
	 (dt (ticks-to-msec ticks)))
    (format t "~%Theorem proving took ~A milliseconds."
	    dt)
    (format t "~%~A inferences were executed."
	    (- *inferences* inf))
    (mapc #'(lambda (n c)
	      (format t "~%~S = " n)
	      (pprint c))
	  names cells)
    (progn (format t "~%Ok ? ")
	   (y-or-n-p))))     ; if this returns NIL, the
                             ; function (display ...) will return NIL,
			     ; and then the goal lisp-predicate-?vars
			     ; will fail, thus initiating
			     ; user-controlled backtracking
			     ; (ie. analogous to giving ";" to the
			     ; Edinburgh Prolog.)



;;; utility predicates

(define-predicate lisp-command
  ((lisp-command ?X)
   (lisp-predicate (progn ?X t))))

(define-predicate lisp-value
  ((lisp-value ?X ?Y)
   (lisp-predicate (unify '?X ?Y))))

(define-predicate cl
  ((cl ?cl)
   (|cl-delcl0| ?cl nil)))

(define-predicate addcl
  ((addcl ?cl)
   (|addcl0| ?cl))
  ((addcl ?cl ?n)
   (|addcl0| ?cl ?n)))    ; index origin 1

(define-predicate delcl
  ((delcl ?cl)
   (|cl-delcl0| ?cl t))
  ((delcl ?pred ?n)
   (|delcl-1-0| ?pred ?n)))

;;; Some useful predicates


(define-predicate =
  ((= ?X ?X)))

(setf (get 'fail ':assertions) NIL)

(define-predicate not
  ((not ?p) (call ?p) (cut) (fail))
  ((not ?p)))


;;; Arithmetic

(define-predicate <
  ((< ?a ?b) (lisp-predicate (< '?a '?b))))

(define-predicate >
  ((> ?a ?b) (lisp-predicate (> '?a '?b))))

(define-predicate >=
  ((>= ?a ?b) (> ?a ?b))
  ((>= ?a ?b) (= ?a ?b)))

(define-predicate =<
  ((=< ?a ?b) (< ?a ?b))
  ((=< ?a ?b) (= ?a ?b)))

;;; The following predicates have a syntax similar to the MRS:
;;; (<operation> <arguments, here two> <result>)

(define-predicate +
  ((+ ?a ?b ?result) (lisp-value ?result (+ '?a '?b))))

(define-predicate -
  ((- ?a ?b ?result) (lisp-value ?result (- '?a '?b))))

(define-predicate *
  ((* ?a ?b ?result) (lisp-value ?result (* '?a '?b))))

(define-predicate /
  ((/ ?a ?b ?result) (lisp-value ?result (/ '?a '?b))))


