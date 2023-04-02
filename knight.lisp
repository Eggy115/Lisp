(in-package :knight.lisp)

(defun main ()
  (print "Hello, world!")
  (terpri))

;; (condp foo
;;   (pred1 term1)
;;   (pred2 term2)
;;   (pred3 term3)
;;   (predn termn))
;; (defmacro condp (val &rest clauses)
;;   (let* ((val-sym (gensym "condp_val_"))
;;          (conditions (mapcar #'(lambda (clause)
;;                                 `((,(car clause) ,val-sym)
;;                                   ,(cadr clause)))
;;                             clauses)))
;;   `(let (,val-sym ,val)
;;      (cond ,@conditions))))

(defun kn-booleanp (val)
  (or (eq nil val) (eq t val)))
(defun kn-listp (val)
  (and (listp val) (eq (car val) :list)))
(defun kn-blockp (val)
  (and (listp val) (eq (car val) :block)))
(defun kn-nullp (val)
  (eq :null val))

(defun coerce-number (val)
  (cond
    ((stringp val) (or (parse-integer val :junk-allowed t) 0))
    ((numberp val) val)
    ((kn-booleanp val) (if val 1 0))
    ((kn-nullp val) 0)
    ((kn-listp val) (length (cdr val)))
    (t (error "Could not coerce term to number: ~a" val))))
(defun coerce-string (val)
  (cond
    ((stringp val) val)
    ((numberp val) (format nil "~d" val))
    ((kn-booleanp val) (if val "true" "false"))
    ((kn-nullp val) "")
    ((kn-listp val) (str:join #\Newline (mapcar #'coerce-string (cdr val))))
    (t (error "Could not coerce term to string: ~a" val))))
(defun coerce-boolean (val)
  (cond
    ((stringp val) (not (str:emptyp val)))
    ((numberp val) (not (equal 0 val)))
    ((kn-booleanp val) val)
    ((kn-nullp val) 0)
    ((kn-listp val) (not (eq (cdr val) '())))
    (t (error "Could not coerce term to boolean: ~a" val))))
;; N.B. Lists are represented as (cons :list data) to differentiate
;; them from other forms represented as lisp lists
(defun coerce-list (val)
  (cond
   ((stringp val) (cons :list (coerce val 'list)))
   ((numberp val) (cons :list (map 'list (lambda (c)
					   (or (digit-char-p c) '-))
				   (prin1-to-string val))))
   ((kn-booleanp val) (cons :list (if val '(t) '())))
   ((kn-nullp val) '(:list))
   ((kn-listp val) val)
   (t (error "Could not coerce term to list: ~a" val))))

(defun valuep (val)
  (or (stringp val)
      (numberp val)
      (kn-booleanp val)
      (kn-nullp val)
      (kn-listp val)
      (kn-blockp val)))

(defun kn-eval (term ctx)
  (if (valuep term)
      term
      (kn-eval (kn-eval-step term ctx) ctx)))

;; TODO: maybe find some way to make this tail recursive? could probably just
;; flip around terms, since we've already parsed it to a form where RPN won't
;; break everything...(right?)

(defun kn-eval-step (term ctx)
  (when (valuep term)
    (return-from kn-eval-step term))
  ;; singletons of terms are also valid terms. this is really the most
  ;; neat way to solve this, without implementing a bunch of annoying
  ;; cdr nonsense (or using cons cell "tuples" which becomes annoying
  ;; very quickly)
  ;; TODO: maybe use cons cell "tuples" in this way?
  (when (and (listp term) (equal (length term) 1) (valuep (car term)))
    (return-from kn-eval-step (car term)))
  (when (not (listp term))
    (error "malformed term, must be either a value or a list: ~a" term))

  (case (car term)
    ;; Nullary
    (:prompt (read-line nil nil ""))
    (:random (random 32768))

    ;; Unary
    (:noop (cdr term))
    ;; N.B. BLOCK is covered by `valuep'
    ;; todo: call
    (:quit (sb-ext:exit :code (coerce-number (kn-eval (cdr term) ctx))))
    (:not (not (coerce-boolean (kn-eval (cdr term) ctx))))
    (:length (length (coerce-list (kn-eval (cdr term) ctx))))
    ;; TODO
    (:dump (let ((val (kn-eval (cdr term) ctx)))
             (cond
               ((stringp val) (format t "String(~s)" val))
               ((numberp val) (format t "Number(~d)" val))
               ((equal t val) (format t "Boolean(true)"))
               ((equal nil val) (format t "Boolean(false)"))
               ((equal :null val) (format t "Null()"))
               ((and (listp val) (equal :block (car val))) (format t "Block(<opaque>)")))
             val))
    (:negate (- (coerce-number (kn-eval (cdr term) ctx))))
    (:ascii (let ((val (kn-eval (cdr term) ctx)))
	      (cond
		;; this seems to be the most concise way to do this
		((numberp val) (string (code-char val)))
		((stringp val) (char-code (aref val 0)))
		(t (error "Undefined behaviour: applied invalid argument to ASCII: ~a" val)))))
    (:box (let ((val (kn-eval (cdr term) ctx)))
	    (list :list val)))
    (:head (let ((val (kn-eval (cdr term) ctx)))
	     (cond
	       ((stringp val) (subseq val 0 1))
	       ((kn-listp val) (cadr val))
	       (t (error "Undefined behaviour: applied invalid argument to [: ~a" val)))))
    (:tail (let ((val (kn-eval (cdr term) ctx)))
	     (cond
	       ((stringp val) (subseq val 1))
	       ((kn-listp val) (cons :list (cddr val)))
	       (t (error "Undefined behaviour: applied invalid argument to ]: ~a" val)))))
	     
    ;; Binary


    ))
