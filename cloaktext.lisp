; The grammar.

;; (sentence (noun-phrase verb-phrase))
;; (verb-phrase (intransitive-verb)
;; 	     (transitive-verb noun-phrase)
;; 	     (verb-phrase prepositional-phrase))
;; (noun-phrase (determiner noun)
;; 	     (noun-phrase prepositional-phrase))
;; (prepositional-phrase (preposition noun-phrase))
;; (intransitive-verb ((word sleeps)))
;; (transitive-verb ((word saw)))
;; (noun ((word man) (word woman) (word telescope)))
;; (determiner (word the))
;; (preposition ((word with) (word in)))

;; (defparameter *grammar* '((sentence (noun verb))
;; 			  (noun (man))
;; 			  (verb (eat))))

(defparameter *grammar*
  '((sentence (noun-phrase verb-phrase))
    (verb-phrase (intransitive-verb)
                 (transitive-verb noun-phrase)
                 (verb-phrase prepositional-phrase))
    (noun-phrase (determiner noun)
	         (noun-phrase prepositional-phrase))
    (prepositional-phrase (preposition noun-phrase))
    (intransitive-verb SLEEPS EATS)
    (transitive-verb SAW (WONDERED ABOUT))
    (noun MAN WOMAN CHILD DESK)
    (determiner THE A)
    (preposition WITH IN (ON TOP OF) INSIDE)))

;;; Return the right-hand side of some rule in the grammar with
;;; left-hand side NT.
(defun rhs (nt)
  (let ((rule (assoc nt *grammar*)))
    (if (null rule)
	nil ; nt is a terminal
	(nth (1+ (random (1- (length rule))))
	     rule))))

;;; Eliminates the different levels of nesting in the list LST.
(defun flatten (lst)
  (cond ((null lst) nil)
        ((atom lst) (list lst))
        (t (loop for a in lst appending (flatten a)))))

;;; Performs the next left-most derivation of the "string" LST.
(defun derive (lst)
  (cond ((null lst) ; base case
	 nil)
	((rhs (car lst)) ; there is a rule for (car lst)
	 (cons (rhs (car lst)) (cdr lst)))
	(t ; recurse
	 (cons (car lst) (derive (cdr lst))))))

;;; Performs the next left-most derivation of the "string" LST,
;;; flattening the result.
(defun flat-derive (lst)
  (flatten (derive lst)))

(defun derive-complete (lst)
  (let ((derived-list (flat-derive lst)))
    (if (equal derived-list lst)
	lst
	(derive-complete derived-list))))
