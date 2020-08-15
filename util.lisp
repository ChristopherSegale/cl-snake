(in-package :snake)

(defmacro with-gensyms (gsyms &body body)
  `(let ,(mapcar #'(lambda (g) `(,g (gensym))) gsyms)
     ,@body))

(defun pos-or-neg (n)
  (*
   (case (random 2)
     (0 -1)
     (1 1))
   n))

(defun rand-coord (plane)
  (flet ((45-mod (n)
	     (if (eq (mod n *tile*) 0)
		 n
		 (rand-coord plane))))
	   (case plane
	     (:x (45-mod (random (- *screen-width* *tile*))))
	     (:y (45-mod (random (- *screen-height* *tile*))))
	     (t (error "Put in :x or :y")))))

(defun rand-list ()
  (list (rand-coord :x) (rand-coord :y)))

(defun alt (a b)
  (list a b a b))

(defun rep (a b)
  (list a a b b))
