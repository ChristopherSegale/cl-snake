(in-package :snake)

(defun get-color (color)
  (case color
    (:brown
     (list 0.647059 0.164706 0.164706))
    (:green
     (list 0.0 1.0 0.0))
    (:red
     (list 1.0 0.0 0.0))
    (:white
     (list 1 1 1 1))
    (t (error "Put in :brown :red :green or :white as argument."))))

(defun make-square (position)
  (let ((pos position))
    (lambda (&optional npos)
      (if (null npos)
	  pos
	  (setf pos npos)))))

(defun draw-square (s color)
    (let* ((pos (funcall s))
	   (left (car pos))
	   (right (+ left (/ *tile* 2.0)))
	   (top (cadr pos))
	   (bottom (+ top (/ *tile* 2.0))))
      (gl:begin :quads)
      (apply #'gl:color (get-color color))
      (gl:vertex left top)
      (gl:vertex right top)
      (gl:vertex right bottom)
      (gl:vertex left bottom)
      (gl:end)))

(defun square-collide-p (s1 s2)
  (destructuring-bind (x1 y1) (funcall s1)
    (destructuring-bind (x2 y2) (funcall s2)
      (and (eq x1 x2) (eq y1 y2)))))
			 
(defun out-of-bounds-p (s)
  (destructuring-bind (x y) (funcall s)
    (or
     (> y (- *screen-height* (/ *tile* 2.0)))
     (< y 0)
     (> x (- *screen-width* (/ *tile* 2.0)))
     (< x 0))))
