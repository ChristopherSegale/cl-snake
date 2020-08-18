(in-package :snake)

(defun assign-keys (&rest keys)
  (dolist (k keys)
	  (if (consp k)
	      (setf (get 'key (car k)) (cadr k))
	      (error "Arguments must be given as lists."))))

(defmacro define-keys (&rest keys)
  `(assign-keys ,@(mapcar #'(lambda (k) `',k) keys)))

(define-keys (esc :scancode-escape)
	     (q :scancode-q)
	     (up :scancode-up)
	     (down :scancode-down)
	     (left :scancode-left)
	     (right :scancode-right))

(defun game (quit-key move-keys fps)
  (with-gensyms (head body direction fruit win gl-context previous-x previous-y)
    `(let ((,head (make-square '(,(/ *screen-width* 2) ,(/ *screen-height* 2))))
	   ,body
	   (,direction 'left)
	   (,fruit (make-square (rand-list))))
       (with-game-loop (,win ,gl-context *screen-width* *screen-height*)
	 ,(quit-game quit-key)
	 ,(set-direction direction move-keys)
	 ,(game-logic win head body fruit direction previous-x previous-y fps)))))

(defmacro with-game-loop ((win gl-con width height) &body body)
  (with-gensyms (w h)
    `(let ((,w ,width) (,h ,height))
       (sdl2:with-init (:video :timer)
	 (sdl2:with-window (,win :title "Snake"  :w ,w :h ,h :flags '(:shown :opengl))
	   (sdl2:with-gl-context (,gl-con ,win)
	     (sdl2:gl-make-current ,win ,gl-con)
	     (init-gl ,w ,h)
	     (sdl2:with-event-loop (:method :poll)
	       ,@body
	       (:quit () t))))))))

(defun init-gl (screen-width screen-height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (apply #'gl:clear-color (get-color :white))
  (gl:viewport 0 0 screen-width screen-height)
  (gl:ortho 0 screen-width screen-height 0 1 -1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmacro quit-when (predicate)
  `(when ,predicate
     (sdl2:push-event :quit)))

(defun quit-game (quit-key)
  (with-gensyms (keysym)
    `(:keyup (:keysym ,keysym)
	     (quit-when (sdl2:scancode= (sdl2:scancode-value ,keysym) ,(get 'key quit-key))))))

(defun set-direction (d directions)
  (with-gensyms (keysym scancode)
    (if (<= 4 (length directions))
	`(:keydown (:keysym ,keysym)
		   (let ((,scancode (sdl2:scancode-value ,keysym)))
		     (cond
		       ,@(mapcar #'(lambda (d1 d2)
				     `((sdl2:scancode= ,scancode ,(get 'key `,d1))
				       (setf ,d ',d2)))
				 directions '(up down left right)))))
	(error "need at least four directions."))))

(defun move-snake (head direction previous-x previous-y)
  (with-gensyms (hx hy)
    `(destructuring-bind (,hx ,hy) (funcall ,head)
       (setf ,previous-x ,hx ,previous-y ,hy)
       (case ,direction
	 ,@(mapcar #'(lambda (d o v)
		       `(,d (,o ,v ,(/ *tile* 2))))
		   '(up down left right)
		   (alt 'decf 'incf)
		   (rep hy hx)))
       (funcall ,head (list ,hx ,hy)))))

(defun game-logic (win head body fruit direction previous-x previous-y fps)
  `(:idle ()
	  ,(move-snake head direction previous-x previous-y)
	  (quit-when (body-intersect-p ,head ,body))
	  ,(step-snake body previous-x previous-y)
	  (quit-when (out-of-bounds-p ,head))
	  ,(fruit-intersect-expand head body fruit previous-x previous-y)
	  (gl:clear :color-buffer)
	  (with-fps-cap (,fps)
	    ,(draw-snake head body)
	    ,(draw-fruit fruit)
	    (sdl2:gl-swap-window ,win))))

(defun body-intersect-p (sq body)
  (when body
    (dolist (b body) nil
	    (when (square-collide-p sq b)
	      (return-from body-intersect-p t)))))

(defun step-snake (body previous-x previous-y)
  (with-gensyms (px py)
    `(let ((,px ,previous-x) (,py ,previous-y))
       (push (make-square (list ,px ,py)) ,body)
       (setf ,body (butlast ,body)))))

(defun fruit-intersect-expand (head body fruit previous-x previous-y)
  (with-gensyms (h px py)
    `(let ((,h ,head) (,px ,previous-x) (,py ,previous-y))
       (when (square-collide-p ,h ,fruit)
	 (push (make-square (list ,px ,py)) ,body)
	 (setf ,fruit (respawn-fruit ,h ,body))))))

(defun respawn-fruit (head body)
  (do ((sq (make-square (rand-list)) (make-square (rand-list))))
      ((not (or (square-collide-p head sq) (body-intersect-p sq body))) sq)
   nil))

(defun draw-snake (head body)
  (with-gensyms (h rest b)
    `(let ((,h ,head)
	   (,rest ,body))
       (draw-square ,h :brown)
       (when (listp ,rest)
	 (dolist (,b ,rest)
	     (draw-square ,b :green))))))

(defun draw-fruit (fruit)
  `(draw-square ,fruit :red))

(defmacro with-fps-cap ((frame-rate) &body body)
  (with-gensyms (st ellapsed-time)
    `(let ((,st (sdl2:get-ticks)) ,ellapsed-time)
       ,@body
       (setf ,ellapsed-time (- (sdl2:get-ticks) ,st))
       (when (< ,ellapsed-time ,(/ 1000.0 frame-rate))
	 (sdl2:delay (floor (- ,(/ 1000.0 frame-rate) ,ellapsed-time)))))))

(defun expand-commands (commands)
  (let ((args (make-hash-table)))
    (labels ((rec (commands)
	       (let ((c (car commands)))
		 (case (car c)
		   (frame-rate
		    (if (integerp (cadr c))
			(progn
			  (setf (gethash 'fps args ) (cadr c))
			  (rec (cdr commands)))
			(error "Framerate must be an integer.")))
		   (quit-key
		    (setf (gethash 'quit args) (cadr c))
		    (rec (cdr commands)))
		   (move-keys
		    (setf (gethash 'move args) (cdr c))
		    (rec (cdr commands)))))))
      (rec commands)
      (mapcar #'(lambda (k) (gethash k args)) '(quit move fps)))))
