(in-package :snake)

(defmacro start-game (&rest commands)
  (apply #'game (expand-commands commands)))

(defun main ()
  (start-game (frame-rate 7)
	      (quit-key esc q)
	      (move-keys up down left right)))
