(defpackage :snake
  (:use :cl)
  (:export :main))

(in-package :snake)

(defvar *screen-width* 810)
(defvar *screen-height* 630)
(defvar *tile* 45)

(declaim
 (inline pos-or-neg
	 alt
	 rep
	 get-color
	 make-square
	 draw-square
	 square-collide-p
	 out-of-bounds-p
	 body-intersect-p
	 respawn-fruit))
