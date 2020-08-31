(load "bundle/bundle.lisp")
(load "snake.asd")
(asdf:load-system :snake)
(save-lisp-and-die "snake" :toplevel #'snake:main :executable t :application-type :gui)
