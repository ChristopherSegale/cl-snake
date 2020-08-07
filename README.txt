cl-snake

This is an implementation of snake that I wrote in Common Lisp using the sdl2 and cl-opengl packages.

Controls and Rules
Use the arrow keys to move the snake around.  Eating a fruit will increase the size of the snake.  Having the head hit the body of the snake or go out of bounds will end the game.

Compilation instructions
Use the makefile in order to create a standalone executable.  Changing the 'LISP' variable can allow you to change which implementation, SBCL by default, is used.  Be sure to change the other variables to match the flags that your chosen implementation uses.  Alternatively, you can load the 'snake.asd' file in order to load up the whole program from your lisp implementation of choice.
