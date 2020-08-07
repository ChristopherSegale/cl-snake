(asdf:defsystem "snake"
    :author "Christopher Segale"
    :license "MIT"
    :depends-on (:sdl2
		 :cl-opengl)
    :components ((:file "package")
		 (:file "util")
		 (:file "objects")
		 (:file "expansion-functions")
		 (:file "snake"))
    :build-operation "program-op"
    :build-pathname "snake"
    :entry-point "snake:main")
