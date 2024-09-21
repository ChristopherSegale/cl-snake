(asdf:defsystem "snake"
    :author "Christopher Segale"
    :license "MIT"
    :serial t
    :depends-on (:sdl2
		 :cl-opengl)
    :components ((:file "package")
		 (:file "util")
		 (:file "objects")
		 (:file "expansion-functions")
		 (:file "snake"))
    :build-operation "program-op"
    :build-pathname "bin/cl-snake"
    :entry-point "snake:main")
