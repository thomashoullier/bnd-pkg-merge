(defsystem bpm
  :name "bpm"
  :version "0.1"
  :author "karl"
  :license "UNLICENSE"
  :description "Boundary package-merge implementation"
  :components ((:file "package")
	       (:file "bnd-pkg-merge" :depends-on ("package"))))
