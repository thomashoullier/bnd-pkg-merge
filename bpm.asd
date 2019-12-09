(defsystem bpm
  :name "bpm"
  :version "0.1"
  :author "karl"
  :license "UNLICENSE"
  :description "Boundary package-merge implementation"
  :components ((:file "src/package")
               (:file "src/bnd-pkg-merge" :depends-on ("src/package"))))
