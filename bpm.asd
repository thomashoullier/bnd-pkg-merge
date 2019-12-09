(defsystem bpm
  :name "bpm"
  :version "0.1"
  :author "karl"
  :license "UNLICENSE"
  :description "Boundary package-merge implementation"
  :components ((:module "src"
                :components ((:file "package")
                             (:file "bnd-pkg-merge" :depends-on ("package")))))
  :in-order-to ((test-op (test-op "bpm/test"))))

(defsystem bpm/test
  :name "bpm/test"
  :version "0.1"
  :author "karl"
  :license "UNLICENSE"
  :description "Boundary package-merge test suite."
  :depends-on ("bpm" "rove")
  :components ((:module "test"
                :components ((:file "package")
                             (:file "test" :depends-on ("package")))))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
