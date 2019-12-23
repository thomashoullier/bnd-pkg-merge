;;;; Tests for Boundary package-merge algorithm implementation, the algorithm
;;;; for limited-length Huffman encoding.

(in-package :bpm/test)

;;; Helpers
(defun a-to-l-valid (a l-valid)
  (let ((l (a-to-l a))
        (valid-p T))
    (setf valid-p (= (length l) (length l-valid)))
    (loop for li across l
          for li-valid across l-valid do
            (setf valid-p (and valid-p (= li li-valid))))
    valid-p))

(defun encode-limited-valid (probs L a-valid)
  (let ((a (encode-limited probs L))
        (valid-p T))
    (setf valid-p (= (length a) (length a-valid)))
    (loop for ai across a
          for aival across a-valid do
            (setf valid-p (and valid-p (= ai aival))))
    T))

(defun large-set-valid ()
  "Example of large set.
Note that the random sequence of weights is deterministic for the sake of
repeatability and testing the output."
  (let ((probs (make-array 100000 :element-type 'fixnum))
        (a nil)
        (expected-a
          #(990 3035 6091 13144 26006 51657 100000 100000 100000 100000
            100000 100000 100000 100000 100000 100000 100000 100000
            100000 100000 100000 100000))
        (valid-p T))
    (loop for i from 0 below (length probs) do
      (setf (aref probs i)
            (1+ (random 100 (sb-kernel::seed-random-state i)))))
    (setf probs (sort probs #'<=))
    (setf a (encode-limited probs 32))
    (loop for ai across a
          for aval across expected-a do
            (setf valid-p (and valid-p (= ai aval))))
    valid-p))

;;; Tests
(deftest a-to-l
  (ok (a-to-l-valid #(2 3 6 6) #(4 4 3 2 2 2)))
  (ok (a-to-l-valid #(4 6 6) #(3 3 3 3 2 2)))
  (ok (a-to-l-valid #(2 3 4) #(3 3 2 1)))
  (ok (a-to-l-valid #(2) #(1 1))))

(deftest encode-limited
  (ok (encode-limited-valid #(1 1 5 7 10 14) 4 #(2 3 6 6)))
  (ok (encode-limited-valid #(1 1 5 7 10 14) 3 #(4 6 6)))
  (ok (encode-limited-valid #(1 1) 1 #(2)))
  (ok (encode-limited-valid #(1 1 3 3) 4 #(2 3 4)))
  (testing "Errors"
    (ok (signals (encode-limited #(1 1 3 3) 1))
        "L too short error.")
    (ok (signals (encode-limited #(1 3 1 3) 5))
        "probs-i not sorted in increasing order.")
    (ok (signals (encode-limited #(0 1 3 3) 5))
        "probs-i contains a 0.")))

;; Bugs
;; (format t "~A" (encode-limited #(1 2 3) 5))

(deftest large-set
  (ok (large-set-valid)))
