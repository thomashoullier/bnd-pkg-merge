;;;; Tests for Boundary package-merge algorithm implementation, the algorithm
;;;; for limited-length Huffman encoding.

(in-package :bpm)

;;; a-to-l
(defun a-to-l-valid (a l-valid)
  (let ((l (a-to-l a)))
    (assert (= (length l) (length l-valid)))
    (loop for li across l
	  for li-valid across l-valid do
	  (assert (= li li-valid)))))

(a-to-l-valid #(2 3 6 6) #(4 4 3 2 2 2))
(a-to-l-valid #(4 6 6) #(3 3 3 3 2 2))
(a-to-l-valid #(2 3 4) #(3 3 2 1))
(a-to-l-valid #(2) #(1 1))

;;; chain struct
(let* ((c (make-chain :weight 3 :count 4))
       (c2 (make-chain :tail c)))
  (setf (chain-tail c2) nil))

;;; encode-limited test
(defun encode-limited-valid (probs L a-valid)
  (let ((a (encode-limited probs L)))
    (assert (= (length a) (length a-valid)))
    (loop for ai across a
	  for aival across a-valid do
	    (assert (= ai aival)))))
;; Katajainen1996 examples
(encode-limited-valid #(1 1 5 7 10 14) 4 #(2 3 6 6))
(encode-limited-valid #(1 1 5 7 10 14) 3 #(4 6 6))
;;(encode-limited-valid #(1 1) 1 #(2))
(encode-limited-valid #(1 1 3 3) 4 #(2 3 4))

(format t "limited-test: PASS~%")

;;; Performance benchmark
;; Note that the random sequence of weights is deterministic for the sake of
;; repeatability and testing the output.
(let ((probs (make-array 100000 :element-type 'fixnum))
      (a nil)
      (expected-a #(990 3035 6091 13144 26006 51657 100000 100000 100000 100000
		    100000 100000 100000 100000 100000 100000 100000 100000
		    100000 100000 100000 100000)))
  (loop for i from 0 below (length probs) do
    (setf (aref probs i) (1+ (random 100 (sb-kernel::seed-random-state i)))))
  (setf probs (sort probs #'<=))
  (time (setf a (encode-limited probs 32)))
  (loop for ai across a
	for aval across expected-a do
	  (assert (= ai aval))))
