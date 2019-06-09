;;;; Performance benchmarking of bnd-pkg-merge

(in-package :bpm)

;;; Time
(let ((probs (make-array 100000 :element-type 'fixnum))
      (a nil))
  (loop for i from 0 below (length probs) do
    (setf (aref probs i) (1+ (random 100 (sb-kernel::seed-random-state i)))))
  (setf probs (sort probs #'<=))
  (time (setf a (encode-limited probs 32))))
