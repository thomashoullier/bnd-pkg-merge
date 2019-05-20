;;;; Implementation of length-limited Huffman coding using the algorithm from
;;;; https://doi.org/10.1007/BFb0015404.
;;;;   * O(nL) in time
;;;;   * O(L^2) in memory
;;;; With n the number of symbols in the dictionary to encode ; L the maximum
;;;; encoded length of the messages.

(defstruct chain
  ;; The probability weight of the chain
  (weight 0 :type fixnum)
  ;; The number of leaf chains before this chain in the vector it is in.
  ;; Includes the present chain if it is a leaf.
  (count 0 :type fixnum)
  ;; The chain this chain points to in a preceding list.
  (tail nil :type (or null chain)))

(defun add-chain (arr-chains j probs pair-needed)
  "A chain must be added in the vector 'j' of 'arr-chains'. Recursive."
  (let* ((cur-vec (aref arr-chains j))
	 (ilast (1- (length cur-vec)))
	 (c (chain-count (aref cur-vec ilast)))
	 (p (aref probs c)))
      (if (= j 0)
       ;; End of recursion, append a new chain to the first vector with weight
       ;; taken from 'probs'.
	  (vector-push-extend (make-chain :count (1+ c) :weight p) cur-vec)
	  (progn
	    ;; If the previous pair was previously used in a package, ask for a
	    ;; new pair.
	    (when (aref pair-needed (1- j))
	      (dotimes (it 2 t) (add-chain arr-chains (1- j) probs pair-needed))
	      (setf (aref pair-needed (1- j)) nil))
	    (let* ((prev-vec (aref arr-chains (1- j)))
		   (iplast (1- (length prev-vec)))
		   (s ( + (chain-weight (aref prev-vec (1- iplast)))
			  (chain-weight (aref prev-vec iplast)))))
	      (if (> s p)
		  ;; Append a leaf chain
		  (vector-push-extend
		   (make-chain :count (1+ c) :weight p
			       :tail (chain-tail (aref cur-vec ilast))) cur-vec)
		  ;; Append a package and signal for two nodes in vector j - 1
		  ;; to be added next time a sum s is needed.
		  (progn (vector-push-extend
			  (make-chain :count c :weight s
				      :tail (aref prev-vec iplast)) cur-vec)
			 (setf (aref pair-needed (1- j)) T))))))))

(defun encode-limited (probs L)
  "Implementation of the boundary package-merge algorithm for length-limited
huffman codes.
I: probs: array of fixnum probabilities representing the frequencies of the
          symbols to encode. Must be sorted in order of increasing values.
   L: Maximum length of an encoded character.
O: a: array counting the number of active leaf nodes in each row of the lists in
the boundary package-merge algorithm."
  (let* ((n (length probs))
	 (arr-chains (make-array L :element-type '(vector chain)
				   :initial-element #((make-chain))))
	 (a (make-array 0 :fill-pointer 0 :element-type 'fixnum))
	 (probs-padded (make-array (+ n 2) :element-type 'fixnum))
	 (pair-needed (make-array (1- L)
				  :element-type 'boolean :initial-element nil)))
    ;; Test whether constructing the length-limited code is even possible.
    ;; A prefix tree of height L contains at most 2^L leaves.
    (when (< (expt 2 L) n) (error "L=~a is too short for ~a symbols." L n))
    ;; Test whether the first, smallest weight is zero. The boundary package-
    ;; merge does not work with zero-weights.
    (when (>= 0 (aref probs 0))
      (error "Frequencies of 0 or below in 'probs' are not allowed!"))
    ;; Fill the probs-padded vector.
    (loop for p across probs
	  for i from 0 do
	    (setf (aref probs-padded i) p))
    (setf (aref probs-padded n) most-positive-fixnum)
    (setf (aref probs-padded (1+ n)) most-positive-fixnum)
    ;; Fill the initial vectors of chains with two initial chains.
    (loop for i from 0 below L do
      (setf (aref arr-chains i)
	  (make-array
	   2 :fill-pointer 2 :element-type 'chain
	     :initial-contents
	     (list (make-chain :weight (aref probs-padded 0) :count 1)
		   (make-chain :weight (aref probs-padded 1) :count 2)))))
    ;; Ask for 2n-2-2 nodes in the last list.
    (dotimes (it (- (* 2 n) 4) t)
      (add-chain arr-chains (1- L) probs-padded pair-needed))
    ;; Go up the last boundary chain and get the 'a' vector of counts.
    (let* ((lastvec (aref arr-chains (1- L)))
	   (lastchain (aref lastvec (1- (length lastvec)))))
      (loop while lastchain do
	(vector-push-extend (chain-count lastchain) a)
	(setf lastchain (chain-tail lastchain))))
    (nreverse a)))

(defun a-to-l (a)
  "Transform the 'a' vector counting the number of active leaf nodes in each row
into the list 'l' of lengths of the encoded messsages.
Note that the length of 'a' is L the maximum length of the encoded messages.
eg. in https://doi.org/10.1007/BFb0015404:
  a = [2,3,6,6] -> l = [4,4,3,2,2,2] for p = [1,1,5,7,10,14]
  This can be read as:
    * 2 nodes are counted in the four lists [length 4]
    * 3-2 = 1 node is counted in three lists and not in 4.
    * 6-3 = 3 nodes are counted in not more than 2 lists.
    * 6-6 = 0 nodes counted in only one list."
  (let* ((Lmax (length a))
	 (l (make-array (aref a (1- Lmax)) :element-type 'fixnum
					   :initial-element 0))
	 (il 0)
	 (aibuf 0))
    (loop for ai across a do
      (dotimes (it (- ai aibuf) t)
	(setf (aref l il) Lmax)
	(incf il))
      (decf Lmax)
      (setf aibuf ai))
    l))