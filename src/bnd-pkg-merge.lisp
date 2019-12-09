;;;; Implementation of length-limited Huffman coding using the algorithm from
;;;; https://doi.org/10.1007/BFb0015404.
;;;;   * O(nL) in time
;;;;   * O(L^2) in memory
;;;; With n the number of symbols in the dictionary to encode ; L the maximum
;;;; encoded length of the messages.

(in-package :bpm)

(defstruct chain
  ;; The number of leaf chains before this chain in the vector it is in.
  ;; Includes the present chain if it is a leaf.
  (count 0 :type fixnum)
  ;; The chain this chain points to in a preceding list.
  (tail nil :type (or null chain)))

(defun mkclosures (probs-i L)
  "Defines an environment around the necessary functions created as `labels`."
  (let* ((n (length probs-i))
	 ;; Padded array of probabilities
	 (probs (make-array (+ (length probs-i) 2) :element-type 'fixnum))
	 ;; Each line is a set of chains.
	 (arr-chains (make-array L :element-type '(vector chain)
				   :initial-element #((make-chain))))
	 ;; Last set in arr-chains.
	 (last-vec (aref arr-chains (1- L)))
	 ;; Whether a new pair is needed in line j.
	 (pair-needed (make-array
		       L :element-type 'boolean :initial-element nil))
	 ;; Weight of last package in each line.
	 (last-pack-weight 
	   (make-array L :element-type 'fixnum
			 :initial-element (+ (aref probs-i 0)
					     (aref probs-i 1))))
	 ;; Array storing the last count for each row j. 
	 (last-count (make-array L :element-type 'fixnum
				   :initial-element 2))
	 ;; Array of last tail element for each line. Only for building,
	 ;; cannot be traversed.
	 (last-tail (make-array L :element-type '(or null chain)
				  :initial-element nil)))
    (labels
	((add-chain (j)
	   ;; If the previous pair was previously used in a package, ask for a
	   ;; new pair.
	   (when (and (> j 0) (aref pair-needed (1- j)))
	     (dotimes (it 2 t) (add-chain (1- j))))
	   (let* ((cur-vec (aref arr-chains j))
		  (ilast (1- (length cur-vec)))
		  (c (aref last-count j))
		  (p (aref probs c)))
	     (when (= j 0)
	       ;; End of recursion, append a new chain to the first vector with
	       ;; weight taken from 'probs'.
	       (incf (aref last-count j))
	       (if (aref pair-needed j)
		   (progn (setf (aref pair-needed j) nil)
			  (setf (aref last-pack-weight j) 0))
		   (vector-push-extend (make-chain :count (aref last-count j))
				       cur-vec))
	       (incf (aref last-pack-weight j) p)
	       (return-from add-chain))
	     (let* ((prev-vec (aref arr-chains (1- j)))
		    (iplast (1- (length prev-vec)))
		    (s (aref last-pack-weight (1- j)))
		    (nweight s)
		    (ntail (aref prev-vec iplast))
		    (ncount c))
	       (if (> s p)
		   ;; Append a leaf chain
		   (psetf nweight p
			  ntail (chain-tail (aref cur-vec ilast))
			  ncount (incf (aref last-count j)))
		   ;; Append a package and signal for two nodes in vector j - 1
		   ;; Signal a pair to be added next time a sum s is needed.
		   (progn (setf (aref last-tail j) ntail)
			  (setf (aref pair-needed (1- j)) T)))
	       (if (aref pair-needed j)
		   (progn (setf (aref last-pack-weight j) 0)
			  (setf (aref pair-needed j) nil))
		   (vector-push-extend
		    (make-chain :count ncount :tail (aref last-tail j))
		    cur-vec))
	       (incf (aref last-pack-weight j) nweight))))
	 (chains-gc ()
	   (let* ((bound-chain (aref last-vec (1- (length last-vec))))
		  (curvec nil)
		  (curchain nil)
		  (inewchain 0))
	     (loop for ivec from (1- L) downto 0 do
	       (when (not bound-chain) (return-from chains-gc))
	       (setf curvec (aref arr-chains ivec))
	       (setf inewchain 0)
	       (loop for ichain from (1- (length curvec)) downto 0
		     until (eq curchain bound-chain) do
		       (setf curchain (aref curvec ichain))
		       (setf (aref curvec inewchain) curchain)
		       (incf inewchain))
	       (setf (fill-pointer curvec) inewchain)
	       (setf bound-chain (chain-tail bound-chain))
	       (setf curvec (nreverse curvec)))))
	 (encode-limited ()
	   (let ((a (make-array 0 :fill-pointer 0 :element-type 'fixnum))
		 ;; Current last tail of last vector. Triggers garbage
		 ;; collection if changed.
		 (cur-last-tail nil)
		 (lastchain nil))
	     ;; Test whether constructing the length-limited code is even
	     ;; possible.
	     ;; A prefix tree of height L contains at most 2^L leaves.
	     (when (< (expt 2 L) n)
	       (error "L=~a is too short for ~a symbols." L n))
	     ;; Test whether the first, smallest weight is zero. The boundary
	     ;; package-merge does not work with zero-weights.
	     (when (>= 0 (aref probs-i 0))
	       (error "Frequencies of 0 or below in 'probs' are not allowed!"))
	     ;; Fill the probs-padded vector.
	     (loop for p across probs-i
		   for i from 0 do
		     (setf (aref probs i) p))
	     (setf (aref probs n) most-positive-fixnum)
	     (setf (aref probs (1+ n)) most-positive-fixnum)
	     ;; Fill the initial vectors of chains with two initial chains.
	     (loop for i from 0 below L do
	       (setf (aref arr-chains i)
		     (make-array
		      2 :fill-pointer 2 :element-type 'chain
		      :initial-contents
		      (list (make-chain :count 1)
			    (make-chain :count 2)))))
	     ;; Bind the last-vec
	     (setf last-vec (aref arr-chains (1- L)))
	     ;; Ask for 2n-2-2 nodes in the last list.
	     (dotimes (it (- (* 2 n) 4) t)
	       (when (= 0 (mod it 2)) (setf (aref pair-needed (1- L)) T))
	       (add-chain (1- L))
	       ;; Call manual garbage collection every time the tail of the last
	       ;; vector changes (a package was created).
	       (setf lastchain (aref last-vec (1- (length last-vec))))
	       (when (not (eq cur-last-tail
			      (setf cur-last-tail (chain-tail lastchain))))
		 (chains-gc)))
	     ;; Go up the last boundary chain and get the 'a' vector of counts.
	     (setf lastchain (aref last-vec (1- (length last-vec))))
	     (loop while lastchain do
	       (vector-push-extend (chain-count lastchain) a)
	       (setf lastchain (chain-tail lastchain)))
	     (nreverse a))))
      (values #'encode-limited))))

(defun encode-limited (probs-i L)
  (funcall (mkclosures probs-i L)))

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
