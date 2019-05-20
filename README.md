# Boundary package-merge algorithm Common Lisp implementation
The boundary package-merge algorithm is an algorithm for building length-limited
Huffman codesi [1]. For an alphabet of _n_ symbols and with a constraint of
maximum length _L_ for the encoded messages, this algorithm has _O(n.L)_ time
complexity and _O(L^2)_ space complexity.

This is better than the package-merge algorithm on which it is based [2]. The
package-merge algorithm is _O(n.L)_ time and _O(n)_ space. To the best of my
knowledge, the boundary package-merge algorithm is still the best algorithm
for length-limited Huffman encoding if time and space are taken into account.

## Recommended reading
The full proof of why the boundary package-merge works can be understood by
reading just [2] and [1], preferably in this order. This is for readers
already familiar with the Huffman encoding problem, on which you will otherwise
find many resources online (sadly accompanied by the naive unefficient
implementation from the original paper!).

## Algorithm presentation


## Changes to the algorithm
The boundary package-merge algorithm, can be improved in minor ways to increase
performance. I took the benchmark case of encoding 100k symbols of random
weights to quantify the improvements (that are most of the time obvious anyway).
Some of these changes are straightforward, it is possible that they were even
implied in [1], however they are not explicit in the raw implementation 
presented in the paper.

* Computation of chain pairs in the sets preceding _j_ only if needed. The
algorithm in [1] implemented literally involves asking for nodes that will
possibly never be considered for insertion as packages. The performance impact
is almost a division by 2 in execution time for our test case. It seems that
implementing this change necessarily comes at the cost of adding an auxiliary
array of booleans of length _L - 1_ to keep track of wheter new pairs are
needed. The cost is negligeable.


## References
1. Katajainen J., Moffat A., Turpin A. (1995) A fast and space-economical algorithm for length-limited coding. In: Staples J., Eades P., Katoh N., Moffat A. (eds) Algorithms and Computations. ISAAC 1995. Lecture Notes in Computer Science, vol 1004. Springer, Berlin, Heidelberg. https://doi.org/10.1007/BFb0015404
1. Larmore, Lawrence L., and Daniel S. Hirschberg. "A fast algorithm for optimal length-limited Huffman codes." Journal of the ACM (JACM) 37.3 (1990): 464-473. https://doi.org/10.1145/79147.79150
