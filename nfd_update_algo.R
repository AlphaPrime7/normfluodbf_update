#Figured out the algo for the dat data cleanup
#1. extremely janky data and will need some old code but some new stuff to really get the data cleaned
#up and accurate.
#2. The Algo summary:
# turns out for every 3 tuples in a column, it needs to be converted into 1 row, to represent 3 samples
A1 B1 C1

so as such

v1
1
2
3
4
5
6

becomes (depending on your experimental setup)

A1 -- B1 -- C1
1     2     3
4     5     6
