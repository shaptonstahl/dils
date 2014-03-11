The function I used (hclust in R) performs a hierarchical cluster analysis using a set of dissimilarities for the n objects being clustered. Initially, each object is assigned to its own cluster and then the algorithm proceeds iteratively, at each stage joining the two most similar clusters, continuing until there is just a single cluster.

Row i of 'merge.tsv' describes the merging of two clusters at step i of the clustering. If an element j in the row is negative, then official -j was merged at this stage. If j is positive then the merge was with the cluster formed at the (earlier) stage j of the algorithm. Thus negative entries in merge indicate agglomerations of single officials, and positive entries indicate agglomerations of groups of officials.

For example, suppose merge.tsv begins

   -1       -135
   -3793    1
   -2       -2195
   -3       -152
   -320     4
   -394     5
   ...      ...

In this example we start with 4183 clusters, each with a single official, numbered -1 to -4183.  The first row above says to merge official 1 and official 135.  The second row says to merge official 3793 with the cluster created in row 1, so now the only non-singleton cluster is {1, 135, 3793}. Row 3 says to merge officials 2 and 2195, creating the cluster {2, 2195}.  Row 4 says to merge officials 3 and 152 to get {3, 152}.  

Row 5 says to put official 320 in the cluster created in row 4, giving us the cluster {3, 152, 320}. Row 6 says to add official 394 to the cluster created in row 5.  So far, our list of clusters is

  {1, 135, 3793}, {2, 2195}, {3, 152, 320, 394}, plus all the rest as singletons

====

In hierarchical cluster displays, a decision is needed at each merge to specify which subtree should go on the left and which on the right. Since, for n observations there are n-1 merges, there are 2^{(n-1)} possible orderings for the leaves in a cluster tree, or dendrogram. The algorithm used in hclust is to order the subtree so that the tighter cluster is on the left (the last, i.e., most recent, merge of the left subtree is at a lower value than the last merge of the right subtree). Single observations are the tightest clusters possible, and merges involving two observations place them in order by their observation sequence number.

The object 'order.tsv' is a vector giving the permutation of the original officials suitable for plotting, in the sense that a cluster plot using this ordering and matrix 'merge.tsv' will not have crossings of the branches.

For example, suppose order.tsv begins

   906
   4167
   375
   718
   3913
   3137
   ...

This means that official 906 (the 906th one in the original data set) whould be displayed first, then original observation 4167, etc.
