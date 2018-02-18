Categorical Distance Metrics for Clustering

In common data science applications, many (if not all) variables may be categorical.  However, the default index used is typically Gower's
dissimilarity index, which assigns 1's to matches and 0's to mismatches.  Although Gower's index may be acceptable when most variables are 
continuous, it fails to include all the information provided when variables are all or almost all categorical. Hence, it may be beneficial
to use other dissimilarity metrics better suited to take in all the information provided by categorical variables.  For example, Goodall's index
assigns higher weighting to variables that match on an infrequent attribute, such as the presence of a disease.  

This R package implements categorical dissimilarity indices from the paper "Similarity Measures for Categorical Data: A Comparative Evaluation," by Boriah,
Chandela, and Kumar (2008).  Although not all indices are implemented, the following are: Goodall1, Goodall2, Goodall3, Goodall4, Occurence Frequency,
Inverse Occurence Frequency, and the Eskin Index.  Currently, the plan is to implement most of the metrics from the paper.  

Indices are called from the wrapper function catDist() by specifying the method parameter.  In addition, continuous variables may be passed into the function to 
create an analogue of gower's distance from the daisy() function from the cluster package.  Continuous variables are defaulted to manhattan distance, and
a weighted average of variables is taken from the continuous and categorical distances (after normalization).  

The package is intended to be used with clustering applications in mind; in particular, with the pam() algorithm by Kaufman and Rousseeuw (1990).  In R, 
the pam() algorithm accepts dissimilarity matrices, and this package is intended to leverage that functionality.

Joshua Agterberg (2/18/18)
Johns Hopkins AMS Master's Student