# Here we see Generalized Low Rank Models::
# While this snippet introduces the theory, it's no way a complete set 
# Have to see Debasattam's Videos to get solid understanding 


# The PCs constructed in PCA are linear in nature, which can cause deficiencies
# in its performance. This is much like the deficiency that linear regression
# has in capturing nonlinear relationships. Alternative approaches, known as
# matrix factorization methods have helped address this issue. More recently,
# however, a generalization of PCA and matrix factorization, called generalized
# low rank models (GLRMs) (Udell et al., 2016), has become a popular approach
# to dimension reduction.

library(dplyr)
library(ggplot2)
library(tidyr)
# for data manipulation
# for data visualization
# for data reshaping
# Modeling packages
library(h2o)
# for fitting GLRMs

url <- "https://koalaverse.github.io/homlr/data/my_basket.csv"
my_basket <- readr::read_csv(url)

typeof(my_basket)
dim(my_basket)

# GLRMs reduce the dimension of a data set by producing a condensed vector
# representation for every row and column in the original data. Specifically, given
# a data set A with m rows and n columns, a GLRM consists of a decomposition
# of A into numeric matrices X and Y. The matrix X has the same number of
# rows as A, but only a small, user-specified number of columns k. The matrix
# Y has k rows and n columns, where n is equal to the total dimension of the
# embedded features in A. For example, if A has 4 numeric columns and 1
# categorical column with 3 distinct levels (e.g., red, blue, and green), then Y
# will have 7 columns (due to one-hot encoding). When A contains only numeric
# features, the number of columns in A and Y are identical.

# Both X and Y have practical interpretations. Each row of Y is an archetypal
# feature formed from the columns of A, and each row of X corresponds to
# a row of A projected onto this smaller dimensional feature space. We can
# approximately reconstruct A from the matrix product X × Y, which has rank k.
# The number k is chosen to be much less than both m and n (e.g., for 1 million
# rows and 2,000 columns of numeric data, k could equal 15). The smaller k is,
# the more compression we gain from our low rank representation.
# To make this more concrete, lets look at an example using the mtcars data set
# (available from the built-in datasets package) where we have 32 rows and 11
# features:


head(mtcars)               # The mtcars dataset 

# mtcars represents our original matrix A. If we want to reduce matrix A to a
# rank of k = 3 then our objective is to produce two matrices X and Y that
# when we multiply them together produce a near approximation to the original
# values in A. Note that rank(A,B)<=min(rank(A),rank(B))


# We call the condensed columns and rows in matrices X and X, respectively,
# “archetypes” because they are a representation of the original features and
# observations. The archetypes in X represent each observation projected onto
# the smaller dimensional space, and the archetypes in Y represent each feature
# projected onto the smaller dimensional space. It should be columns in Y and rows in X.


# The resulting archetypes are similar in spirit to the PCs in PCA; as they are a
# reduced feature set that represents our original features. In fact, if our features
# truly behave in a linear and orthogonal manner than our archetypes produced
# by a GLRM will produce the same reduced feature set as PCA. However, if
# they are not linear, then GLRM will provide archetypes that are not necessarily
# orthogonal.
# However, a few questions remain:
#   1. How does GLRM produce the archetype values?
#   2. How do you select the appropriate value for k?

# A loss function is chosen 
# Then each of X and Y are alternately optimized according to gradient descent.


# If you’re using GLRMs to merely describe your data and gain a better
# understanding of how observations and/or features are similar then you do
# not need to use regularization. If you are creating a model that will be used
# to assign new observations and/or features to these dimensions, or you want
# to use GLRMs for imputation then you should use regularization as it can
# make your model generalize better to unseen data.

# Even when we are focusing on dimension reduction, applying regularization
# to the X matrix can still improve performance. Consequently, it is good
# practice to compare different approaches. You could use k-fold Cross Validation 
# or similar techniques to see if regularization helps. Again Bias-Variance Tradeoff
# Underfitting and Overfitting.


# As stated above, the optimal achetype values are selected based on minimizing
# some loss function. The loss function should reflect the intuitive notion of what
# it means to “fit the data well”. The most common loss function is the quadratic
# loss. The quadratic loss is very similar to the SSE criterion for
# supervised learning models where we seek to minimize the squared difference
# between the actual value in our original data (matrix A) and the predicted
# value based on our achetypal matrices (X × Y) (i.e., minimizing the squared
# residuals). Also there might be other loss functions defined on 2-Norms. See 
# Debasattam's lecture notes. 



# Selecting the value of k::
# First, if you’re using GLRMs to describe your data, then you can use many of the
# same approaches we discussed in Section 17.5 (Eigenvalue, PVE, Scree Plot), where we assess how different
# values of k minimize our loss function. If you are using GLRMs to produce a
# model that will be used to assign future observations to the reduced dimensions
# then you should use some form of Cross Validation. 






























