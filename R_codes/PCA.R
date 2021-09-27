library(dplyr)      # basic data manipulation and plotting
library(ggplot2)    # data visualization
library(h2o)             # performing dimension reduction 

# Refer to the book for more theory and equations 
# All the packages are installed here 

url <- "https://koalaverse.github.io/homlr/data/my_basket.csv"        # The my_basket dataset
my_basket <- readr::read_csv(url)
typeof(my_basket)           # List 2000x42 
dim(my_basket)         # 2000 samples and 42 features 

# Now we have seen the dimension of the datset 
# It is 2000 (samples) x 42 (features)

my_basket             # A 2000x42 tibble 

names(my_basket)          # Displaying column names in R

colnames(my_basket)           # Column names in R

# So we have seen what columns the dataset contains (the items that you put in the basket):: features 
# About the tidy format::
#Tidy data is a specific way of organizing data into a consistent format which 
#plugs into the tidyverse set of packages for R.
 

# To do dimensionality reduction in R:
#1. Data are in tidy format per Wickham et al. (2014);
#2. Any missing values in the data must be removed or imputed;
#3. Typically, the data must all be numeric values (e.g., one-hot, label,
                                                   #ordinal encoding categorical features);
#4. Numeric data should be standardized (e.g., centered on mean and scaled with variance) to
#make features comparable.

# The tidy data format::

#Each variable in the data set is placed in its own column
#Each observation is placed in its own row
#Each value is placed in its own cell
  
#Data that satisfies these rules is known as tidy data. 

# The data at hand (my_basket) satisfies all the above properties.
# So we will reveiew tidying the data later.

#One option includes examining pairwise scatterplots of each variable against
#every other variable and identifying co-variation.

# Viewing these scatter plots pairwise we can keep those features which we require 

# PCA in brief::

#PCA examines the
#covariance among features and combines multiple features into a smaller set of
#uncorrelated variables. These new features, which are weighted combinations of
#the original predictor set, are called principal components (PCs) and hopefully
#a small subset of them explain most of the variability of the full feature set.
#The weights used to form the PCs reveal the relative contributions of the
#original features to the new PCs. The more data varies, the more information it contains!!!

#It can be shown, using techniques from linear algebra 1 , that the eigenvector
#corresponding to the largest eigenvalue of the feature covariance matrix is the
#set of loadings that explains the greatest proportion of feature variability.

# Generally these steps need to be followed before doing dimensionality reduction:: 
#standardizing numeric features, imputing
#missing values, and encoding categorical features.


# Start the h2o instance::
update.packages("h2o")
update.packages("data.table")
h2o.no_progress()  
# turn off progress bars for brevity
h2o.init(max_mem_size = "5g")
# connect to H2O instance

#First, we convert our my_basket data frame to an appropriate h2o object and
#then use h2o.prcomp() to perform PCA. There are some arguments to the h2o.prcomp() function::

# pca_method :
#   Character string specifying which PC method to use. there
# are actually a few different approaches to calculating principal components
# (PCs). When your data contains mostly numeric data (such as my_basket ),
# its best to use pca_method = ”GramSVD” . When your data contain many cate-
#   gorical variables (or just a few categorical variables with high cardinality)
# we recommend you use pca_method = ”GLRM” .
# k : Integer specifying how many PCs to compute. It’s best to create the same
# number of PCs as there are features and we will see shortly how to identify
# the number of PCs to use, where the number of PCs is less than the number
# of features.
# transform : Character string specifying how (if at all) your data should be
# standardized.
# impute_missing : Logical specifying whether or not to impute missing values;
# if your data have missing values, this will impute them with the corresponding
# column mean.
# max_runtime_secs : Number specifying the max run time (in seconds); when
# working with large data sets this will limit the runtime for model training.

my_basket.h2o <- as.h2o(my_basket)    # Convert with an appropriate h2o object 
# run PCA
my_pca <- h2o.prcomp(
  training_frame = my_basket.h2o,
  pca_method = "GramSVD",
  k = ncol(my_basket.h2o),            # Same as the number of features in the original dataset
  transform = "STANDARDIZE",
  impute_missing = TRUE,
  max_runtime_secs = 1000
)


# Our model object ( my_pca ) contains several pieces of information that we
# can extract (you can view all information with glimpse(my_pca) ). The most
# important information is stored in my_pca@model$importance (which is the
#                                                             same output that gets printed when looking at our object’s printed output).
# This information includes each PC, the standard deviation of each PC, as well
# as the proportion and cumulative proportion of variance explained with each
# PC.

glimpse(my_pca)

my_pca@model$importance          # Gives information about the PCs 

# Naturally, the first PC (PC1) captures the most variance followed by PC2, then
# PC3, etc. We can identify which of our original features contribute to the PCs by
# assessing the loadings. The loadings for the first PC represent φ 11 , φ 21 , ... , φ u�1
# in Equation (17.1). Thus, these loadings represent each features influence
# on the associated PC. If we plot the loadings for PC1 we see that the largest
# contributing features are mostly adult beverages (and apparently eating candy
# bars, smoking, and playing the lottery are also associated with drinking!).  


my_pca@model$eigenvectors


my_pca@model$eigenvectors %>%
  as.data.frame() %>%
  mutate(feature = row.names(.)) %>%
  ggplot(aes(pc1, reorder(feature, pc1))) +
  geom_point()
# Some variables compute negative to the PC, some contribute positive to the PC
# Some contribute 0 to the PC

my_pca@model$eigenvectors %>%
  as.data.frame() %>%
  mutate(feature = row.names(.)) %>%
  ggplot(aes(pc1, pc2, label = feature)) +
  geom_text()

# We can also compare PCs against one another. For example, Figure 17.4 shows
# how the different features contribute to PC1 and PC2. We can see distinct
# groupings of features and how they contribute to both PCs. For example, adult
# beverages (e.g., whiskey and wine) have a positive contribution to PC1 but
# have a smaller and negative contribution to PC2. This means that transactions
# that include purchases of adult beverages tend to have larger than average
# values for PC1 but smaller than average for PC2.

  

# So here we have seen the contribution of each feature to the PCs

# How many PCs to keep? There are criterias for this as in::
# There are three common approaches in helping to make this decision:
# 1. Eigenvalue criterion
# 2. Proportion of variance explained criterion
# 3. Scree plot criterion

# First we look at the eigenvalue criterion 
# The Eigenvalues of the covariance matrix give us an idea about the variance of each of the 
# PCs. That is the variance of v'X, where X is the data matrix.
# Note that we should standardize the data centered on it's mean and then compute the covariance matrix.

# Rationale behind the eigenvalue criterion::
# The rationale for using the eigenvalue
# criterion is that each component should explain at least one variable’s worth
# of the variability, and therefore, the eigenvalue criterion states that only
# components with eigenvalues greater than 1 should be retained.

eigen <- my_pca@model$importance["Standard deviation", ]      # Standard deviation of each of the PCs
eigen

eigen <- my_pca@model$importance["Standard deviation", ] %>%       # Convert the standard deviation to variance
  as.vector() %>%                                             # Pipe it and convert it to a vector 
  .^2
eigen
# Sum of all eigenvalues equals number of variables
sum(eigen)
## [1] 42
# Find PCs where the sum of eigenvalues is greater than or equal to 1
which(eigen >= 1)                 # Which eigenvalues are greater than 1 

# Now let's plot the variance values versus the nth PC



typeof(eigen)                 # This happens to be a list 

dim(eigen)

x_axis = c(1:42)

plot(x_axis,unlist(eigen),col="black",pch=19) %>%
abline(h=1,col="red")

# Great now we have filled circles and a line having eigenvalues > 1 
# From here we can choose the number of PCs we require 


# PVE :: Proportion of variance explained criterion::

# In statistics the R2 (aka R-squared) is a criterion to measure "goodness of fit" 
# It is simply the ratio of the predicted values variance to the actual values variance

# The PVE is a similar criterion for the same::
# It sees the explained variance by each of the PCs compared to the total model

# h2o.prcomp() provides us with the PVE and also the cumulative variance
# explained (CVE), so we just need to extract this information and plot it


# seq_along() creates a sequence from 1 to the length of the input. 
# This is really useful for for loops. seq.int() creates a sequence from one number to another. 
# You can pass a by argument to specify the step size.

typeof(my_pca@model$importance)
dim(my_pca@model$importance)
typeof(seq_along(my_pca@model$importance))
typeof(unlist(my_pca@model$importance))
dim(unlist(my_pca@model$importance))        # Doesnt have dimension 

# Extract and plot PVE and CVE
plo<-data.frame(
  PC = my_pca@model$importance %>% seq_along(),        # Just creates a sequence along the length 
  PVE = my_pca@model$importance %>% .[2,] %>% unlist(),      # variance unlisted 
  CVE = my_pca@model$importance %>% .[3,] %>% unlist()

) %>%
  tidyr::gather(metric, variance_explained, -PC)  %>%
  ggplot(aes(PC, variance_explained)) +
  geom_point() +
   facet_wrap(~ metric, ncol = 1, scales = "free")

# The first PCt in our example explains 5.46% of the feature variability, and
# the second principal component explains 5.17%. Together, the first two PCs
# explain 10.63% of the variability. Thus, if an analyst desires to choose the
# number of PCs required to explain at least 75% of the variability in our original
# data then they would choose the first 27 components.

plo
df<-data.frame(
  PC = my_pca@model$importance %>% seq_along(),        # Just creates a sequence along the length 
  PVE = my_pca@model$importance %>% .[2,] %>% unlist(),      # variance unlisted 
  CVE = my_pca@model$importance %>% .[3,] %>% unlist()
)

min(which(df$CVE>=0.75))        # First 27 PCs should be chosen to get 75 % CVE

# Cumulative Variance Explained :: CVE

# What amount of variability is reasonable? This varies by application and
# the data being used. However, when the PCs are being used for descriptive
# purposes only, such as customer profiling, then the proportion of variability
# explained may be lower than otherwise. When the PCs are to be used as
# derived features for models downstream, then the PVE should be as much as
# can conveniently be achieved, given any constraints.


# SCREE PLOT Criterion:: 

# A scree plot shows the eigenvalues or PVE for each individual PC. Most
# scree plots look broadly similar in shape, starting high on the left, falling
# rather quickly, and then flattening out at some point. This is because the first
# component usually explains much of the variability, the next few components explain a moderate amount, 
# and the latter components only explain a small
# fraction of the overall variability. The scree plot criterion looks for the “elbow”
# in the curve and selects all components just before the line flattens out

data.frame(
  PC= my_pca@model$importance %>% seq_along,         # Just some integers 
  PVE = my_pca@model$importance %>% .[2,] %>% unlist()    # The variance or the eigenvalues 
) %>%                                                  # The data-frame is piped 
  ggplot(aes(PC, PVE, label = PC)) +         # The PC values are labeled 
  geom_point() +
  geom_line() +
  geom_text(nudge_y = -.002,size=4)     # Text labels 
  # The label attribute specifies which label to use 
# If we were merely trying to profile customers we would
# probably use 8 or 10, if we were performing dimension reduction to feed into
# a downstream predictive model we would likely retain 26 or more (the exact number being based on, 
# for example, the CV results in the supervised modeling
# process). This is part of the challenge with unsupervised modeling, there is
# more subjectivity in modeling results and interpretation.

# Traditional PCA has a few disadvantages worth keeping in mind. First, PCA
# can be highly affected by outliers. There have been many robust variants of
# PCA that act to iteratively discard data points that are poorly described
# by the initial components (see, for example, Luu et al. (2019) and Erichson
#                            et al. (2018)). In Chapter 18 we discuss an alternative dimension reduction
# procedure that takes outliers into consideration, and in Chapter 19 we illustrate
# a procedure to help identify outliers.
# Also, note in Figures 17.1 and 17.2 that our PC directions are linear. Conse-
#   quently, traditional PCA does not perform as well in very high dimensional
# space where complex nonlinear patterns often exist. Kernel PCA implements
# the kernel trick discussed in Chapter 14 and makes it possible to perform
# complex nonlinear projections of dimensionality reduction. See Karatzoglou
# et al. (2018) for an implementation of kernel PCA in R. Chapters 18 and
# 19 discuss two methods that allow us to reduce the feature space while also
# capturing nonlinearity.

# Next we focus on generalized low rank models and autoencoders


library(dplyr)      # basic data manipulation and plotting
library(ggplot2)    # data visualization
library(h2o)             # performing dimension reduction 

# Refer to the book for more theory and equations 
















