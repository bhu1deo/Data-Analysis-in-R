# Here we start with the IRIS dataset # We do basic data exploration and test the kNN algorithm to 
# do classification here. We also try to implement Naive Bayes :: Logistic Regression and Decision tree classifiers 
# Here 

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}                                                 # This function would be used to normalize the dataset here 
# Takes in x calculates the mean and does min-max scaling here 

library(haven)
library(plyr)
library(readr)
library(ggplot2)
library(GGally)
library(dplyr)
library(mlbench)
library(class)                    # This is used for the kNN here 



library(tidyverse)
library(stringr)
library(DT)
library(grid)
library(gridExtra)
library(corrplot)
library(gmodels)

library(e1071)
library(caTools)
library(caret)

library(h2o)
library(rsample)
library(klaR)

library(aod)
library(ggplot2)

library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling functions
library(broom)

library(rpart)
library(rpart.plot)
library(e1071)
library(randomForest)

# So load the IRIS dataset here :: 

data <- iris

dim_iris <- dim(data)                   # 150 5 

str_c("The column names are as follows:")

dimnames(data)[2]                 # Only column names displayed here 

# Check missing values, if present one needs to impute them by averaging, nearest values, etc. 

missing_count <- sum(is.na(data))               # is.na returns TRUE wherever values are missing 

str_c("There are ", missing_count, " missing values")             # No missing values here 


# Data exploration :: Histogram plot here :: 

# <- is the assignment operator in R :: Higher preference than the = operator 
# %>% is the pipes function, composition of multiple functions rather than nesting 

h1 <- data %>% 
  ggplot(aes(Sepal.Length))+            # Plotting library 
  geom_histogram(aes(fill = Species), binwidth =0.2, col = "black")+       # Histogram colour is the boundary colour
  # Fill indicates the dichotomy of the columns 
  geom_vline(aes(xintercept = mean(Sepal.Length)), linetype = "dashed", color = "black")+
  # A vertical dashed black line is drawn here 
  labs(x = "Sepal Length (cm)", y = "Frequency")+        # Labels 
  theme(legend.position = "none")                   # Where to place in the grid 

h2 <- data %>% 
  ggplot(aes(Sepal.Width))+
  geom_histogram(aes(fill = Species), binwidth =0.2, col = "black")+
  geom_vline(aes(xintercept = mean(Sepal.Width)), linetype = "dashed", color = "black")+
  labs(x = "Sepal.Width (cm)", y = "Frequency")+
  theme(legend.position = "none")

h3 <- data %>% 
  ggplot(aes(Petal.Length))+
  geom_histogram(aes(fill = Species), binwidth =0.2, col = "black")+
  geom_vline(aes(xintercept = mean(Petal.Length)), linetype = "dashed", color = "black")+
  labs(x = "Petal.Length (cm)", y = "Frequency")+
  theme(legend.position = "none")

h4 <- data %>% 
  ggplot(aes(Petal.Width))+
  geom_histogram(aes(fill = Species), binwidth =0.2, col = "black")+
  geom_vline(aes(xintercept = mean(Petal.Width)), linetype = "dashed", color = "black")+
  labs(x = "Petal.Width (cm)", y = "Frequency")+
  theme(legend.position = "right")

grid.arrange(h1,h2,h3,h4, nrow=2, top = textGrob("Iris Histogram"))

# One may have to estimate the class densities and there a histogram might be of treendous use 
# BoxPlot gives variation in the feature around it's mean 

# Inferences : One can see the distribution of the features, also can deduce noise in the dataset
# One can also check whether a simple decision stump classifier would be enough for classification here 
# Two types of Box Plots : Normal Box plots and violin box plots 


# First Box Plot  :: 

v1 <- data %>% 
  ggplot(aes(Species, Sepal.Length))+
  geom_violin(aes(fill = Species))+
  geom_boxplot(width = 0.1)+
  scale_y_continuous("Sepal Length", breaks = seq(0, 10, by = .5))+
  theme(legend.position = "none")

v2 <- data %>% 
  ggplot(aes(Species, Sepal.Width))+
  geom_violin(aes(fill = Species))+
  geom_boxplot(width = 0.1)+
  scale_y_continuous("Sepal Width", breaks = seq(0, 10, by = .5))+
  theme(legend.position = "none")

v3 <- data %>% 
  ggplot(aes(Species, Petal.Length))+
  geom_violin(aes(fill = Species))+
  geom_boxplot(width = 0.1)+
  scale_y_continuous("Petal Length", breaks = seq(0, 10, by = .5))+
  theme(legend.position = "none")

v4 <- data %>% 
  ggplot(aes(Species, Petal.Width))+
  geom_violin(aes(fill = Species))+
  geom_boxplot(width = 0.1)+
  scale_y_continuous("Petal Width", breaks = seq(0, 10, by = .5))+
  theme(legend.position = "right")

grid.arrange(v1,v2,v3,v4, nrow = 2, top = textGrob("Box plot of Iris species"))


# Second Box Plot : 
# gather :: Makes a list of key value pairs here 
# facet_grid() forms a matrix of panels defined by row and column faceting variables.
# It is most useful when you have two discrete variables, and all combinations of the variables exist in the data. 
#If you have only one variable with many levels, try facet_wrap() .



gather(data, Var, value, -Species) %>% # Var and value are the two new attributes of the list 
  ggplot(aes(Var, value)) +                # Var and value are the 2 attributes here 
 geom_violin(aes(fill = Species)) +      # Violin plot         
 facet_grid(~Species) + 
theme(axis.text.x = element_text(angle = 90, vjust = .5))+        # Regular text and other theme 
 labs(x = "Measurements", y = "Length in cm", title = "Violin Boxplot of Species")+
 geom_boxplot(width=0.1)                         # Draws the boxes here 



# Now :: Average tabular measurement and also the standard deviation 
Avg_Iris <- data %>% 
  group_by(Species) %>%             # This pipe thing allows passing of objects inherently :: grouped by species here 
  summarise(Avg_sepal_length = mean(Sepal.Length), Avg_sepal_width = mean(Sepal.Width), Avg_petal_length = mean(Petal.Length),
            Avg_petal_width = mean(Petal.Width))

# A table which shows the average statistics 

Sd_Iris <- data %>% 
  group_by(Species) %>% 
  summarise(Sd_sepal_length = sd(Sepal.Length), Sd_sepal_width = sd(Sepal.Width), Sd_petal_length = sd(Petal.Length),
            Sd_petal_width = sd(Petal.Width))

datatable(Avg_Iris, caption = "Average measurement of all Species") %>% 
  formatRound(2:5,digits = 2)                  # Similar datatable for standard deviation also 

# Scatter plot :: 2 features at a time 
s1 <- data %>%                                      # Nothing significant with the scatter plot here 
  ggplot(aes(Sepal.Length, Sepal.Width))+           # X and Y axis here  
  geom_point(aes(col = Species))+                   # Classes 
  labs(x = "Sepal.Length (cm)", y = "Sepal.Width")+         # Labels and the plot indicating the classes 
  theme(legend.position = "right")

s2 <- data %>% 
  ggplot(aes(Petal.Length, Petal.Width))+
  geom_point(aes(col = Species))+
  theme(legend.position = "none")

s3 <- data %>% 
  ggplot(aes(Species))+
  geom_bar(aes(fill = Species))+
  theme(legend.position = "top")

layout <- matrix(c(1,2, 3, 3 ), 2, 2, byrow = T)
multiplot(s1, s2, s3, layout = layout)            # Or we can visualize one plot at a time not an issue 

# 2D viualization helps us separate the points according to their classes immediately if they support it 

# Correlation matrix :: Amongst the features 
# Tells us about feature correlation 
# Look at select filter arrange here 
cor_iris <- data %>% 
  select(-Species) %>%            # This - acts as a ~ here and everywhere else :: this is an artifact of copy pasting
  cor()               # Calculates the covariance of the avilable features here :: positive definite 

p.mat <- data %>% 
  select(-Species) %>% 
  cor.mtest() %>% 
  .$p                 # p values and confidence intervals :: See this once 

corrplot(cor_iris, type = "upper", method = "number", diag = F)    # Shows the correlation plot here 



# Let's normalize the dataset first :: 
# Helps numerical accuracy as well as brings all the features on the same scale 
# The normalization works best if :: The ranges for these two variables are significantly different from each other, 
# and therefore may affect the performance of "distance" sensitive algorithms.


# Each column of the dataset needs to be minimized independently 
# The new columns can be stored in a new dataset 

df <- data 

df$Sepal.Length <- normalize(data$Sepal.Length)                 # Min max scaling would bring the min and the max values to 0 and 1 respectively 
df$Sepal.Width <- normalize(data$Sepal.Width)
df$Petal.Length <- normalize(data$Petal.Length)
df$Petal.Width <- normalize(data$Petal.Width)

# colnames can be used to get the colnames : returns a character :: can be indexed 

View(df)                                  # Just to view the dataframe here 

# The class labels meaning the species column remains the same because it is not normalized ofcourse 


# This may not be the optimal way we should write an end to end function which normalizes all the columns in the 
# dataset rather than doing one by one 



# Now let's try to apply a simple kNN here 
# Tweak k to see different results 

# Let's first create a training and test dataset separately here :: 
df1 <- df[sample(nrow(df)),]              # This would result in a shuffled dataset here 

df1_train = df1[1:round(0.65*nrow(df1),0)-1,]
  
df1_test = df1[round(0.65*nrow(df1),0):nrow(df1),]     # We have our train and our test data here 

train_labels = as.numeric(as.factor(df1_train[,5]))       # Convert them into numerical labels now 
test_labels = as.numeric(as.factor(df1_test[,5]))

knn_pred <- knn(train = df1_train[,-5], test = df1_test[,-5],cl = train_labels, k=10)   # Contains the prediction labels for the test 
# dataset here 

# Evaluate the predicted labels by kNN 

CrossTable(x=test_labels,y=knn_pred,prop.chisq=FALSE)             # It's just like the confusion matrix here 
# KNN is giving 95% accuracy good overall # Normalization helps in KNN 

# Now let's apply a Naive Bayes Classifier :: Get the test and the train accuracy 
# The features are independent given the class labels 
# Corrplot would be very useful in a Naive Bayes setting :: 
# Do feature correlation analysis for samples of a particular class 
# Do this for all classes, if the features are found to be closely independent then we can conclude 
# That the naive bayes assumption would work well here :: Here we do not do but implement simple Naive Bayes 
# Also one can do laplace smoothing here :: to avoid vanishing problems of 0 probabilities 


# The greatest weakness of the naïve Bayes classifier is that it relies on an often-faulty assumption of equally important and independent features which results in biased posterior probabilities.
# Although this assumption is rarely met, in practice, this algorithm works surprisingly well. 
# This is primarily because what is usually needed is not a propensity (exact posterior probability) for each record that is accurate in absolute terms but just a reasonably accurate rank ordering of propensities.



# We implement Naive Bayes using Caret :: Give the training and the test dataset results here :: 

# The densities are estimated either by kernel density estimates or by gaussian mixture models 
# The hyperparameters to be tuned are as follows: 

# usekernel parameter allows us to use a kernel density estimate for continuous variables versus a guassian density estimate,
# adjust allows us to adjust the bandwidth of the kernel density (larger numbers mean more flexible density estimate),
# fL allows us to incorporate the Laplace smoother. :: Zero probabilities 

train_control <- trainControl(                    # Cross Validation k fold cross validation 
  method = "cv", 
  number = 10
)

# train model
nb.m1 <- train(
  x = df1_train[,-5],          # The class labels need not be present 
  y = as.factor(train_labels),
  method = "nb",                      # caret naive bayes classifier 
  trControl = train_control             # cross validation 
)

confusionMatrix(nb.m1)             # Average accuracy is very good for NB classifier here for the training data 


# Now let's see on the test dataset here :: 

pred <- predict(nb.m1, newdata = df1_test[,-5])
confusionMatrix(pred, as.factor(test_labels))           # Works well for the test data also 
# NB classifier is done, one can do dimensionality reduction techniques 
# Tuning the NB classifier in order to improve :: we just normalised the data here

# Now logistic regression classifier 
# Sigmoid of the linear function learnt via MAX LIKELIHOOD 
# glms are different from lms lms model the error as a gaussian distribution 
# glm can model the error as a binomial distribution as in the case of logistic regression and some other noise 
# functions also :: Also it can have a generalized non-inear mapping from input to output (eg. sigmoid) unlike linear 
# models 


# Here we want the independent variables to be all the features instead of a single feature ::
# For this classification task we need one vs rest or one vs one 
# We do one vs one here 

# We do only setosa and versicolor dichotomy here :: both for the training and the test data 
# First filter the dataset according to the above class labels here :: 

df1_train_new = filter(df1_train,df1_train$Species=="setosa" | df1_train$Species=="virginica")
df1_test_new = filter(df1_test,df1_test$Species=="setosa" | df1_test$Species=="virginica")

train_labels_new = as.numeric(as.factor(df1_train_new[,5]))       
test_labels_new = as.numeric(as.factor(df1_test_new[,5]))
# train_labels_new<-replace(train_labels_new, train_labels_new==2,1) 
# test_labels_new<-replace(test_labels_new, test_labels_new==2,1) 
train_labels_new<-replace(train_labels_new, train_labels_new==3,0) 
test_labels_new<-replace(test_labels_new, test_labels_new==3,0)

model <- glm( train_labels_new ~., data = df1_train_new[,-5], family = binomial)
# Summarize the model
summary(model)

probabilities_train <- model %>% predict( df1_train_new[,-5], type = "response")
probabilities_test <- model %>% predict( df1_test_new[,-5], type = "response")

predicted.classes_train <- ifelse(probabilities_train > 0.5, 1, 0)
predicted.classes_test<- ifelse(probabilities_test > 0.5, 1, 0)

# Get the accuracy here :: 
mean(predicted.classes_train == train_labels_new)
mean(predicted.classes_test == test_labels_new)            # One can do One vs Rest for different classes similarly 

# One can also plot the linear decision boundary in 2D 

# Now decision tree 
# Partwise linear classifiers here sometimes boundaries parallel to the feature axis 
# Decision trees do not require feature scaling :: also if properly tuned then it stops prematurely 
# Thus it's VC dimension can be quite low :: which means large amount of data is not always necesary 
# It might be useful to visualize small decision trees here 
fit <- rpart(train_labels_new~., data = df1_train_new[,-5], method = 'class')
rpart.plot(fit, extra = 106)             # Based on one feature itself it is able to classify here 


predict_unseen <-predict(fit, df1_test_new[,-5], type = 'class') 

table_mat <- table(test_labels_new, predict_unseen)
table_mat            

# One can see the different hyperparameters for the decision trees and then see the plots 


# Now we will do classification using SVM here :: 

# SVM on the IRIS dataset using different kernels here :: SVM requires +-1 as the class labels!!!!

train_labels_new<-replace(train_labels_new, train_labels_new==0,-1) 
test_labels_new<-replace(test_labels_new, test_labels_new==0,-1)

fit <- svm(train_labels_new~., data = df1_train_new[,-5], scale = FALSE, kernel = "radial", cost = 5)



predict_unseen <-predict(fit, df1_test_new[,-5], type = 'class')          # On the Test dataset here 

 
predict_unseen<-replace(predict_unseen, predict_unseen>=0,1) 
predict_unseen<-replace(predict_unseen, predict_unseen<0,0)
predict_unseen<-replace(predict_unseen, predict_unseen==0,-1)


table_mat <- table(test_labels_new, predict_unseen)
table_mat                                                    # Just like the confusion matrix 


# So we have done for the radial basis function we can also find the number of support vectors etc. 
# We can also plot the decision boundaries as we do using meshgrid in python 

## Random Forest Classifier for IRIS dataset :: 

# Bagging (bootstrap aggregating) regression trees is a technique that can turn a single tree model with high variance and poor predictive power into a fairly accurate prediction function.
# Unfortunately, bagging regression trees typically suffers from tree correlation, which reduces the overall performance of the model.
# Random forests are a modification of bagging that builds a large collection of de-correlated trees and have become a very popular “out-of-the-box” learning algorithm that enjoys good predictive performance.
# This tutorial will cover the fundamentals of random forests.


# This characteristic is known as tree correlation and prevents bagging from optimally reducing variance of the predictive values.
# In order to reduce variance further, we need to minimize the amount of correlation between the trees.
# This can be achieved by injecting more randomness into the tree-growing process.
# Random forests achieve this in two ways:

# Bootstrap: similar to bagging, each tree is grown to a bootstrap resampled data set, which makes them different and somewhat decorrelates them.
# Split-variable randomization: each time a split is to be performed, the search for the split variable is limited to a random subset of m of the p variables. 
# For regression trees, typical default values are m=p/3
# but this should be considered a tuning parameter. 
# When m=p, the randomization amounts to using only step 1 and is the same as bagging.

# The Randomforest package will be used here :: It doesn't scale well but we use it for our simple task here ::


randforest <- randomForest(
  formula = train_labels ~ .,
  data    = df1_train[,-5]
)
predict_unseen <-predict(randforest, df1_test[,-5], type = 'class') 

# Find the closest integer to the prediction and then put into that class for random forest here :: 
predict_unseen<-replace(predict_unseen, predict_unseen<=1.5,1)
predict_unseen<-replace(predict_unseen, predict_unseen>=1.5 & predict_unseen<=2.5,2)
predict_unseen<-replace(predict_unseen, predict_unseen>2.5,3)




table_mat <- table(test_labels, predict_unseen)
table_mat                                               # Okay confusion amtrix looks good here We have done a simple 
# Random forest 

# Random forests are fairly easy to tune since there are only a handful of tuning parameters. Typically, the primary concern when starting out is tuning the number of candidate variables to select from at each split. However, there are a few additional hyperparameters that we should be aware of. Although the argument names may differ across packages, these hyperparameters should be present:
#   
#   ntree: number of trees. We want enough trees to stabalize the error but using too many trees is unncessarily inefficient, especially when using large data sets.
# mtry: the number of variables to randomly sample as candidates at each split. When mtry =p
# 
# the model equates to bagging. When mtry =1
# the split variable is completely random, so all variables get a chance but can lead to overly biased results. A common suggestion is to start with 5 values evenly spaced across the range from 2 to p.
# sampsize: the number of samples to train on. The default value is 63.25% of the training set since this is the expected value of unique observations in the bootstrap sample. Lower sample sizes can reduce the training time but may introduce more bias than necessary. Increasing the sample size can increase performance but at the risk of overfitting because it introduces more variance. Typically, when tuning this parameter we stay near the 60-80% range.
# nodesize: minimum number of samples within the terminal nodes. Controls the complexity of the trees. Smaller node size allows for deeper, more complex trees and smaller node results in shallower trees. This is another bias-variance tradeoff where deeper trees introduce more variance (risk of overfitting) and shallower trees introduce more bias (risk of not fully capturing unique patters and relatonships in the data).
# maxnodes: maximum number of terminal nodes. Another way to control the complexity of the trees. More nodes equates to deeper, more complex trees and less nodes result in shallower trees.

## Bias Variance tradeoff :: We look at model performance via the BV tradeoff lens 
# Train the model on different datasets :: given a new datapoint calculate the average prediction across datasets 
# Calculate variance via the sample variance estimate here :: 


# Bias is related to the complexity of the model, variance is related to data availability 
# More data available means more representative data points would be produced in the samples 
# This would lead to smoother less varying estimates 
# Also bias depends upon the amount of data available as compared to the model complexity 



# Our Base classifier would be kNN :: 
# Do a for loop of 10 iterations :: In each sample from the training data 
# Then train model on it :: Assess performance on unseen validation data 
# Then calculate the variation of the accuracies so obtained here ::




bias_variance <- function(input) {
  # print(nrow(input))
  acc <- list()                    # An empty list initially 
  # We have the input dataset here 
  for(i in 1:10) {
    # Shuffle the training data and choose 90% for training 
    dat <- input[sample(nrow(input)),]                   # shuffle 
    
    # choose 90% for training here and rest for validation here :: 
    dat_train = dat[1:round(0.8*nrow(dat),0)-1,]
    dat_val = dat[round(0.8*nrow(dat),0):nrow(dat),]       # The validation and the training datasets here 
    # Extract train and validation labels here ::
    train_labels = as.numeric(as.factor(dat_train[,5]))       # Convert them into numerical labels now 
    val_labels = as.numeric(as.factor(dat_val[,5]))
    
    
    
    # Train kNN model ::
    knn_pred <- knn(train = dat_train[,-5], test = dat_val[,-5],cl = train_labels, k=10) 
    
    total = length(knn_pred)            # Total number of samples here 
    
    cou = 0 
    
    for(j in 1:total)
    {
      pred = knn_pred[j]         # prediction here 
      
      actual = val_labels[j]          # Actual class label 
      
      if(actual==pred)
      {
        cou = cou + 1 
      }
    }
    acc[i] <- cou/total
    # Check accuracy on independent validation data and return it ::
    
    # predict(knn_pred, dat_val[,-5], type = 'class')
  }
  return(acc) 
}

accuracy_list = bias_variance(df1_train)             # Note that this dataset is already normalized here 

# Great we have our list of accuracies : One can now calculate the bias and the variamnce of the model 
# This would lead us to conclude that KNN works quite well on this dataset 

  

