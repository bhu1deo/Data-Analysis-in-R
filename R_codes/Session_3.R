

library(MASS)
library(tidyverse)
library(NMF)
library(ISLR)
library(ggplot2)
library(splines)
library(e1071)
library(caret)
library(caretEnsemble)
library(corpcor)

#SVD  Testing and Concept#
a<-c(1,2,3,4,5,6,5,7,9)
P<-matrix(a,nrow=3,ncol=3,byrow=TRUE)
P
qr(P)
svd(P)
t1<-svd(P)

D=diag(t1$d)
U=t1$u
Vt=t(t1$v)

A<-U%*%D%*%Vt
A

# Applying K Rank Approximation for Recommender System

a<-c(3,4,3,0,0)
b<-c(3,3,3,0,0)
c<-c(4,4,4,0,0)
d<-c(5,5,5,0,0)
e<-c(0,0,0,4,4)
f<-c(0,0,0,5,5)
g<-c(0,0,0,2,2)
RR<-rbind(a,b,c,d,e,f,g)
RR
SVDRR<-svd(RR)
SVDRR

#K Best Rank Approximation

D<-diag(SVDRR$d)
D
D<-D[1:2,1:2]
D
U<-SVDRR$u
U1<-U[1:7,1:2]
U1
V<-SVDRR$v

Vt<-t(V)
V1<-Vt[1:2,1:5]
V1

##Making a Prediction ##

a<-c(3,3,1,0,0)
a%*%V

# Faster SVD 

library(corpcor)
FSVD1<-fast.svd(RR)
FSVD1<-fast.svd(RR,2)
FSVD1

##Making a Prediction ## Mapping User to Subspace

a<-c(3,3,1,0,0)
a%*%FSVD1$v
t(FSVD1$v)%*%(a)

b<-c(0,0,5,4,4)
t(FSVD1$v)%*%(b)

c<-c(0,0,0,3,3)
t(FSVD1$v)%*%(c)


# X ~ WH' 
# X is an n x p matrix
# W = n x r  user feature matrix
# H = r x p  movie feature matrix

# get ratings for 5 users on 4 movies
x1 <- c(5,4,1,1)
x2 <- c(4,5,1,1)
x3 <- c(1,1,5,5)
x4 <- c(1,1,4,5)
x5 <- c(1,1,5,4)

R <- as.matrix(rbind(x1,x2,x3,x4,x5)) # n = 5 rows p = 4 columns 

set.seed(12345)

res <- nmf(R, 4,"lee") # lee & seung method

V.hat <- fitted(res) 
print(V.hat) # estimated target matrix

w <- basis(res) #  W  user feature matrix matrix
dim(w) # n x r (n= 5  r = 4)
print(w) 

h <- coef(res) # H  movie feature matrix
dim(h) #  r x p (r = 4 p = 4)
print(h) 


movies <- data.frame(t(h))
print(movies)
features <- cbind(movies$X1,movies$X2)
print(features)
plot(features)
title("Movie Feature Plot")


## Preprocessing technique Box Cox Transformation 

attach(mtcars)
M<-lm(mpg~hp)
plot(M)

transform_bc<- boxcox(M,lamda=seq(-3,3))

transformed_model<- lm((mpg^-0.8) ~ hp)
plot(transformed_model)
transformed_model1<- lm((mpg^0.8) ~ hp)
plot(transformed_model1)

library(outliers)
outlier(mtcars)
boxplot(mtcars$hp)

which(mtcars$hp==335)
mtcars1<-mtcars[-31,]
nrow(mtcars1)
nrow(mtcars)


M1<-lm(mtcars1$mpg~mtcars1$hp)
plot(M1)

transform_bc<- boxcox(M1,lamda=seq(-3,3))

transformed_model2<- lm((mtcars1$mpg^-0.8) ~ mtcars1$hp)

plot(transformed_model2)


## Polynomial Regression
data(Wage)
?Wage

fit_1 = lm(wage~age, data = Wage)
fit_2 = lm(wage~poly(age,2), data = Wage)
fit_3 = lm(wage~poly(age,3), data = Wage)
fit_4 = lm(wage~poly(age,4), data = Wage)
fit_5 = lm(wage~poly(age,5), data = Wage)
print(anova(fit_1,fit_2,fit_3,fit_4,fit_5))

print(coef(summary(fit_5)))


fit = lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))


# Get min/max values of age using the range() function
agelims = Wage %>%
  select(age) %>%
  range

# Generate a sequence of age values spanning the range
age_grid = seq(from = min(agelims), to = max(agelims))

# Predict the value of the generated ages,
# returning the standard error using se = TRUE
preds = predict(fit, newdata = list(age = age_grid), se = TRUE)

# Compute error bands (2*SE)
se_bands = cbind("upper" = preds$fit+2*preds$se.fit, 
                 "lower" = preds$fit-2*preds$se.fit)

ggplot() +
  geom_point(data = Wage, aes(x = age, y = wage)) +
  geom_line(aes(x = age_grid, y = preds$fit), color = "#0000FF") +
  geom_ribbon(aes(x = age_grid, 
                  ymin = se_bands[,"lower"], 
                  ymax = se_bands[,"upper"]), 
              alpha = 0.3) +
  xlim(agelims) +
  labs(title = "Degree-4 Polynomial")


# Using Splines
agelims = Wage %>%
  select(age) %>%
  range

# Generate a sequence of age values spanning the range
age_grid = seq(from = min(agelims), to = max(agelims))

# Fit a regression spline using basis functions
fit = lm(wage~bs(age, knots = c(25,40,60)), data = Wage)

# Predict the value of the generated ages, 
# returning the standard error using se = TRUE
pred = predict(fit, newdata = list(age = age_grid), se = TRUE)

# Compute error bands (2*SE)
se_bands = with(pred, cbind("upper" = fit+2*se.fit, 
                            "lower" = fit-2*se.fit))

# Plot the spline and error bands
ggplot() +
  geom_point(data = Wage, aes(x = age, y = wage)) +
  geom_line(aes(x = age_grid, y = pred$fit), color = "#0000FF") + 
  geom_ribbon(aes(x = age_grid, 
                  ymin = se_bands[,"lower"], 
                  ymax = se_bands[,"upper"]), 
              alpha = 0.3) +
  xlim(agelims)

# Using ns to split natural splines
fit2 = lm(wage~ns(age, df = 4), data = Wage)
pred2 = predict(fit2, newdata = list(age = age_grid), se = TRUE)

# Compute error bands (2*SE)
se_bands2 = with(pred, cbind("upper" = fit+2*se.fit, 
                             "lower" = fit-2*se.fit))

# Plot the natural spline and error bands
ggplot() +
  geom_point(data = Wage, aes(x = age, y = wage)) +
  geom_line(aes(x = age_grid, y = pred2$fit), color = "#0000FF") + 
  geom_ribbon(aes(x = age_grid, 
                  ymin = se_bands2[,"lower"], 
                  ymax = se_bands2[,"upper"]), 
              alpha = 0.3) +
  xlim(agelims)


## Support Vector Machines
data(iris)
View(iris)
x <- subset(iris, select=-Species)
y <- iris$SSpecies

svm_fit1 <- svm(Species ~ ., data=iris)
summary(svm_fit1)


pred <- predict(svm_fit1,x)

table(pred,y)


### IRIS with Train and Test Sets 

set.seed(3033)
intrain <- createDataPartition(y = iris$Species, p= 0.6, list = FALSE)
trainiris <- iris[intrain,]
testiris <- iris[-intrain,]
nrow(trainiris)
nrow(testiris)

svm_fit2 <- svm(trainiris$Species ~ ., data=trainiris)
summary(svm_fit2)

pred <- predict(svm_fit2,testiris)


table(pred,testiris$Species)



svm_fit3 <- svm(trainiris$Species ~ ., data=trainiris,kernal="radial",gamma=1,cost=100)
summary(svm_fit3)

pred <- predict(svm_fit3,testiris)

table(pred,test$Species)

# Example SVM
library(MASS)
data(cats)

m1 <- svm(Sex~.,data = cats,cost=10,kernal="radial",scale=FALSE)
plot(m1, cats)
m2 <- svm(Sex~.,data = cats,cost=10,kernal="quadratic",scale=FALSE)
plot(m2,cats)
m3 <- svm(Sex~.,data = cats,cost=10,kernal="quadratic",scale=TRUE)
plot(m3,cats)

# Using Caret Ensemble to run multiple algorithms 
trainControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)

algorithmList <- c('rf', 'svmRadial')

set.seed(100)
models <- caretList(Species ~ ., data=iris, trControl=trainControl, methodList=algorithmList) 
results <- resamples(models)
results
summary(results)

# Box plots to compare models
bwplot(results)









