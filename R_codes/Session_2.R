library(tidyverse)
library(plotly)
library(shiny)
library(dplyr)
library(corrplot)
library(gapminder)
library(fpc)
library(factoextra)
library(dbscan)
library(FactoMineR)
library(ranger)
library(mlbench)
library(ISLR)
library(caret)
library(glmnet)
library(glmnetcr)
library(rio)
## Some more basis Commands from dplyr

?mtcars              # Motor Trend Car Road Tests
attach(mtcars)
View(mtcars)
print(dim(mtcars))                 # 32 cars 11 features 
filter(mtcars,mpg>20)              # Filter according to arguments 
filter(mtcars,mpg>20 & cyl==6)     
filter(mtcars,mpg>20 & cyl==4)

# By Row Number

slice(mtcars, 10:15)            # Car number 10 to 15 both included
slice(mtcars, c(10:15,25:30))

# Sorting Data 
arrange(mtcars, mpg)               # Sort on the basis of miles per gallon 
arrange(mtcars,desc(mpg))          # Sort in descending order, according to mpg

## Selecting Data
head(select(mtcars, mpg, hp))       # Select 2 arguments mpg and hp and display first few cars 
head(select(mtcars, -mpg, -hp))     # Select all arguments except mpg and hp and display first few cars
head(select(mtcars, mpg:hp))        # Select all arguments from mpg to hp both included
head(select(mtcars, starts_with('m')))  # Features starting from m 

## Adding Columns 
mtcars       # This feature doesn't exist
new_mtcars=mutate(mtcars, wt_mpg = wt/mpg)  # Add a feature to the dataframe 
is.data.frame(new_mtcars)      # TRUE this is a dataframe 
print(new_mtcars)        # Let's see the newmtcars dataframe 
mtcars$wt_mpg              # This dowsn't have attribute wt_mpg     
new_mtcars$wt_mpg          # This has attribute wt_mpg
print(dim(mtcars))
print(dim(new_mtcars))      # 12 features 32 car entries 
## Group By 


# All 3 functions are carried out simultaneously 
mtcars %>%
  group_by(cyl) %>%
  summarise(cyl_count=n()) %>%
  arrange(desc(cyl_count))

?mean 

mtcars %>%
  group_by(cyl) %>%
  summarise(mean_mpg = mean(mpg, na.rm = TRUE)) # NA values should be stripped before the computation proceeds


## Using List for multiple retrun values in summary function
(list(quantile(mpg)))           # Gives you quartiles in a list
Q<-mtcars %>%
  group_by(cyl) %>%
  summarise(quantile_mpg = list(quantile(mpg)))
Q
View(Q)

?sample_n

mtcars %>% sample_n(5)

(x <- 1:30)
case_when(
  x %% 5 == 0 ~ "Div5",
  x %% 7 == 0 ~ "Div7",
  TRUE ~ as.character(x)
)

?band_members

is.data.frame(band_members)      # It is a dataframe 
band_members       # Name ad Surname
band_instruments   # Name and Instrument Name 

# Joining data combining 2 dataframes
band_members %>% left_join(band_instruments,by="name")   # printing tells you what this does 

band_members %>% right_join(band_instruments,by="name")  # printing tells you what this does 

band_members %>% full_join(band_instruments)         # Joins fully 


# To detect the point of change in character variable or for differencing to generate statistical forecast
x <- 1:24
lead(x)
lead(x,2)

lag(x,1)
lag(x,12)

## Mapping Functions in Purrr 
?lm
?split
mtcars %>%
  .$cyl
?map

mtcars   %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .))   # mpg vs wt linear regression is done here 



mtcars %>%
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")               # R2 close to 1 is good 

mtcars %>%
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ hp, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")       # R2 close to 0 is bad 


x <- list(1, 1, 1)
y <- list(10, 20, 30)
z <- list(100, 200, 300)

?map2

map2(x, y, ~ (.x + .y)/(.x - .y))  # So here elementwise function mapping is done, (1+10)/(1-10)=-1.22 and so on

pmap(list(x, y, z), function(first, second, third) (first + third) * second)   # again elementwise


#Performing Exploratory Data Analaysis 

?geom_point()
ggplot(iris, aes(Sepal.Length, Petal.Length)) +
 geom_point()

ggplot(iris, aes(Sepal.Length, Petal.Length)) +
geom_point(shape=4)


ggplot(iris, aes(Sepal.Length, Petal.Length, colour=as.factor(Species)))+
geom_point()            # arranged according to colour 

ggplot(iris, aes(Sepal.Length, Petal.Length, colour=Species))+
geom_point() +
geom_jitter()

ggplot(iris, aes(Sepal.Length, Petal.Length, colour=Species)) +
 geom_point(shape=1) +
 geom_smooth(method=lm)                            # Fit a linear regression line

ggplot(iris, aes(Sepal.Length, Petal.Length, colour=Species)) +
  geom_point(shape=1) +
  geom_smooth(method=lm,se=FALSE)

data(mtcars) 
ggplot(mtcars,aes(mpg,hp,color=cyl))+
geom_point()

ggplot(mtcars,aes(mpg,hp,color=as.factor(cyl)))+
  geom_point()           # use as.factor 

ggplot(mtcars,aes(mpg,hp))+
  geom_point()+
  facet_grid(vars(cyl))

ggplot(mtcars,aes(mpg,hp))+
  geom_point()+
  facet_wrap(vars(cyl))


ggplot(mtcars,aes(x=mpg))+
  geom_histogram()         # How the 32 cars are distributed across the mpg values in the histogram

ggplot(mtcars,aes(x=mpg))+
  geom_histogram()+
  facet_wrap(vars(cyl))   # 4 6 8 cyl values are split

ggplot(mtcars,aes(y=mpg))+
  geom_boxplot()

ggplot(mtcars,aes(y=mpg))+
  geom_boxplot()+
  facet_wrap(vars(cyl))

## Using ggplot + plotly combnation for Bubble plot 

p1<-ggplot(mtcars,aes(mpg,hp,color=as.factor(cyl)))+   # mpg vs hp for the cars considered 
  geom_point()

ggplotly(p1)

p2<-ggplot(mtcars,aes(mpg,hp,color=as.factor(cyl),size=wt))+   # The size of the blobs would be according to the weight of the cars 
  geom_point(alpha=0.5)

ggplotly(p2)

### Correaltion Plots 

corr1<-cor(mtcars)
print(corr1)                   # The Correlation Matrix of features is detailed here 
# A heat Map may be used to get the correlation matrix to be viewable 
corrplot(corr1,meth)
corrplot(corr1,method="pie")   # Indication of the portion of the pie 

library(FactoMineR)
library(rio)
install_formats()
mtcars_pca <- PCA(mtcars)        # Principal Component Analysis 



mtcars_pca$eig      # Eigenvalue and percentage Variation Cumulative Variance 

mtcars_pca$var$cos2        

mtcars_pca$var$contrib      # Contribution of Variable to the altered dimension; mtg, disp and cyl contribute heavily 

?dimdesc
lis<-dimdesc(mtcars_pca)          # Correlation of Dim with variable
lis
plot(mtcars_pca)



## Regression Understanding How to use variable transformation and interaction
## to improve model performance of linear regression

# Regression #
attach(mtcars)

# Standard error and R_squared are the two metrics for measuring good_ness_of_fit
M<-lm(mpg~hp+wt+disp)  # Linear Model of mpg vs Hp,Wt,Disp
summary(M)

M1<-lm(mpg~hp+wt)
summary(M1)

plot(mpg~wt)
plot(mpg~hp)
plot(mpg~log10(hp))      # On log10 scale 

M2<-lm(mpg~log(hp)+wt)
summary(M2)

M3<-lm(mpg~log(hp)*wt)
summary(M3)

factor(am)
am
hp
factor(hp)
M4<-lm(mpg~log(hp)*wt+factor(am))
summary(M4)


## Regression Applying to  Grouped Data Set 

by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()

by_country
View(by_country)

by_country$data             # This is data for all 142 countries
by_country$data[[1]]                  # This is 1st country's data 


country_model <- function(df) {
  lm(lifeExp ~ year, data = df)            # A BIT DOUBTFUL
}


lm(unlist(by_country$data[[1]]['lifeExp']) ~ unlist(by_country$data[[1]]['year']))   # This works 

#by_country$data[[1]]['lifeExp']
#?mutate
#?lm
#map(by_country$data[[1]], country_model)

map(by_country$data,country_model)              # This works 


by_country <- by_country %>% 
  mutate(model = map(data, country_model))
View(by_country)       # The lm is mutated here 


by_country <- by_country %>% 
  mutate(
    summary1 = map(model,summary)        # The summary is mutated here 
  )

names(by_country)

by_country[[5]][59]     ## India 
by_country[[5]][10]
by_country[[5]][118]  ## South Africa
by_country[[5]][108]  ## Rawanda

names(by_country)

a1<-map_dbl(by_country$summary1,"r.squared")

View(a1)
a2<-as.vector(by_country$country)
View(a2)
final<-cbind(a2,a1)
View(final)

diamonds

# Fit lm model: model
model <- lm(price ~ ., diamonds)

# Predict on full data: p
p <- predict(model, diamonds)

# Compute errors: error
errors = p - diamonds$price

any(is.na(errors))           # No NA in errors

# Calculate RMSE
sqrt((mean(errors^2)))      # MSE

diamonds
?split
?round
nrow(diamonds)
# Determine row to split on: split
split<-round(nrow(diamonds)*.80)           # Training and testing data 

# Create train
train<-diamonds[1:split,]     # Training dataset

# Create test
test<-diamonds[(split+1):nrow(diamonds),]      # Test dataset 

# Fit lm model on train: model
model<-lm(price~.,data=train)   # Linear Regression Model

# Predict on test: p
p<-predict(model,test)

# Compute errors: error
error<-p-test$price

# Calculate RMSE
sqrt(mean(error^2))

## 
?Hitters
Hitters = na.omit(Hitters)   # Omit NA entries 


x = model.matrix(Salary~., Hitters)[,-1]

y = Hitters$Salary

# ridge Regression to estimate parameters
?glmnet
fit.ridge<-glmnet(x,y,alpha=0)

plot(fit.ridge,xvar="lambda",labels=TRUE)

cv.ridge = cv.glmnet(x,y,alpha=0)

plot(cv.ridge)

coef(cv.ridge)

# laso to estimate the number of parameters and their coefficeints
fit.lasso<-glmnet(x,y,alpha=1)

plot(fit.lasso,xvar="lambda",labels=TRUE)

cv.lasso = cv.glmnet(x,y,alpha=1)

plot(cv.lasso)

coef(cv.lasso)


## Logistic Regression with Sonar Data Set 

data(Sonar)

# Get the number of observations
n_obs<-nrow(Sonar)

# Shuffle row indices: permuted_rows
permuted_rows<-sample(n_obs)

# Randomly order data: Sonar
Sonar_shuffled<-Sonar[permuted_rows,]

# Identify row to split on: split
split <- round(n_obs * 0.60)

# Create train
train<-Sonar_shuffled[1:split,]

# Create test
test<- Sonar_shuffled[(split+1):nrow(Sonar_shuffled),]

# Fit glm model: model
model<-glm(Class ~ ., family = "binomial", train)

# Predict on test: p
p<-predict(model,test,type="response")

# If p exceeds threshold of 0.5, M Postive cases else R: m_or_r
m_or_r <- ifelse(p > 0.5, "M", "R")

# Convert to factor: p_class
p_class <- factor(m_or_r, levels = levels(test[["Class"]]))

# Create confusion matrix
confusionMatrix(p_class, test[["Class"]])


# If p exceeds threshold of another 0.9 instead of 0.5, M else R: m_or_r
m_or_r <- ifelse(p > 0.9, "M", "R")

# Convert to factor: p_class
p_class <- factor(m_or_r, levels = levels(test[["Class"]]))

# Create confusion matrix
confusionMatrix(p_class, test[["Class"]])


# UnSupervised Learning 

# LOAD DATA ----
data(iris)
plot(iris)

# SCALE DATA ----
irisScaled <- scale(iris[, -5]) # Scaling of features helps algorith to converge

View(irisScaled)

# K-MEANS CLUSTERING ----
## CLUSTERING
fitK <- kmeans(irisScaled[, -5], 3)
fitK
str(fitK)
fitK$cluster
plot(iris, col = fitK$cluster)
table(iris[,5],fitK$cluster)

plot(irisScaled[,c("Sepal.Length", "Sepal.Width")], col=fitK$cluster)
points(fitK$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=23, cex=3)


# DENSITY-BASED CLUSTERING ----
install.packages("dbscan")
library(dbscan)
fitD <- dbscan(irisScaled, eps = 0.7, minPts = 5)
fitD
fitD$cluster
table(iris[,5],fitD$cluster)
plot(irisScaled[,c("Sepal.Length", "Sepal.Width")], col=fitD$cluster)

#  DBSCAN using fpc package for complex shapes 

data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]
df


# Compute DBSCAN using fpc package
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)
# Plot DBSCAN results
plot(db, df)

plot.dbscan(db, df)

fviz_cluster(db, df, geom = "point")

### Random Forecast Example using Caret and Ranger

data(wine)
View(wine)


train.idx <- sample(nrow(wine), 0.9 * nrow(wine))
wine.train <- wine[train.idx, ]
wine.test <-  wine[-train.idx, ]


model1 <- train(
  Overall.quality~.,
  tuneLength = 1,
  data = wine.train, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)

predict(model1,wine.test)

## tune length is changed from 1 to 3 

model2 <- train(
  Overall.quality ~.,
  tuneLength = 3,
  data = wine.train, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)


predict(model2,wine.test)


