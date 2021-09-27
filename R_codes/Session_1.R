
## Session 1 Introduction to R programming 


X <- c(1,-2,5.3,6,-20,4) # numeric vector
print(X)

Y <- c("one","two","three") # character vector
print(Y)

Z <- c(FALSE,TRUE,FALSE,FALSE,TRUE,FALSE) #logical vector
print(Z)

W <- c(FALSE,1,-2.0,"two")
print(W)
mode(W)

# Accessing vector elements using position.
x <- c("Jan","Feb","Mar","April","May","June","July")
y <- x[c(2,3,6)]
print(y)


# Accessing vector elements using logical indexing.
v <- x[c(TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)]
print(v)


# Accessing vector elements using negative indexing.

t <- x[c(-2,-5)]

print(t)

# Create two vectors.
v1 <- c(1,2,4,5,7,11)
v2 <- c(12,4,3,8,1,21) 

# Vector addition.
add.result <- v1 +v2
print(add.result) 

# Vector substraction.
sub.result <- v1-v2
print(sub.result) 

# Vector multiplication.
multi.result <- v1*v2
print(multi.result) 

# Vector division.
divi.result <- v1/v2
print(divi.result)

## Recycling of Vectors 
v1 <- c(1,2,4,5,7,11)
v2 <- c(4,11)    # V2 becomes c(4,11,4,11,4,11) 
add.result <- v1+ v2
print(add.result) 
sub.result <- v1-v2
print(sub.result)


# numerical vector sort
v1 <- c(1,2,4,5,7,11)
sort(v1) # sorts in ascending order which is default
sort(v1,decreasing=TRUE) # sorts in descending order  

# Character vector sort
v2 <- c("Cherry","BlueBerry","Apple","Pineapple")
sort(v2) # sorts in Alphabetical order which is default
sort(v2,decreasing=TRUE) # sorts in Reverse Alphabetical order


## Creating a Matrix 
#mymatrix <- matrix(vector, nrow=r, ncol=c, byrow=FALSE)

mymatrix <- matrix(c(1,2,3,5,6,7,9,10,12), nrow=3, ncol=3, byrow=FALSE)
mymatrix

# Access the element at 3rd column and 1st row.
print(mymatrix[1,3])

# Access the element at 2nd column and 3rd row.
print(mymatrix [3,2])

# Access only the  2nd row.
print(mymatrix [2,])

# Access only the 3rd column.
print(mymatrix [,3])

matrix1 <- matrix(c(1, 3, 5, 7), nrow = 2)
matrix2 <- matrix(c(2, 4, 6, 8), nrow = 2)

# Add the matrices.
Mat.Add <- matrix1  +  matrix2

# Subtract the matrices
Mat.Sub <- matrix1 - matrix2

# Multiply the matrices.
Mat.Multi <- matrix1 * matrix2

# Divide the matrices
Mat.Div <- matrix1 / matrix2


## Data Frames 

# create data frame in R.
subject=c("English","Maths","Chemistry","Physics") # vector1 named as subject
percentage =c(80,100,85,95) # vector2 named as percentage
students_df=data.frame(subject,percentage) # Vector1 and vector2 together as datafram

names(students_df)<-c("Course","Score")
str(students_df)
summary(students_df)


## List Objects in R 
##Lists are the R objects which contain elements of different types like ??? numbers, strings, vectors and another list inside it. A list can also contain a matrix or a function as its elements.
## List is created using list() function.

list_data <- list("Red", "Green", c(21,32,11), TRUE, 51.23, 119.1)
print(list_data)



# Create a list containing a vector, a matrix and a list.
list_data <- list(c("Jan","Feb","Mar"), matrix(c(3,9,5,1,-2,8), nrow = 2),
                  list("green",12.3))

# Give names to the elements in the list.
names(list_data) <- c("1st Quarter", "A_Matrix", "A Inner list")

# Show the list.
print(list_data)

# Access the thrid element. As it is also a list, all its elements will be printed.
print(list_data[3])

## Creating a List 
n = c(2, 3, 5) 
s = c("aa", "bb", "cc", "dd", "ee") 
b = c(TRUE, FALSE, TRUE, FALSE, FALSE) 
x = list(n, s, b, 3)   

x[c(2, 4)]  ## Slicing multiple elements of a list 

x[[2]][1]   ## Accessing inner elements 

x[[2]][1] = "sun"

x[[2]]

v = list(bob=c(2, 3, 5), john=c("aa", "bb")) 
v 
 
v[["bob"]] 
 
v$bob

### Creating List Columns  in dataframe 
library(tidyverse)
df <- tribble(
  ~x1,
  "a,b,c", 
  "d,e,f,g"
) 


df<-mutate(df,x2 = stringr::str_split(x1, ","))

df[["x2"]]

View(df)

Demand<-c(20,22,24,25,27,28)
Stock<-c(22,25,24,26,30,39)
any(Demand>Stock)
all(Demand>Stock)
X<-mean(Demand)
Y<-mean(Stock)
if(Y>X)
{print("Excess Stock")}
ifelse(X<Y,0,1)
ifelse(X<Y,"Excess Stock","Less Stock")
for(i in c(1:15))
{print(i)}
for(i in seq(2,20,2))
{print(i)}
for(i in seq(2,20,2))
{if(i%%4==0)
{print(sqrt(i))}}
for(i in seq(2,20,2))
{if(i%%2==0)
{print(sqrt(i))}}
for(i in seq(2,20,2))
{if(i%%2==0)
{print(i)}}
for(i in seq(2,20,2))
{if(i%%2==0)
{print(i)}
  if(i>=8)
  {break}} 

# for loop in R
for(i in 1:5)
{
  print (i^2)
}

x <- 1:5
for (i in x) {
  if (i == 3){
    break
  }
  print(i)
}

#"next" discontinues a particular iteration and jumps to the next cycle. In fact, it jumps to the evaluation of the condition holding the current loop.

# R for loop with next statement

x <- 1:5
for (i in x) {
  if (i == 2){
    next
  }
  print(i)
}

#R Which function for Vector:
  
  
# which function in R for vector

x <- c(1,3,5,7,8)
which(x==3)
which(x>=7)
#Returns the index position

which(letters=="z")
which(letters=="s")
#Returns the position of "Z" and "S" in the letters object.so the output will be



library(outliers)
x<-c(32,30,35,37,38,41,27,29,59,95)
outlier(x)
boxplot(x)
which.min(x)
which.max(x)


# Using functions in List Columns in dataframe 
sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)

 sim<-mutate(sim,sims = invoke_map(f, params, n = 10))

View(sim)

#paste function in R

paste('one',2,'three',4,'five')

paste('X',1:5,sep='')

paste(c('one','two','three','four'),collapse=' and ')

paste(c('X','Y'),1:5,sep='_',collapse=' and ')

paste0('X',1:5)

paste0('X',1:5,collapse=",")

x1<-rnorm(10,100,15)
x2<-runif(10,min=30,max=100)

x3<-cbind(x1,x2)
View(x3)

is.data.frame(x3)
is.matrix(x3)

## Importing CSV Files from the Net 

path1 <- "http://faculty.marshall.usc.edu/gareth-james/ISL/Advertising.csv"
data <- read.csv(path1)
View(data)

## Importing csv file via Interactive method

data2 <- read.csv(file.choose())

## Importing csv file with fread.csv used for >100 MB of data
library(data.table)
data3<- fread(path1)

## Importing Excel File 


library(xlsx)
data4<-read.xlsx(file.choose(),sheetIndex = 1)
View(data4)

?read.csv

## Important functions in Base R 

# apply
View(mtcars)
apply(mtcars,2,mean) ## apply mean function to entire dataset by columns

apply(mtcars,2,sd) ## apply sd functin to entire data by col.

apply(mtcars[2],2,mean) ## apply mean function only to the second column 

apply(mtcars,1,mean) ## apply mean by row. 1 indicates row

# lapply 

x<-list(a=c(1:10),b=rnorm(20,100,15))
lapply(x,mean)

lapply(x,function(y) y[1] ) # Lapply to create an anon functin that returns the first element.

sapply(x,mean)

test <- 1:100
r1 <- vapply(test, function(x) x < 10, logical(1))
r1
r2 <- vapply(test, function(x) {x + 10}, numeric(1))
r2
print(r)


r3

?vapply

var1 <- 1:10
var2  <- 101:110
mapply(sum, var1, var2)

rnorm(5,10,2) # to generate five random normal variables 
rnorm(1:5,11:15,2) # does not work in vectorised way  

mapply(rnorm,n=1:5,mean=11:15,sd=2) # Vecctorized version of rnorm

tapply(mtcars$mpg,mtcars$cyl,mean)

tapply(mtcars$mpg,mtcars$cyl,sd)

aggregate(mpg~mtca$cyl,mtcars,mean)

aggregate(mpg~(cyl+gear),mtcars,mean)

aggregate(mpg~(cyl+gear),mtcars,mean)

aggregate(cbind(mpg,disp)~(cyl+gear),mtcars,mean) # mpg and disp are aggregated


### Normality Testing Practical Examples 
library(nortest)
norm1<-rnorm(n=12,mean=100,sd=20)  ##p >0.05 then its normal 
ad.test(norm1)

exp<-rexp(12,3) ## Not Normal p < 0.05
ad.test(exp)

## Creating a DataSet to apply anderson darling test on multiple data points
mean1<-seq(110,200,10)
sd1<-seq(5,50,5)
length(mean1)
length(sd1)
b1<-mapply(rnorm,mean=mean1,sd=sd1,n=12)
b1<-cbind(b1,exp)
view(b1)

## Applying the Anderson Darling test on multiple columns
result<-apply(b1,2,ad.test)
View(result)

pval<-map(result,"p.value")
print(pval)


