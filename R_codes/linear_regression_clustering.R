# Here we do clustering and linear regression in R and then we do a simple time series data analysis after it:: 

# Linear model (lm): lm() is a linear model function in R. It can be used to create a
# simple regression model.
# 
# Generalised linear model (glm): It is specified by giving a symbolic description of
# a linear predictor and a description of the error distribution. Error distribution need not be gaussian 
# as in linear regression :: Useful for ML based classification 
# 
# Linear model for mixed effects (lme)
# 
# Non-linear least square (nls): It determines the non-linear (weighted) least-square
# estimate of the parameters of a non-linear model.
# 
# Generalised additive models (GAM): GAMs are simply a class of statistical models in
# which the usual linear relationship between the response and predictors is replaced
# by several non-linear smooth functions to model and capture the non-linearities in
# the data.


# lm(formula, data, subset, weights, na.action,
#    method = “qr”, model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
#    singular.ok = TRUE, contrasts = NULL, offset, ...)

# Necesary libraries here :: 

library(dplyr)

# So here mostly the first 3 parameters are of importance :: 

# We work with the housing dataset here :: 

house = read.table("http://www.rossmanchance.com/iscam2/data/housing.txt", header = T, sep = "\t")
# Reads into a dataframe :: typeof(house) returns a list here ::



# Let's see colnames of the house dataset ::
colnames(house)
summary(house)                 # Summarize the data here 


# Let's see the head of the dataset :: 
head(house)
house = house[rowSums(is.na(house))==0,]               # Actually no data is missing in this dataset 

# Let's ensure w ediscard the NA values if any 




# Let's normalize the features here :: 
# The City feature is non-numeric so to get it as a feature one needs to convert it into 
# categorical feature which reflects each city's importance accordingly :: Here we simply drop it 

# price is our target variable here :: 
# Let's see it's min and max values here :: 

min(house[,2])               # 89900 
max(house[,2])               # 1250000


# This range is also huge :: let's try to scale this column also 
# The scale function scales all the columns of the data and expects it to be numeric in nature 
# Instead as below, we scale only the numeric columns : 


# mutate_at is used to change specific columns 

house <- house %>% mutate_at(c("sqft", "price","bedrooms","baths"), ~(scale(.) %>% as.vector))
# This ~ is NOT well understood :: why use it before a function??
# all numeric columns are normalized here :: 
# now we fit our linear model here :: 


# "sqft"     "price"    "City"     "bedrooms" "baths"    # These are the cols in the data 

# Now let's fit a linear model to the data 
# Some general points relating correlation in R :: 

# 1. For non-linear relationships, correlation is NOT an appropriate measure of
# association. To determine whether two variables may be linearly related, a scatter
# plot can be used.
# 2. Pearson correlation can be affected by outliers. A box plot can be used to identify
# the presence of outliers. The effect of outliers is minimal for Spearman correlation.
# Therefore, if outliers cannot be manipulated or eliminated from the analysis with
# proper justification, Spearman correlation is preferred.
# 3. A correlation value close to 0 indicates that the variables are not linearly associated.
# However, these variables may still be related. Thus, it is advised to plot the data.
# 4. Correlation does not imply causation, i.e. based on the value of correlation. It
# cannot be asserted that one variable causes the other.
# 5. Correlation analysis helps in determining the degree of association only.

linear_model <- lm(price ~ sqft+bedrooms+baths,data=house)              # target and then predictor here 
# Note the syntax for multiple regression : it wasted quite some time here :: 

linear_model

summary(linear_model)

# The intercept indicates the mean of the response variable Y when all X are 0
# It is the best constant approximation minimizing SUM of squared errors 

# The coefficient, Standard Error measures the average amount that the coefficient estimates
# vary from the actual average value of our response variable.
# One can calculate the exact value of the coefficients through formula :: The standard error is the 
# error wrt this exact value 
# t value is a measure of standard deviation of our coefficient estimate 
# higher the better :: NULL hypothesis can be rejected if these values are high enough  
# measure of how many standard deviations our coefficient
# estimate is far away from 0. The further away from zero the coefficient more assurance we can give 
# about it's dependency 
# A small p-value indicates that it is unlikely we will observeLinear Regression using R

# The last column in the summary is the p-value : smaller the better due to reasons mentioned above 


# Residuals min median max 1st Quartile 3rd Quartile is given 
# We do only this much analysis for linear regression 
# One should note that we assume an underlying linear relationship exists between target and features 
# Secondly the error is jointly gaussian with diagonal scaled identity covariance matrix 
# Noise (error) is 0 mean and iid across samples and also the error vector has 0 correlation  


# Clustering needs to be done on the mtcars dataset here :: 
# We only do k-means clustering but their are the following improvements here :: 

# BFR (Bradley, Fayyad, and Reina) algorithm, a variant of the
# k-means algorithm that performs clustering in a high-dimensional Euclidean space, the
# CURE (Clustering Using REpresentatives) algorithm, the GRGPF algorithm that uses
# non-Euclidean space, the BDMO algorithm, stream-clustering algorithms (B. Babcock, M.
#                                                                        Datar, R. Motwani, L. O’Callaghan)


# In high dimensional spaces, almost all pairs of points are
# at approximately the same distance from each other and it is not intuitively clear how
# to group them. This is synonymous with the curse of dimensionality :: In high dimensions points are 
# placed far apart :: also we have to do a dense sampling from the distribution to get the representative 
# points 

# Various distance metrics could be used 

# mtcars 

head(mtcars)
summary(mtcars)
colnames(mtcars)
nrow(mtcars)                # 32 rows here 
# "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear" "carb"

# In real world applications one should analyse the data carefully first before proceeding 
# No missing values in the mtcars dataset 


# First we see hierarchical clustering here :: 
# In the bottom up approach :: initially we have a set of clusters 
# Merge the clusters one by one similarity/dissimilarity metric 
# End when you have got a pre-specified number of clusters 

dist_meas <- dist(as.matrix(mtcars))     # 496 length vector
# This is ravelled into a vector of length 32*31/2 A Lower traingular matrix is sufficient here 

hc <- hclust (dist_meas)      
# One must note that the distance is calculated using all the features here 

plot(hc)

# function; “method” argument defines the type of method used for clustering.
# This can be “ward.D”, “ward.D2”, “single”, “complete”, “average”, “median”, “centroid”,
# “mcquitty”; the dots “...” define the other optional arguments.

# Here we used the euclidean distance metric :: one can explore other methods as 
# and when needed 


# Now we move on to k-means clustering : 
# Initialize the centroids of the k clusters randomly 
# Assign a point in the data to a cluster if it's distance to the centroid of that cluster 
# is the lowest 
# Once all points are assigned recompute the centroids of the clusters 
# This follows the algorithm : mean is the minimizer of the least squares problem 
# Continue till the cluster assignment doesn't change 

# k-means(x, centers, iter.max= 10, nstart = 1, algorithm =
#           c(“Hartigan-Wong”, “Lloyd”, “Forgy”, “MacQueen”),...)

# “x” argument defines a numeric matrix of the data or an object that can be converted to
# a matrix; “centers” argument contains either the number of clusters (k) or a set of initial
# (distinct) cluster centers; “iter.max” argument defines the maximum number of iterations;
# “nstart” argument defines the number of random sets that should be chosen if centers is
# a number; “algorithm” defines the type of algorithm used for clustering; the dots, “...”
# defines the other optional arguments. 


# The number of clusters should be less than the number of columns in the distance matrix 
# This is quite obvious here 

kc <- kmeans(mtcars, 5)

# Here we do not analyse it any further :: 
# We should set the value of k such that :: The ratio of within cluster sum of squares 
# to the inter cluster distance should be minimized subject to overfitting 



# Let's do some basic time series data processing here :: 


# 
# 
# R language provides many commands that plot the given data, such as plot() , hist() ,
# pie() , boxplot() , stripchart() , curve() , abline() , qqnorm() , etc. of which plot()
# and hist() commands are mostly used in time series analysis.

# scan and ts are used to read and convert to time series data structures 

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
# Age on death of successive kings of england min 13 max 86

kingstimeseries <- ts(kings)
# This stores the data into a time series object 


# In an additive model the time series is expressed as: Y = T + S + C + I.
# So the components are : Trend Seasonal Cyclical Iregular components


# A multiplicative decomposition roughly corresponds to an additive decomposition of the logarithms. The additive decomposition is the most appropriate if the magnitude of the seasonal fluctuations, or the variation around the trend-cycle, does not vary with the level of the time series. When the variation in the seasonal pattern, or the variation around the trend-cycle, appears to be proportional to the level of the time series, then a multiplicative decomposition is more appropriate. Multiplicative decompositions are common with economic time series.
# 
# An alternative to using a multiplicative decomposition is to first transform the data until the variation in the series appears to be stable over time, then use an additive decomposition. So, basically you need to check for heteroskedasticity, eliminate that if it is there by transformations and do an additive decomposition of the transformed series.
# 
# Most common transformations are log or square root of the series and are special cases of Power transform.


plot.ts(kingstimeseries)
# We can see from the time plot that this time series could probably be described using an additive model, 
# since the random fluctuations in the data are roughly constant in size over time.


# If the random fluctutations depend upon the level of the time series 
# then we can do some nonlinear transformations and then impose an additive model here 


# A seasonal pattern exists when a series is influenced by seasonal factors (e.g., the quarter of the year, the month, or day of the week). Seasonality is always of a fixed and known period. Hence, seasonal time series are sometimes called periodic time series.
# 
# A cyclic pattern exists when data exhibit rises and falls that are not of fixed period. The duration of these fluctuations is usually of at least 2 years. Think of business cycles which usually last several years, but where the length of the current cycle is unknown beforehand.
# 
# Many people confuse cyclic behaviour with seasonal behaviour, but they are really quite different. If the fluctuations are not of fixed period then they are cyclic; if the period is unchanging and associated with some aspect of the calendar, 
# then the pattern is seasonal. 
# In general, the average length of cycles is longer than the length of a seasonal pattern, and the magnitude of cycles tends to be more variable than the magnitude of seasonal patterns.


# For example in summer the prices of fruits inflate. :: Seasonal pattern 
# Business cycles : Unpredictable and can prolong/vanish without knowledge. 


# First we look at non-seasonal time series here :: 

library(TTR) 

# Non seasonal time series : trend component and a irregular component 
# A non-seasonal time series consists of a trend component and an irregular component. 
# Decomposing the time series involves trying to separate the time series into these components, 
# that is, estimating the the trend component and the irregular component.
# 
# To estimate the trend component of a non-seasonal time series that can be described using an additive model, 
# it is common to use a smoothing method, such as calculating the simple moving average of the time series.

# The kings death time series is non-seasonal as we do not see regular periodic fluctuations here 

kingstimeseriesSMA3 <- SMA(kingstimeseries,n=8)          # Length of the averaging window here 
plot.ts(kingstimeseriesSMA3)              # Gives a roughcut analysis of the trend of the time series


# The irregular component doesn't give us much information here 


# Decomposing Seasonal data :: The king death age is not seasonal as a periodic trend is not observed 
# One can surmise the no. of briths across a decade can be a bit seasonal assuming people know the best 
# time to bring a child 

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))       # start year and start index of month
birthstimeseries


birthstimeseriescomponents <- decompose(birthstimeseries)

plot(birthstimeseriescomponents)


# trend random and seasonal are illustrated here :: trend shows we have increasing numbers 
# seasonal is a periodic regular pattern
# irregular is just irregular 


birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal

plot(birthstimeseriesseasonallyadjusted)           # no regular seasonal fuctutations found here 

# Forecasting time series data :: 
# HoltWinter's exponential smoothing forecasting :: 

library(forecast)
library(stats)

rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)

rainseriesforecasts2 <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)       # This is for the available time range

# Rain forecast prediction here :: 

rainseriesforecasts <- forecast:::forecast.HoltWinters(rainseriesforecasts2,h=8)
plot(rainseriesforecasts)

# We often want something between these two extremes.
# For example, it may be sensible to attach larger weights to more recent observations than to observations from 
# the distant past. This is exactly the concept behind simple exponential smoothing. 
# Forecasts are calculated using weighted averages, where the weights decrease exponentially as observations come from further in the past — 
# the smallest weights are associated with the oldest observations: 

# This is simple exponential smoothing here 



# Autoregressive models :: 
