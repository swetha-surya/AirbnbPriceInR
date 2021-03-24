install.packages("fastDummies")
install.packages("MASS")
install.packages("e1071")
install.packages(c("caret", "ISLR", "pROC", "pROC", "corrplot", "psych", "ggpubr"))

library(fastDummies)
library(caret)
library(MASS)
library(e1071)
library(psych)
library(corrplot)
library(ISLR)

setwd("/Users/....")
getwd()
AirbnbNyc <- read.csv('listings_detail_edited.csv')
#review data structure
str(AirbnbNyc)
names(AirbnbNyc)
head(AirbnbNyc)
summary(AirbnbNyc)
#View(AirbnbNyc)
# display total number of rows and columns
ncol(AirbnbNyc)
nrow(AirbnbNyc)
# display number of null values in the entire table
sum(is.na (AirbnbNyc))
colSums(is.na(AirbnbNyc))

AirbnbNyc.copy <- AirbnbNyc
head(AirbnbNyc.copy)
#to remove special characters from Host Response Rate
typeof(AirbnbNyc$host_response_rate)
head(AirbnbNyc$host_response_rate)
# remove % and replace with nothing
AirbnbNyc$host_response_rate <- gsub("%", "", AirbnbNyc$host_response_rate, fixed = TRUE)
head(AirbnbNyc$host_response_rate)
typeof(AirbnbNyc$host_response_rate)
#Remove all the rows with N/A
AirbnbNyc <- AirbnbNyc[- grep("N/A", AirbnbNyc$host_response_rate),]
#to view this column
#View(AirbnbNyc$host_response_rate)
# see the length of all N/A
length(grep("N/A", AirbnbNyc$host_response_rate))
#changing the data type to numberic again
AirbnbNyc$host_response_rate <- as.numeric(AirbnbNyc$host_response_rate)
#View(AirbnbNyc)
summary(AirbnbNyc)
#backup 
AirbnbNyc.copy <- AirbnbNyc
#-----------------------------
#to remove special characters from price
typeof(AirbnbNyc$price)
head(AirbnbNyc$price)
# remove % and replace with nothing
AirbnbNyc$price <- gsub("$", "", AirbnbNyc$price, fixed = TRUE)
head(AirbnbNyc$price)
typeof(AirbnbNyc$price)
# see the length of all N/A
length(grep("N/A", AirbnbNyc$price))
#to view this column
#View(AirbnbNyc$price)
#changing the data type to character
AirbnbNyc$price <- as.numeric(AirbnbNyc$price)
# there is a warning that NAs are introduced by coercion but there are no NAs in the column 
# so we continue with the data type change
length(grep("NA", AirbnbNyc$price))
#backup 
AirbnbNyc.copy <- AirbnbNyc
#-----------------------------
#to remove special characters from cleaing fee
typeof(AirbnbNyc$cleaning_fee)
head(AirbnbNyc$cleaning_fee)
# remove % and replace with nothing
AirbnbNyc$cleaning_fee <- gsub("$", "", AirbnbNyc$cleaning_fee, fixed = TRUE)
head(AirbnbNyc$cleaning_fee)
typeof(AirbnbNyc$cleaning_fee)
# see the length of all N/A
length(grep("N/A", AirbnbNyc$cleaning_fee))
#to view this column
#View(AirbnbNyc$cleaning_fee)
#changing the data type to character
AirbnbNyc$cleaning_fee <- as.numeric(AirbnbNyc$cleaning_fee)
# there is a warning that NAs are introduced by coercion but there are no NAs in the column
length(grep("NA", AirbnbNyc$cleaning_fee))
#backup 
AirbnbNyc.copy <- AirbnbNyc
#-----------------------------
#to remove special characters from extra people
typeof(AirbnbNyc$extra_people)
head(AirbnbNyc$extra_people)
# remove % and replace with nothing
AirbnbNyc$extra_people <- gsub("$", "", AirbnbNyc$extra_people, fixed = TRUE)
head(AirbnbNyc$extra_people)
typeof(AirbnbNyc$extra_people)
# see the length of all N/A
length(grep("N/A", AirbnbNyc$extra_people))
#to view this column
#View(AirbnbNyc$extra_people)
#changing the data type to character
AirbnbNyc$extra_people <- as.numeric(AirbnbNyc$extra_people)
# there is a warning that NAs are introduced by coercion but there are no NAs in the column
length(grep("NA", AirbnbNyc$extra_people))
#backup without dummy variables
AirbnbNyc.copy <- AirbnbNyc

#-----------All special characters are removed from the dataset------
View(AirbnbNyc)
#-----------------------------
# to add dummy variables: 
#reference https://cran.r-project.org/web/packages/fastDummies/fastDummies.pdf

#table with dummy variables
AirbnbNyc_toUse <- fastDummies::dummy_cols(AirbnbNyc)
names(AirbnbNyc_toUse)
summary(AirbnbNyc_toUse)
View(AirbnbNyc_toUse)
View(AirbnbNyc)

#deleting the categorical columns

AirbnbNyc_toUse$host_is_superhost <- NULL
AirbnbNyc_toUse$room_type <- NULL
AirbnbNyc_toUse$instant_bookable <- NULL
AirbnbNyc_toUse$is_business_travel_ready <- NULL

names(AirbnbNyc_toUse)

#backup with dummy variables
AirbnbNyc_toUse.copy <- AirbnbNyc_toUse
#-----------------------------

plot(AirbnbNyc_toUse$price ~ AirbnbNyc_toUse$review_scores_rating)

#to analyze the different independent variables, omitting price column here
#Creating dataset without the dependent variable we are going to predict
AirbnbNyc_forCorrel <- AirbnbNyc_toUse[-c(8)]
View(AirbnbNyc_forCorrel)

library(corrplot)
AllCorrel1 <- cor(AirbnbNyc_forCorrel[1:30])

corrplot(AllCorrel1, type="upper", order="hclust", tl.col = "black", tl.srt = 45)

AirbnbNyc_AfterCorrel <- AirbnbNyc_toUse
ToOmitVal <- AllCorrel1
ToOmitVal <- NULL
#trying to find the variables with correlation coefficient greater than 0.65
x <- (1:30) 
y <- (1:30)
count <- 0
for (i in x)
for (j in y)
  {
  {
  if (AllCorrel1[i,j] >= 0.65 && i != j) 
  {
    count <- count +1
    ToOmitVal <- cbind(ToOmitVal, AllCorrel1[i,j]) 
    #a = colnames(AirbnbNyc_toUse[i]) 
  }
  }
}
count

#All the dependent variables that have correlation coefficient greater than 0.70 are omitted / deleted
#Will need to automate this for scalability later

AirbnbNyc_toUse$beds = NULL
AirbnbNyc_toUse$bedrooms = NULL
AirbnbNyc_toUse$review_scores_value = NULL

#backup of the final table to be used
AirbnbNyc_toUse.copy <- AirbnbNyc_toUse

names(AirbnbNyc_toUse)
str(AirbnbNyc_toUse)

#remove NA and N/A values from price column
colSums(is.na(AirbnbNyc_toUse))
copy = na.omit(AirbnbNyc_toUse)
AirbnbNyc_toUse <- copy

#simple linear regression
Airbnb.lm1 <- lm(price ~ host_response_rate + host_listings_count + accommodates + bathrooms + cleaning_fee
                + guests_included + extra_people + minimum_nights + maximum_nights + number_of_reviews 
                + review_scores_rating + review_scores_accuracy + review_scores_cleanliness 
                + review_scores_checkin + review_scores_communication + review_scores_location
                + reviews_per_month + host_is_superhost_f + host_is_superhost_t + room_type_Entire
                + room_type_Private + room_type_Shared + instant_bookable_f + instant_bookable_t
                +  is_business_travel_ready_f + is_business_travel_ready_t, data= AirbnbNyc_toUse)

#review model fit
summary(Airbnb.lm1)

confint(Airbnb.lm1) 

#review sum of squares break down
anova(Airbnb.lm1)

#interactions effects
install.packages("scatterplot3d")
library(scatterplot3d)

#basic 3d scatter plot of 3 variables
scatterplot3d(AirbnbNyc_toUse$accommodates, AirbnbNyc_toUse$review_scores_rating, AirbnbNyc_toUse$room_type_Entire)

#PREDICTION
Airbnb.lm1.pred <- predict(Airbnb.lm1, AirbnbNyc_toUse, type="response") 
summary(Airbnb.lm1.pred)

plot(Airbnb.lm1) 

#resampling!! Validation set approach: dividing 60% for training and 40% for testing

library(ISLR)
set.seed(1)
AirbnbTrainRows <- sample(rownames(AirbnbNyc_toUse), dim(AirbnbNyc_toUse)[1]*0.6)
AirbnbTrainData <- AirbnbNyc_toUse[AirbnbTrainRows,]

AirbnbValidRows <- setdiff(rownames(AirbnbNyc_toUse), AirbnbTrainRows)
AirbnbValidData <- AirbnbNyc_toUse[AirbnbValidRows,]

#simple linear regression with resampling 1
Airbnb.lm2 <- lm(price ~ host_response_rate + host_listings_count + accommodates + bathrooms + cleaning_fee
                 + guests_included + extra_people + minimum_nights + maximum_nights + number_of_reviews 
                 + review_scores_rating + review_scores_accuracy + review_scores_cleanliness 
                 + review_scores_checkin + review_scores_communication + review_scores_location
                 + reviews_per_month + host_is_superhost_f + host_is_superhost_t + room_type_Entire
                 + room_type_Private + room_type_Shared + instant_bookable_f + instant_bookable_t
                 +  is_business_travel_ready_f + is_business_travel_ready_t, data= AirbnbNyc_toUse, 
                 subset = AirbnbTrainRows)
#review model fit
summary(Airbnb.lm2)
plot(Airbnb.lm2)


#PREDICTION 2
Airbnb.lm2.pred <- predict(Airbnb.lm2, newdata = AirbnbValidData)
summary(Airbnb.lm2.pred) 

#table(Airbnb.lm2.pred) 
#to see the results
lm2.result <- data.frame(AirbnbValidData$price, residuals = AirbnbValidData$price - Airbnb.lm2.pred) 
head(lm2.result) 



#------------
#trying 10 fold cross validation from scratch - from week 5 class
set.seed(33)
AirbnbNyc_toUse_sampling <- AirbnbNyc_toUse # creating copy
#Creating 10 equally sized folds
folds <- cut(seq(1,nrow(AirbnbNyc_toUse_sampling)),breaks=10,labels=FALSE)
acc.cv <- rep(NA, 10)
tpr.cv <- rep(NA, 10)

#Randomize data a head of time
AirbnbNyc_toUse_sampling<-AirbnbNyc_toUse_sampling[sample(nrow(AirbnbNyc_toUse_sampling)),]

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- AirbnbNyc_toUse_sampling[testIndexes, ]
  trainData <- AirbnbNyc_toUse_sampling[-testIndexes, ]
  
  Airbnb.lm3 <- lm(price ~ host_response_rate + host_listings_count + accommodates + bathrooms + cleaning_fee
                   + guests_included + extra_people + minimum_nights + maximum_nights + number_of_reviews 
                   + review_scores_rating + review_scores_accuracy + review_scores_cleanliness 
                   + review_scores_checkin + review_scores_communication + review_scores_location
                   + reviews_per_month + host_is_superhost_f + host_is_superhost_t + room_type_Entire
                   + room_type_Private + room_type_Shared + instant_bookable_f + instant_bookable_t
                   +  is_business_travel_ready_f + is_business_travel_ready_t, 
                  data=trainData, family="binomial")
  
  #PREDICTION 3
  #predict on test set
  Airbnb.lm3.pred <- predict(Airbnb.lm3, testData, type="response")
}

summary(Airbnb.lm3.pred)

plot(Airbnb.lm3)

#***********************************************************

#using caret create a model of the dummy variables, with Full rank so N factor levels-1 new predictors
Airbnb.dmodel <- dummyVars( ~ ., data=AirbnbNyc_toUse, fullRank=T)

#apply the model to the credit data to create the new variables
#and credit new data frame with dummy codes instead of factors
Airbnb.d <- as.data.frame(predict(Airbnb.dmodel, AirbnbNyc_toUse))

set.seed(195)
Airbnb.train.index <- sample(nrow(Airbnb.d), nrow(Airbnb.d) * .7) #let keep 70% of data for training

#create test and traing data frames
bnb.train <- Airbnb.d[Airbnb.train.index,] #model with this
bnb.test <- Airbnb.d[-Airbnb.train.index,] #we don't touch this while training

ctrl <- trainControl(method = "cv", number=10)
set.seed(33) 

Airbnb.lm4 <- train(price ~ ., data= bnb.train, method = "lm", trControl=ctrl)
Airbnb.lm4
varImp(Airbnb.lm4) #to see which variables are important


set.seed(33) 

Airbnb.lm5 <- train(price ~ ., data= bnb.train, method = "lasso", trControl=ctrl)
Airbnb.lm5
plot(Airbnb.lm5)
#ggplot(Airbnb.lm5$finalModel)
varImp(Airbnb.lm5)
plot(varImp(Airbnb.lm5))

#coef(Airbnb.lm5)
summary(Airbnb.lm5)

plot(Airbnb.lm5$finalModel, pch = 4, col = "red")


library(glmnet)
grid = 10^seq(10, -2, length = 100)
x = model.matrix(price ~ . - 1, AirbnbNyc_toUse)
lasso_mod <-  glmnet(x, AirbnbNyc_toUse$price, data=bnb.train, alpha = 1, 
                 lambda = grid)



summary(lasso_mod)
plot(lasso_mod)
lasso_mod$lambda.min

## K NEAREST NEIGHBOR
#lets fit our first linear regression model

#set values of k to search through, K 1 to 20
Airbnb.k.grid <- expand.grid(k=1:20)

set.seed(195) #ALWAYS USE same SEED ACROSS trains to ensure identical cv folds

Airbnb.lm6 <- train(price ~ ., data= bnb.train, method = "knn", 
               tuneGrid=Airbnb.k.grid, trControl=ctrl) 
Airbnb.lm6
plot(Airbnb.lm6)
ggplot(Airbnb.lm6)
varImp(Airbnb.lm6)

#we can plot parameter performance
plot(Airbnb.lm6)


#preprocess K means performs better when standardized!
set.seed(195)
Airbnb.lm7 <- train(price ~ ., data= bnb.train, method = "knn", 
                  preProcess=c("center", "scale"),
                  tuneGrid= Airbnb.k.grid, trControl=ctrl)
Airbnb.lm7
plot(Airbnb.lm7)
varImp(Airbnb.lm7)



#Comparison!!
#To compare all of the model resampling performance
#first lets put all trained models in a list object
Airbnb.models<- list("LM 70-30 validation"= Airbnb.lm4, "Lasso" = Airbnb.lm5, "KNN Base"=Airbnb.lm6,
                     "KNN PreProcessed" = Airbnb.lm7) 


Airbnb.resamples<- resamples(Airbnb.models)
summary(Airbnb.resamples)

#plot performances
bwplot(Airbnb.resamples)
bwplot(Airbnb.resamples, metric="RMSE", col="#69b3a1")
bwplot(Airbnb.resamples, metric="RMSE")
bwplot(Airbnb.resamples, metric="Rsquared")

#evalaute differences in model and see if their are statistical sig differnces
Airbnb.model.diff <- diff(Airbnb.resamples)
summary(Airbnb.model.diff) #lower diagonal is p-value of differences

#All Models summary
summary(Airbnb.lm1.pred) #simple linear regression without sampling 
summary(Airbnb.lm2.pred) #lm with sampling 60-40
summary(Airbnb.lm3.pred) #10 fold cross-validation

Airbnb.lm5.pred <- predict(Airbnb.lm5, newdata = bnb.test )
summary(Airbnb.lm5.pred)



Airbnb.Pred.Summary <- list("Simple Linear Regression" = summary(Airbnb.lm1.pred),
                         "Linear Regression with 60-40 Sampling" =  summary(Airbnb.lm2.pred),
                         "Linear Regression with k-fold smapling" = summary(Airbnb.lm3.pred),
                         "Lasso Regression" = summary(Airbnb.lm5.pred)
                        )

Airbnb.Pred.Summary

