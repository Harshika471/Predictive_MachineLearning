#Reading the CSV file
install.packages("rmarkdown")
library(rmarkdown)
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("Amelia")
install.packages("GGally")
install.packages("DataExplorer")
library(DataExplorer)
library(GGally)
library(Amelia)
library(ggplot2)
library(tidyverse)
getwd()
# Install package readxl to read excel files
install.packages("readxl")
library(readxl)
#Import the data
file1 = read_excel("Credit_Risk6_final.xlsx",sheet = "Scoring_Data")
file2 = read_excel("Credit_Risk6_final.xlsx",sheet = "Training_Data")
#Perform EDA
head(file2)
# know the datatypes of the attributes
str(file2)
file2$`Credit Standing` <- as.factor(file2$`Credit Standing`)
#check for null values in the dataset
missmap(file2)
#Method2 : To find the missing values seperately in the seperate column
summary(file2)
sapply(file2,function(x) sum(is.na(x)))
new_df <- file2
View(new_df)
sapply(new_df,function(x) sum(is.na(x)))
#Replacing Employment Missing values with Mode
val <- unique(new_df$Employment[!is.na(new_df$Employment)])
mode <- val[which.max(tabulate(match(new_df$Employment,val)))]
new_df$Employment[is.na(new_df$Employment)]<-mode
colSums(is.na(new_df))
sapply(new_df,function(x) sum(is.na(x)))

#Replacing Personal status value with mode
val1 <- unique(new_df$`Personal Status`[!is.na(new_df$`Personal Status`)])
mode2 <- val1[which.max(tabulate(match(new_df$`Personal Status`,val1)))]
new_df$`Personal Status`[is.na(new_df$`Personal Status`)] <- mode2

#Replacing Housing values with mode
val2 <- unique(new_df$Housing[!is.na(new_df$Housing)])
mode3 <- val2[which.max(tabulate(match(new_df$Housing,val2)))]
new_df$Housing[is.na(new_df$Housing)] <- mode3

#Exploratory data analysis
psych::describe(new_df)
plot_str(new_df)
plot_correlation(new_df, type = 'continuous')
#Observation: Continous variables are not co-related.
# Working with the categorical data
tbl_cnt <- table(new_df$`Credit History`,new_df$`Savings Acct`)
tbl_cnt
ggplot(new_df,aes(x=new_df$`Credit History`, fill = new_df$`Savings Acct`))+geom_bar(position = "fill")+ylab("proportion")
prop.table(tbl_cnt)

# Type of loan reasons?
unique(new_df$`Loan Reason`)
table(new_df$`Loan Reason`)
type_counts <- table(new_df$`Loan Reason`)
type_counts/sum(type_counts)
# so about 2/3 of our observations are associate with New car.
unique(new_df$`Checking Acct`)
# Bivariate Analysis
ggplot(new_df, aes(x = new_df$`Checking Acct`))  +
  geom_histogram(alpha = 0.8, binwidth = 5,stat = "count") + 
  xlab("account balance") +
  facet_wrap(~ new_df$`Loan Reason`, ncol = 4)
new_df$ID <- NULL
ggplot(new_df,aes(x=new_df$`Loan Reason`,y=new_df$`Savings Acct`,color=new_df$`Checking Acct`))+geom_point()
new_df$`Credit History` <- as.factor(new_df$`Credit History`)
new_df$`Credit Standing` <- as.factor(new_df$`Credit Standing`)
new_df$Employment <- as.factor(new_df$Employment)
new_df$`Credit History` <- as.factor(new_df$`Credit History`)
new_df$`Loan Reason` <- as.factor(new_df$`Loan Reason`)
#Trivariate Analysis Credit History, Age and Credit standing
ggplot(new_df,aes(x=new_df$`Credit History`,y=new_df$Age))+geom_boxplot(aes(fill=new_df$`Credit Standing`))
#Trivariate analysis Months since account open, saving account and credit standing
ggplot(new_df,aes(x=new_df$`Savings Acct`,y=new_df$`Months since Checking Acct opened`))+geom_boxplot(aes(fill=new_df$`Credit Standing`))
#Trivariate analysis Months since account open, saving account and credit standing
ggplot(new_df,aes(x=new_df$`Credit History`,y=new_df$`Months since Checking Acct opened`))+geom_boxplot(aes(fill=new_df$`Credit Standing`))

#Multivariate Analysis
# plotting between loan reasons, check_acct, saving_acct and credit_history
ggplot(new_df,aes(x=new_df$`Loan Reason`,y=new_df$`Savings Acct`,color=new_df$`Checking Acct`,size=new_df$`Credit History`))+geom_point(alpha =.6)

---------QUES2-------------------
#Descision Tree
#Remove the ID Column from the sheet
new_df$ID <- NULL
is.na(new_df$`Credit Standing`)
table(new_df$`Credit Standing`)
# P(Good) = 461/780 == 0.59
#P(Bad) = 319/780 == 0.40
# Therefore the Entropy of parent node is 0.148
#B. Plot the Decision tree using tree package and describe its parameters
install.packages("tree")
install.packages("dplyr")
library(dplyr)
install.packages("rpart")
install.packages("rpart.plot")
library(rpart.plot)
library(rpart)
library(tree)
require(tree)
#create a decision tree model
# set. seed ?
set.seed(183)
tree <- rpart(new_df$`Credit Standing`~. ,data = new_df, method = 'class')
tree
#Describe the each and every parameter ?????????

# Visualize the decision tree with rpart.plot
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE,cex = 0.5)
#cex I used to zoom out the text inside the decision tree
#c.Make Prediction for the Scoring set
library(dplyr)
install.packages("rpart.plot")
library(rpart.plot)
partition_train_test(new_df, size = 0.8, train = TRUE)
#Arguments:
# df: Data frame to be used
#size : size of the split
#train: if test set to true funtion create the train set else create the test set. Lets try to run and see the output:
partition_train_test <- function(newdf,size=0.8,train=TRUE){
  count_row = nrow(new_df)
  total_row = size*count_row
  train_sample <- 1:total_row
  if(train == TRUE){
    return(new_df[train_sample,])
  }else{
    return(new_df[-train_sample,])
  }
}
# Create a function and add the function arguments
# First count the total rows in the data set and then multiply the total rows with the split ratio( for us it is 0.8 and total rows are 780 hence
#780*0.8=624(Train data), Remaning values now will be 780-624 = 156 == Test data)
#In the next step after partitioning the dataset will take the first row of the nth rows, if train== true will return training dataset else return false
train_data <- partition_train_test(new_df,0.8,train = TRUE)
test_data1 <- partition_train_test(new_df,0.8,train = FALSE)
View(train_data)
View(test_data1)
#Percentage of the Credit standing parameters
prop.table(table(train_data$`Credit Standing`))
prop.table(table(test_data1$`Credit Standing`))

# Create a model using rpart R package
library(rpart)
library(rpart.plot)
library(dplyr)
fit <- rpart(`Credit Standing`~.,data = new_df,method = 'class')
rpart.plot(fit, box.palette="RdBu", shadow.col="darkgray", nn=TRUE,cex = 0.5)

#Now after plotting the tree the next step to make prediction on the unseen dataset (for me it is Test data)
predict_test <- predict(fit,test_data1,type='class')
predict_test
# Check the output how many customers are having the Good credit standing rate and how many have bad
check <- table(test_data1$`Credit Standing`,predict_test)
check
# As per the Confusion Matrix 
#predict_test
#Bad Good
#Bad   39   34
#Good   9   74 ( There are 74 customers predicted having the good credit history)

# Accuracy Test ( Which is the sum of the true postives and true negatives over sum of matix)
test_accuracy <- sum(diag(check))/sum(check)
test_accuracy
# We have got the accuracy of around 72%.
# Test the accuracy on the scoring dataset in the similar way
# Copy file1 data into another variable
score_data <- file1
#Clean the data by Removing the ID Column
score_data$ID <- NULL
#Rename the column name (Residence time(in current disctrict))
#scoring_data <- rename(scoring_data,ResidenceTime (In current district)=Residence Time)
#rename(score_data,replace = c("Residence Time (In current district)"="Residence Time"))
library(dplyr)
score_data = score_data %>%
  rename(`Residence Time (In current district)` = `Residence Time` ) 
names(score_data)
names(train_data)
View(score_data)
predict_score <- predict(fit,score_data, type='class')
predict_score
# Tune the parameters
tune_parameters <- function(tune){
  predict_test <- predict(fit,test_data1,type='class')
  check <- table(test_data1$`Credit Standing`,predict_test)
  test_accuracy <- sum(diag(check))/sum(check)
  test_accuracy
}
control <- rpart.control(minsplit = 4,minbucket = round(5/3),maxdepth = 3,cp=0)
final_tune <- rpart(`Credit Standing`~.,data = train_data,method = 'class',control = control)
tune_parameters(final_tune)
# Observation: No difference in the Accuracy
# Random Forest using the bagging technique
install.packages("caret",dependencies = TRUE)
install.packages("randomForest")
library(dplyr)
library(randomForest)
library(caret)
# How to decide how many Random forest tree I can build ? ---- Square root of number of predictors we have(In our case we have 12 so 
#sqrt of 12 will be close to 3.46)
# In the Below Lines of code I am changing the name for residence time for train and test data using the dplyr library
names(train_data) <- gsub('\\s+', '_', names(train_data))
train_data %>% 
  rename(
    Residence_Time =  `Residence_Time_(In_current_district)`
  )
names(train_data)[12] <- "Age" 
names(train_data)
names(test_data1) <- gsub('\\s+', '_', names(test_data1))
test_data1 %>% 
  rename(
    Residence_Time =  `Residence_Time_(In_current_district)`
  )
names(test_data1)[12] <- "Age" 
names(test_data1)
set.seed(183)
is.na(train_data)
library(dplyr)
# ID to be NUll as No Use
train_data$ID <- NULL
test_data1$ID <- NULL
train_data=train_data %>% mutate_if(is.character, as.factor)
str(train_data)
train_data=train_data %>% 
  rename(
    Residence_Time =  `Residence_Time_(In_current_district)`
  )
names(train_data)
test_data1=test_data1 %>% mutate_if(is.character, as.factor)
str(test_data1)
test_data1=test_data1 %>% 
  rename(
    Residence_Time =  `Residence_Time_(In_current_district)`
  )
names(test_data1)
View(train_data)
#Creating the Random forest tree
rf_tree <- randomForest(train_data$Credit_Standing~., data = train_data, mtry= 8, importance=TRUE, ntree = 500
                        ,na.action = na.exclude)
                    
rf_tree
# Here we got 24.09% of OOB which means nearly 76% of Accuracy.
attributes(rf_tree)
rf_tree$confusion
plot(rf_tree)
importance(rf_tree)
#varImpPlot(rf_tree,scale = TRUE)

#----------Prediction and confusion matrix with Train data--------------
library(caret)
p1 <- predict(rf_tree,data=train_data)
head(p1)
# Now see the initial values of train data and compare it with predicted data
head(train_data$Credit_Standing)
#Observation: As per the data below, only three values(Predicted and actual) are matching.
confusionMatrix(p1,train_data$Credit_Standing)
# as per the CM we are geetinh 76% of Accuracy.

#-----------------Prediction and confusion matrix with Test data---------
library(dplyr)
test_data1=test_data1 %>% mutate_if(is.character, as.factor)
p2 <- predict(rf_tree, test_data1)
confusionMatrix(p2, test_data1$Credit_Standing)
plot(rf_tree)
str(test_data1)

# Tune mtry
# I made a use of this line as my Tunrf function was throwing error
#(Error in randomForest.default(x, y, mtry = mtryStart, ntree = ntreeTry,  : 
#length of response must be the same as predictor)
t <- tuneRF(train_data[,-13],train_data[,13],stepFactor = 0.5, plot = TRUE, ntreeTry = 300, trace = TRUE, improve = 0.05)
#Observation: As we can see the t value and plot. the OOB rate decreases as the mtry value increases so we will go back to the
#rf_tree and increase the mtry value from 3 to 6 and see the accuracy ?
#Observation : S0 as the tree grows the number of errors initially increases and then it becomes constant.
# AS per the VarImpPlot higher the mean decrease accuracy higher the importance of the variable in the model. as per my the plot
# we can see clearly Credit history has the higher mean decrease accuracy which indicates the Most important variable.
#Mean Decrease accuracy : Decides on, how much the model accuracy decreases if we drop that variable.
#------------------No. of Nodes for the Tress----------------
hist(treesize(rf_tree),main = 'No. of nodes for the tress',col = "green")
#Observation : The Histogram clearly states that there are 100 trees which contains 120 nodes. However, there are 
#few tress which has less than 80 nodes. Hence Majority of trees have close to 100 Nodes.
#------------ Extract Single Tree--------------
getTree(rf_tree, labelVar = TRUE)
#-------------Multi Dimensional Scaling Plot
MDSplot(rf_tree,train_data$Credit_Standing)
# which shows Credit standing " Good"( Blue Color) has more data then " Bad"
#( Refer DS Notes one note from DS Notes)
 
#------------- BOOSTING FOR CLASSIFICATION - GBM------------------
install.packages("gbm")
library(gbm)
library(caret)
library(dplyr)
# Referred from the Lab Code
#Pick a model name gbm and see what type of model it is
getModelInfo()$gbm$type
new_df$`Credit Standing` <- as.numeric(new_df$`Credit Standing`)
str(new_df)
new_df$`Credit Standing` <- new_df$`Credit Standing`+1
View(new_df)

#[1] "Regression"     "Classification" We will Pick Classification type here



set.seed(183)
train_data <- data.frame(train_data)
#Gaussian: Use to make tha data Normally distributed when dividing into multiple folds
boost.credit <- gbm(`Credit_Standing`~.-train_data$Credit_Standing,data = train_data,distribution = "gaussian",n.trees = 5000,shrinkage=0.01,interaction.depth = 4)
summary(boost.credit)
str(test_data1)
str(train_data)
test_data1$Credit_Standing <- as.factor(test_data1$Credit_Standing)
train_data$Credit_Standing <- as.factor(train_data$Credit_Standing)
prediction_credit <- predict(boost.credit,newdata = test_data1,n.trees = 5000,type = "link")
prediction_credit
summary(prediction_credit)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.8755  1.3836  1.6060  1.5359  1.7094  1.9849
#Here predict the Good and bad using the mean and median.(threshold values)
predict_class <- ifelse(prediction_credit<1.56,"Bad","Good")
table(predict_class,test_data1$Credit_Standing)
hist(prediction_credit)


#E. ------------------- Anomilies Detection----------------------
library(rpart)
View(new_df)
#copying the New_df data to the another data frame name temp_data
temp_data <- new_df
consecutive_data <- rpart(`Credit Standing`~.,data = temp_data,method = 'class')
#creating a new column in temp data in order to compare the actual and predicted values
temp_data$Prediction <- predict(consecutive_data, data=temp_data,type='class')
View(temp_data)
#Check the accuracy using the confusion matrix
confusionMatrix(consecutive_data,temp_data$Prediction)
#Using loop to check if the which values of IDs are equal or not? If Not fetch the IDs
for (i in 1:length(temp_data$ID)){
  if (temp_data$`Credit Standing`[i] != temp_data$Prediction[i]){
    print(temp_data$ID[i])
  }
}
data_table <- table(temp_data$`Credit Standing`,temp_data$Prediction)
data_table

#F.-----------------Infogain algo to calculate the variable for first split------------
#Create a 1-D table for Credit standing w.r.t Train data
#Reffered the Code Used in the LAB
table(train_data$Credit_Standing)
credit <- prop.table(table(train_data$Credit_Standing))
# Create a function in order to calculate the Entropy of each variable
tabfun <- function(x){
  table(train_data[,x],train_data[,13])
}
print(tabfun(3))
head(train_data)

# Again, write a function to calculate the proportion of Each variable in the given column with respect to the Total no.
tabfun <- function(x){
  prop.table(table(train_data[,x],train_data[,13])+1e-6,margin = 2)
}
print(tabfun(3))
# Now its time to find the ENTROPY (Prop of each variable*log2 prop)
-tabfun(1)*log2(tabfun(1))
x = seq(0,1, by = 0.005)
plot(x, -x*log(x,2))
plot(x, -(x*log(x,2)+(1-x)*log(1-x,2)))

# Find the row sum
rowSums(-tabfun(3)*log2(tabfun(3)))
# need to multiply by prop of each rows
prop.table(table(train_data$Credit_Standing))
#sum(prop.table(table(train_data$Credit_Standing))*rowSums(-tabfun(1)*log2(tabfun(1))))
entopy_tab <- function(x) { tabfun <- prop.table(table(train_data[,x],train_data[,13]), margin = 1)
sum(prop.table(table(train_data[,x]))*rowSums(-tabfun(x)*log2(tabfun(x))))}
entopy_tab(1)
credit <- prop.table(table(train_data$Credit_Standing))
(entropy_total <-sum(-credit*log2(credit)))

#Hence the info gain for the check_account is = 1-entropy_total
#info_gain = 1-0.967 = 0.033

# Again, write a function to calculate the proportion of Each variable in the given column with respect to the Total no.

# For Credit hIstory the Entropy value = 0.78
r <- c(1,2,3,4,5,6,7,8,9,10,11,12)

r3 <- NULL
r4 <- NULL
for (x in r) { r2 <- entopy_tab(x)
              r3 <- c(r3,r2)
              r4 <- c(entropy_total-r3)
print(r3)
print(r4)
}


#7.--------- Adabag for boosting------------------
ID <- c(1,2,3,4,5,6,7,8,9,10)
Label <- c(0,1,1,0,1,1,0,1,0,0)
Weight <- c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
prediction <- c(1,0,0,0,1,1,0,1,0,0)#c(1,1,1,1,0,1,0,1,0,0)
#error <-c(1,1,1,0,0,0,0,0,0,0)# c(1,0,0,1,1,0,0,0,0,0)
ada_boost <- data.frame(ID,Label,Weight,prediction)
ada_boost
adaboost_classifier <- function(ada_boost){
    ada_boost$Error <- ifelse(ada_boost$prediction==ada_boost$Label,0,1)
    ada_boost$WeightError <- c(ada_boost$Weight*ada_boost$Error)
    #ada_boost
    sumweight <- sum(ada_boost$WeightError)
    #calculating the value of alpha with the given formula in the excel sheet.
    alpha <- 0.5*log((1-sumweight)/sumweight)
    #calculate the Incorrect value with the Given formula in the Excel.(=EXP(-G16*-1))
    incorrect <- exp(-alpha*-1)
    #In the similar fashion calculate the Correct value using the formula in the excel(G19 )
    correct <- exp(-alpha*1)
    #comaring the prediction and lable of adaboost
    ada_boost$adj <- ifelse(ada_boost$prediction==ada_boost$Label,correct,incorrect)
    ada_boost$adjweight <- ada_boost$adj*ada_boost$Weight
    sumadj <- sum(ada_boost$adjweight)
    for(i in range(1,12)){
         i <- ada_boost$adjweight
          #print(i)
      }
    ada_boost$newweight <- ada_boost$adjweight/sumadj
    return(ada_boost)
}

######------------------Iteration 1-----------######
ada_boost_T1<-adaboost_classifier(ada_boost)
Error_T1<-ada_boost_T1$Error
ada_boost_T1$Weight<-ada_boost_T1$newweight
ada_boost_T1$prediction<-c(0,1,1,1,0,0,0,1,0,0)#c(1,0,0,0,1,1,0,1,0,0)
#ada_boost_T1$error<-c(0,0,0,1,1,1,0,0,0,0)#c(1,1,1,0,0,0,0,0,0,0)
confusionMatrix(table(ada_boost_T1$prediction,ada_boost_T1$Label))

######------------------Iteration 2-----------######
ada_boost_T2<-adaboost_classifier(ada_boost_T1)
Error_T2<-ada_boost_T2$Error
ada_boost_T2$Weight<-ada_boost_T1$newweight
ada_boost_T2$prediction<-c(1,1,1,1,0,1,0,1,0,0)
confusionMatrix(table(ada_boost_T2$prediction,ada_boost_T2$Label))
######------------------Iteration 3-----------######
ada_boost_T3<-adaboost_classifier(ada_boost_T2)
#Sum of errors

Error_T3<-ada_boost_T3$Error
sum_Error<- Error_T1+Error_T2+Error_T3
print(sum_Error)
confusionMatrix(table(ada_boost_T3$prediction,ada_boost_T3$Label))

# Adding a new column for Cumulative sum of new weights
ada_boost$cum_sum <- cumsum(ada_boost$newweight)
#Generate Random Number
#my_bucket <- function(my_num,ada_boost){
    #for( j in 1:10){
       #if(my_num<ada_boost$cum_sum[j] && my_num>0){
        #return(ada_boost[j,])
      
     #}
#     }
# }
# new_df <- 0
# my_num <- runif(10,0.0,1.0)
# print(my_num)

for (i in 1:length(my_num)){
new_df <- rbind(new_df,my_bucket(my_num[i],ada_boost))
}
new_df<-new_df[-1,]
print(new_df)
##############8. ROC Curve   ###############################
#Using the Random forest model train_data plotting the ROC Curve
library(rpart)
library(rpart.plot)
#Anova : Analysis of Variances which helps to test the differnces between the Means of two Groups.
fit <- rpart(`Credit_Standing`~.,data = train_data,method = 'anova')
pred_y <- predict(fit,train_data)
#tpr: True Positive rate
#fpr: False positive rate
tpr = character()
fpr = character()
for (i in seq(1,2,.001)){
  tpr <- c(tpr, sum( pred_y>= i & train_data$Credit_Standing =="Good") / length(train_data$Credit_Standing =="Good"))
  fpr <- c(fpr, sum( pred_y>= i & train_data$Credit_Standing =="Bad") / length(train_data$Credit_Standing =="Bad"))
}
train_data$`Credit_Standing`
#par(mfrow = c(1,2))

plot(fpr, tpr)


