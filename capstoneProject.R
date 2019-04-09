setwd("E:\\Jigshaw lectures\\R STUDIO CLASS\\machine learning\\capstone")
getwd()
data<-read.csv("sampletelecomfinal.csv",header = TRUE)
str(data)
dim(data)

#****************CHECKQUALITYDATA*****************
library(dataQualityR)
checkDataQuality(data,out.file.cat = "Categorical.csv",out.file.num = "numeric.csv")
#in categorical
data$numbcars<-NULL
dim(data)
#in continuous
data$solflag<-NULL
data$wrkwoman<-NULL
data$div_type<-NULL
data$occu1<-NULL
data$proptype<-NULL
data$cartype<-NULL
data$children<-NULL
data$mailordr<-NULL
data$mailresp<-NULL
data$dwlltype<-NULL
data$dwllsize<-NULL

#seperation of main dataset
str(data)
num<-data[,c(1:30,39:54,57:67)]
cat<-data[,c(31:38,55,56)]
colnames(cat)
colnames(num)

#do it in csv file
write.csv(num,file = "Numericdata.csv")
write.csv(cat,file = "Categoricdata.csv")

#imputation by using MICE
install.packages("mice")
library(mice)
install.packages("randomForest")
library(randomForest)
micemod<-mice(num[,!names(num)%in% "medv"],method = "rf")
miceOutput<- complete(micemod)
miceOutput<-read.csv("miceOutputnew.csv")

#imputation of categorical variable
library(randomForest)
micemod_1<-mice(cat[,!names(cat)%in% "medv"],method = "rf")
miceOutput_1<-complete(micemod_1)
miceOutput_1<-read.csv("miceOutput_1new.csv",header = T)

#it will take 2 hours to use mice.so its better to create csv file of it.
write.csv(miceOutput,"miceOutputnew.csv")
write.csv(miceOutput_1,"miceOutput_1new.csv")

#***************PCA NOT WORKING****************
#corelation between independent  #PCA 
#cor(miceOutput)
#dim(miceOutput)
#install.packages("PCA")
#model<-prcomp(miceOutput,scale. = FALSE,center = F)
#summary(model)

new=cbind(miceOutput,miceOutput_1)
dim(new)

#*********Data Preparation*********#
#-----Continuous Variables------#
names(new)
summary(num)
str(new)


# Convert to Factor and Create Dummy Variables
#age1, age2

#age1
str(new$age1)
new$age1_1<-ifelse(new$age1==0,"Default",ifelse(new$age1<=30,"Young",
                                                    ifelse(new$age1>30 & new$age1<=55,"Mid Age","Old")))
str(new$age1_1)
new$age1_1<-as.factor(new$age1_1)
summary(new$age1_1)

##age_1 remove
names(new)
new<-new[,-31]   
summary(new)


#age2
str(new$age2)
new$age2_1<-ifelse(new$age2==0,"Default",ifelse(new$age2<=30,"Young",
                                                    ifelse(new$age2>30 & new$age2<=55,"Mid Age","Old")))
str(new$age2_1)
new$age2_1<-as.factor(new$age2_1)
summary(new$age2_1)

#age2 remove
names(new)
new<-new[,-31]
dim(new)





### ********** Logistic Regression Model Building ********** ###

# Partition the dataset into training and validation dataset  #MAX NO. OF ROWS THEN DIVIDE (TRAIN & TEST)

sampling<-sort(sample(nrow(new), nrow(new)*.7))
length(sampling)

#Row subset and create training and validation samples using the index numbers

train<-new[sampling,]
test<-new[-sampling,]
nrow(train)
nrow(test)

#Checking Churn Rate 
table(train$churn)/nrow(train)
table(test$churn)/nrow(test)

#collinearity
library(caret)
dim(new)
after_training_num<-train[,c(1:55)]
after_training_cat<-train[,c(56:67)]
corr<-cor(after_training_num)    #corelation just for numeric #for train
findCorrelation(corr,cutoff=.7)
after_training_eliminate<-after_training_num[,-c(25,29,27,1,30,26,28,23,54,21,51,11,19 ,15 ,52, 50, 22 , 3, 45 ,10, 48, 34)]
dim(after_training_eliminate)
final_train_file<-cbind(after_training_eliminate,after_training_cat)
dim(final_train_file)

final_train_file=final_train_file[,-33]   #remove customer id
dim(final_train_file)
colnames(final_train_file)




# Building Logistic Regression Model 
#mod<-glm(churn~.,data=final_train_file,family="binomial")
#summary(mod)


final_train_file$csa<-NULL
x1=randomForest(churn~.,data =final_train_file,importance = TRUE)
b2 = importance(x1,type = 1)
class(b2)
b3 = as.data.frame(b2)
colnames(final_train_file)

b4=varImpPlot(x1,sort = T,n.var = 20)
write.csv(b4,"b4.csv")

result_my<-glm(data = final_train_file,churn ~ totmrc_Mean+mou_Range+change_mou+drop_blk_Mean	+drop_vce_Range+	
                 owylis_vce_Range +mou_opkv_Range	+income+	eqpdays	+custcare_Mean+	iwylis_vce_Mean+callwait_Range+
                 ccrndmou_Range	+ovrrev_Mean+	plcd_vce_Mean+	models+	hnd_price	+actvsubs+	forgntvl	+opk_dat_Mean	
               +mtrcycle+	retdays	+truck	+roam_Mean+	recv_sms_Mean+	blck_dat_Mean+	mou_pead_Mean+	da_Range+	datovr_Mean	
               +drop_dat_Mean+	adjrev+	crclscod+	asl_flag+	prizm_social_one+	area+	refurb_new+	hnd_webcap+	marital
               +car_buy	+age1_1	+age2_1 , family= binomial)
summary(result_my)


#step function
reduced<-step(result_my,direction="both")
result_my_1<-glm(data = final_train_file,churn ~ totmrc_Mean + change_mou + drop_blk_Mean + owylis_vce_Range + 
                   eqpdays + ovrrev_Mean + plcd_vce_Mean + hnd_price + actvsubs + 
                   retdays + mou_pead_Mean + da_Range + drop_dat_Mean + asl_flag + 
                     age1_1,family = binomial)

summary(result_my_1)




#iteration 2
result_my_2<-glm(data = final_train_file, churn ~ totmrc_Mean + drop_blk_Mean + owylis_vce_Range + 
                   eqpdays + ovrrev_Mean + plcd_vce_Mean + hnd_price + actvsubs + 
                   retdays + mou_pead_Mean  + asl_flag + 
                   age1_1,family = binomial)
summary(result_my_2)

final_train_file$predicted = result_my$fitted.values
View(final_train_file)

library(ROCR)
pred<-prediction(final_train_file$predicted,final_train_file$churn)
pred

perf<-performance(pred,"acc")
class(perf)
perf


cutoff<-as.numeric(unlist(perf@x.values))
accuracies<-as.numeric(unlist(perf@y.values))


cutoffvsaccu<-data.frame(cutoff,accuracies)
View(cutoffvsaccu)
cutoffvsaccu_1<-cutoffvsaccu[order(cutoffvsaccu$accuracies, decreasing=TRUE),]


final_train_file$predclass = ifelse(final_train_file$predicted>0.5542450,1,0)

table(final_train_file$predclass,final_train_file$churn)
(7096+11)/(7096+2168+17)
library(irr)
#install.packages("e1071")
library(e1071)
confusionMatrix(as.factor(final_train_file$churn),as.factor(final_train_file$predclass), positive = "1")


kappa2(data.frame(final_train_file$churn,final_train_file$predclass))  #kappa=0.004 

#******************now FOR TEST**********************

test$pred=predict(result_my_1,type="response",newdata=test)

library(ROCR)
pred<-prediction(test$pred,test$churn)
pred

perf<-performance(pred,"acc")
class(perf)
perf


cutoff<-as.numeric(unlist(perf@x.values))
accuracies<-as.numeric(unlist(perf@y.values))


cutoffvsaccu<-data.frame(cutoff,accuracies)
View(cutoffvsaccu)
cutoffvsaccu_1<-cutoffvsaccu[order(cutoffvsaccu$accuracies, decreasing=TRUE),]


test$predclass = ifelse(test$pred>0.3937222,1,0)

table(final_train_file$predclass,final_train_file$churn)
(7096+11)/(7096+2168+17)
library(irr)
#install.packages("e1071")
library(e1071)
confusionMatrix(as.factor(test$churn),as.factor(test$predclass), positive = "1")


#install.packages("e1071")
library(e1071)
## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)

perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(perf,col="red")
# Receiver Operating Characteristic Curve (ROC) a plot of TPR versus FPR 
# for the possible cut-off classification probability values.
# A good ROC curve should be almost vertical in the beginning and 
# almost horizontal in the end.
# "tpr" and "fpr" are arguments of the "performance" function 
# indicating that the plot is between the true positive rate and 
# the false positive rate.

?abline
# Draw a straight line with intercept 0 and slope = 1
# lty is the line type (dotted or dashed etc.)
# The straight line is a random chance line
# ROC curve should be higher than the AB line
abline(0,1, lty = 8, col = "blue")

# Area under the curve should be more than 50%
auc<-performance(pred,"auc")
auc


#QUESTIONS

#1.	What are the top five factors driving likelihood of churn at Mobicom?

summary(result_my_1)


#install.packages("lm.beta")
#library(lm.beta)
#?lm.beta
#lm.beta(result_my_1)

# The above library is not available to me on LMS so I am manually looking at the beta coefficients 
#head(sort(abs(result_my_1$coefficients),decreasing = T),10)
#summary(result_my_1)

## The model results show that the top 5 factors affecting churn are:
           
#1.  age1_1Old       -0.32283675
#2. asl_flagY        -0.29424352
#3. age1_1Mid Age    -0.20845280
#4. actvsubs          0.11028380
#5. drop_dat_Mean    -0.08058917 


#The 1st factor explains, with a unit increase in  of age1_old variable , there is 0.32283675
#unit decrease in churn.
# The 2nd Factor explains, with a unit increase in age1_1Mid Age variable , there is 0.29424352 unit decrease
#in churn.
# Same explaination applies to the next 3 variables
plot(final_train_file$age1_1,final_train_file$churn,col= "red")
final_train_file$age1_1
#2.	Validation of survey findings. a) Whether "cost and billing" and "network and service 
#quality" are important factors influencing churn behaviour.  b) Are data usage connectivity issues
#turning out to be costly? In other words, is it leading to churn?

#cost and billing 
#**** Variables 1. totmrc_Mean 'base plan charge' representing cost to customer, 

#*** #var 2. ovrrev_Mean = DATOVR_MEAN + VCEOVR_MEAN i.e. 'Mean overage revenue' (It is the sum of data and voice 
#*** overage revenues) representing the overage revenue earned from customers after billing the same to them.   


summary(result_my_1)
# var totmrc_Mean(customer) has beta coefficient value of -0.00236062 meaning a unit increase in this variable is causing 
# decrease in churn by 0.00236062/unit.
## var ovrrev_Mean (company) has beta coefficient value of 0.00347397 meaning a unit increase in this variable is causing 
# increase in churn by 0.00347397/unit
      
#SO it seems cost and billing is not  very important factors here influencing churn behaviour at Mobicom.


#network and service quality

#owylis_vce_Range   0.00337706
#plcd_vce_Mean     -0.00117151 
#drop_blk_Mean     0.00681792

# var owylis_vce_Range has beta coefficient value of 0.00337706 meaning a unit increase in this variable is causing 
# increse in churn by 0.00337706/unit.Thts very small

#var plcd_vce_Mean has beta coefficient value of -0.00117151 meaning a unit increase in this variable is causing 
# decrease in churn 0.00117151 /unit

#var drop_blk_Mean has beta coefficient value of 0.00681792 meaning a unit increase in this variable is causing 
# increse in churn 0.00681792 by /unit
 #Again these paramter for network and service are not act as an important factor

#*********b) Are data usage connectivity issues
#turning out to be costly? In other words, is it leading to churn?


#  ****************** plcd_dat_Mean - Mean number of attempted data calls placed   -0.001348547
#  ****************** drop_blk_Mean - Mean no. of blocked / failed data calls       0.006543306 
#  ****************** drop_dat_Mean - Mean no. of dropped / failed data calls       -0.049622737

#   The above variables express data usage connectivity.

#   The Data Quality Report for all the above variables show that 
#   making data calls or using the internet. 
#   This could be a matter of concern since the global market survey report shows "Subscribers who 
#   have switched operators in recent months reported two key information sources in their decision:
#   the Internet and recommendation of family and friends.. 
#   In this case it seems customers are not really using the internet. So it would be good to work 
#   towards attaining more customers to use data and also towards proving quality network connectivity
#   and service to provide maximum customer satisfaction and reduce Churn.
#   Since there is not enough usable data for the above variables they are not showing any influence 
#   on the Churn Behaviour at Mobicom.
# as The Beta coefficient is not showing a strong impact of overage billing as an influencer 
#   of churn behaviour. 





#   3. Would you recommend rate plan migration as a proactive retention strategy?

library(dplyr)
new%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat45
dat45$N<-unclass(new%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat45$churn_perc<-dat45$n/dat45$N
dat45$GreaterThan<-unclass(new%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dat45$LessThan<-unclass(new%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dat45$varname<-rep("totmrc_Mean",nrow(dat45))
View(dat45)
plot(dat45$churn_perc,xlab=c("monthly recurring charge"),ylab=c("churn percentage"),
     main=c("churn rate vs totmrc"),col="red")
#there is no trend (increse or decrease) in decile binning .So, we conclude that it wont retain the customer. 
#  Even when i am looking at Variable ovrrev_Mean has beta coefficient of 0.004291200 
#   var ovrrev_Mean = DATOVR_MEAN + VCEOVR_MEAN i.e. 'Mean overage revenue'
#   It is the sum of data and voice overage revenues representing the overage revenue earned 
#   from customers after billing the same to them.
#   The Beta coefficient is not showing a strong impact of overage billing as an influencer 
#   of churn behaviour.
#  But overall rate plan migration as a proactive retention strategy
#   might not help much at Mobicom.



#   4. What would be your recommendation on how to use this churn model for prioritisation
#   of customers for a proactive retention campaigns in the future?

View(test)

#Gains Chart
library(gains)
gains(test$churn,predict(result_my_2,type="response",newdata=test),groups = 10)
#the Gains Chart shows that the top 20% of the probabilities contain 28.0% customers that are highly likely to churn.


# Selecting Customers with high churn rate
test$prob<-predict(result_my_2,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

# Top 20% of the probabilities lie between 0.3113697 0.6773456

# Applying cutoff value to predict customers who Will Churn

pred2<-predict(result_my_2, type="response", newdata=test)
pred2<-ifelse(pred2>=0.3113697 , 1, 0)
table(pred2,test$churn)
Targeted<-test[test$prob>0.3113697 & test$prob<=0.6773456 & test$churn=="1","Customer_ID"]
Targeted<-as.data.frame(Targeted)
nrow(Targeted)

write.csv(Targeted,"Target_Customers.csv",row.names = F)
#   Thus Using the model can be used to predict customers with high probability of Churn and extract the 
#   target list using their "Customer ID".


#5.5.	What would be the target segments for proactive retention campaigns? Falling ARPU forecast is also a concern and therefore,
#Mobicom would like to save their high revenue customers besides managing churn. Given a budget constraint of a contact list of 
#20% of the subscriber pool, which subscribers should prioritized if "revenue saves" is also a priority besides controlling churn. 
#In other words, controlling churn is the primary objective and revenue saves is the secondary objective.


# Solution:
pred3<-predict(result_my_2, type="response", newdata=test)
test$prob<-predict(result_my_2,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

#*************below 30% is retain customer probability and above 0.31 is high churn probability
pred4<-ifelse(pred3<0.2030334,"Low_Score",ifelse(pred3>=0.2030334 & pred3<0.3113697,"Medium_Score","High_Score"))
table(pred4,test$churn)


#************looking for high revenue customers
summary(result_my_2)
str(test$totrev)
quantile(test$totrev,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
Revenue_Levels<-ifelse(test$totrev<670.660,"Low_Revenue",ifelse(test$totrev>=670.660 & 
                                                                  test$totrev<1034.281,"Medium_Revenue","High_Revenue"))

table(Revenue_Levels)

table(pred4,Revenue_Levels)

##  Thus this table can be used to select the levels of customers are to be targeted
##  and the Target list can be extracted as follows:

test$prob_levels<-ifelse(pred3<0.2030334,"Low_Score",ifelse(pred3>=0.2030334 & pred3<0.3113697,"Medium_Score","High_Score"))
test$Revenue_Levels<-ifelse(test$totrev<670.660,"Low_Revenue",ifelse(test$totrev>=670.660 & 
                                                                       test$totrev<1034.281,"Medium_Revenue","High_Revenue"))

Targeted1<-test[test$prob_levels=="High_Score" & test$Revenue_Levels=="High_Revenue","Customer_ID"]
Targeted1<-as.data.frame(Targeted1)
nrow(Targeted1)

write.csv(Targeted1,"High_Revenue_Target_Customers.csv",row.names = F)

hist(data$totcalls,col="orange")
hist(data$churn,)



