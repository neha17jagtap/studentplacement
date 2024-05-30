df<-read.csv("placementdata.csv",header = TRUE, stringsAsFactors = TRUE)
View(df)
df$status<- ifelse ( df$status=="Placed",1,0)
df$status
View(df)

# 10th percentage
cor(df$ssc_p, df$status)
# 12th percentage
cor(df$hsc_p, df$status)
# degree percentage
cor(df$degree_p, df$status)
# mba %age
cor(df$mba_p, df$status)
cor(df$etest_p, df$status)


# salary corellation 
cor(df$mba_p, df$salary)
cor(df$degree_p, df$salary)
cor(df$hsc_p, df$salary)
cor(df$ssc_p, df$salary)


boxplot(df$hsc_p)
boxplot(df$degree_p,outline=FALSE) 

set.seed(345)
train = sample(1:nrow(df), nrow(df)*(2/3)) 
train
df.train = df[train,] 
df.test = df[-train,]

fit = rpart(status~., 
            data=df.train,
            method="class", 
            control=rpart.control(xval=0, minsplit=100),
            parms=list(split="gini"))
fit

install.packages('rpart.plot') 
library(rpart.plot)
rpart.plot(fit, type = 1, extra = 1)

df.pred<-predict(fit,df.train, type="class")
df.actual<-df.train$status
confusion.matrix <-table(df.pred,df.actual)
prop.table(confusion.matrix)
confusion.matrix


# extract the vector of predicted class for each observation in titanic.train
pred <- predict(fit, df.train, type="class")
# extract the actual class of each observation in titanic.train
actual <- df.train$status
# now build the confusion matrix
# which is the contingency table of predicted vs actual
confusion.matrix <- table(pred, actual)
confusion.matrix
# accuracy is 83%
# error rate 0.17
# TPR(senstivity) = 0.23
# TNR(specificity)=
# FNR= 0.76

# logistic regression

# load the data
bank.df <- read.csv("UniversalBank.csv")
# convert output as factor
df$status <- as.factor(df$status)
df$specialisation <- as.factor(df$specialisation)
df$specialisation 
df$ssc_b<- as.factor(df$ssc_b)
df$hsc_b<- as.factor(df$hsc_b)
df$hsc_s<- as.factor(df$hsc_s)
df$hsc_s
df$degree_t<- as.factor(df$degree_t)
df$degree_t
df$workex<-as.factor(df$workex)
# treat SPECIALISATION as categorical
df$degree_t<-factor(df$degree_t, levels = c(1, 2, 3),
                            labels = c("Sci&Tech", "Comm&Mgm","Others"))

# split the data into training and test data sets
set.seed(2) # for reproducible results
train <- sample(1:nrow(df), (0.6)*nrow(df))
train.df <- df[train,]
test.df <- df[-train,]

colnames(df)
colnames(df)<- c("serial no","gender","ssc percentage","ssc board","hss percentage","hsc board","hss stream",
                 "degree percentage","degree","workex","x","specialisation","mba perc","status","salary")
# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic
logit.reg <- glm(status~ ssc_p+ssc_b+hsc_b+hsc_p+hsc_s+ degree_p+etest_p+mba_p+ salary,
                              data = df, family = "binomial") 
# results of logistic regression
summary(logit.reg)
