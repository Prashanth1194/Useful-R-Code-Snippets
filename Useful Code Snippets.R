## Taking First Difference
dt <- data.table(data5)
setkey(dt,ID)
dt[,diff:=c(NA,diff(Value)),by=ID]
dt <- plyr::rename(dt,c("diff" = "first_difference_0"))

## Taking Lag
cols = 'dv'
dt[, c("first_difference_lag1", "first_difference_lag2","first_difference_lag3") := shift(.SD, 1:3),by=ID, .SDcols=cols]

## Group Level Count, mean, median etc
df_train =  df_train %>% group_by(ip,app,channel) %>% summarise(ip_app_chnl_hour_mean = mean(hour)) %>% left_join(df_train,.,by=c('ip','app','channel'))
df_train =  df_train %>% group_by(ip,app,channel) %>% summarise(ip_app_chnl_hour_median = median(hour)) %>% left_join(df_train,.,by=c('ip','app','channel'))
df_train =  df_train %>% group_by(ip,app,channel) %>% summarise(ip_app_chnl_hour_min = min(hour)) %>% left_join(df_train,.,by=c('ip','app','channel'))
df_train =  df_train %>% group_by(ip,app,channel) %>% summarise(ip_app_chnl_hour_max = max(hour)) %>% left_join(df_train,.,by=c('ip','app','channel'))

## Adding Count variables
df_name <-  df_train %>% add_count(ip, wday, in_test_hh) %>% rename("nip_day_test_hh" = n) %>%
  
## Corrplot 
library(corrplot)
corrplot(M, method="number")

## ggplot to plotly directly
library(ggplot2)
library(plotly)

your_ggplot <- ggplot(mtcars, aes(hp, mpg)) + geom_point()
ggplotly(your_ggplot)


## Train-Test Split
set.seed(101)
sample <- sample.int(n = nrow(data), size = floor(.75*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]

library(caTools)
sample = sample.split(data$anycolumn, SplitRatio = .75)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)

## Missing values in each column
sapply(airquality, function(x) sum(is.na(x)))

## Convert all character to factor in R
DF <- as.data.frame(unclass(DF))
str(DF)

DF[sapply(DF, is.character)] <- lapply(DF[sapply(DF, is.character)],as.factor)
str(DF)

## Convert factors to dummy columns and bind numeric and factors together
cont0 <- sapply(train1,is.numeric)
names0 <- names(train1[,cont0])
factor0 <- sapply(train1, is.factor)
numeric0 <- sapply(train1, is.numeric)
numeric <- train1[,numeric0]
numeric <- as.matrix(subset(numeric, select = -c(dv)))
dv <- as.vector(train1$dv)
ivs <- numeric
if(sum(factor0)>1)
{
  factors <- model.matrix(~.,data=train1[,factor0]) # USE THIS IF THERE ARE MULTIPLE FACTORS
  ivs <- cbind(numeric,factors)
}
if(sum(factor0)==1)
{
  factors <- model.matrix(~.,data=data.frame(a = train1[,factor0])) # USE THIS IF THERE ARE MULTIPLE FACTORS
  ivs <- cbind(numeric,factors)
}

## Moving Average
xtrain[ , `:=`(h3a = rollapply(holiday_flg, width = 3, FUN = function(s) sign(sum(s, na.rm = T)),
                               partial = TRUE, fill = 0, align = 'right') ),
        by = c('air_store_id')]


## Logistic Regression Code Snippet
model <- glm(dv ~.,family=binomial(link='logit'),data=train)

### Referral links:
#### http://r-statistics.co/Logistic-Regression-With-R.html
#### https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/

## Logisitc Model Tree (LMT)
### Referral links: 
#### https://stackoverflow.com/questions/7019912/using-the-rjava-package-on-win7-64-bit-with-r
#### https://rdrr.io/cran/RWeka/man/Weka_control.html
#### https://cran.r-project.org/web/packages/RWeka/RWeka.pdf
#### https://www.quora.com/What-is-Logistic-Model-tree-How-can-I-implement-it-using-R
#### https://www.quora.com/3-What-is-Logistic-Model-Tree-And-how-does-it-combine-the-concept-of-Logistic-Regression-and-Decision-Tree

library(rJava)
library(RWeka)

fit_lmt <- LMT(Status ~ ., data = train_data,control = Weka_control(R = TRUE, M = 5))

summary(fit_lmt)

pred_test = predict(fit_lmt,test_data)
table(test_data$Status,pred_test)

#### Evaluation
library(caret)
pred_test = ifelse(pred_test=="good",'1','0')
test_data$Status = ifelse(test_data$Status=="good",'1','0')

pred_test = as.factor(pred_test)
test_data$Status = as.factor(test_data$Status)

precision <- posPredValue(pred_test, test_data$Status, positive="1")
recall <- sensitivity(pred_test, test_data$Status, positive="1")
F1 <- (2 * precision * recall) / (precision + recall)


## Decision Tree Code Snippet
library(rpart)
fit <- rpart(dv ~ iv1 + iv2 + iv3,method="class", data=train)

printcp(fit) 
plotcp(fit) 
summary(fit)

plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

## C5.0 Decision Tree
library(recipes)
data(credit_data)

vars <- c("Home", "Seniority")

set.seed(2411)
in_train <- sample(1:nrow(credit_data), size = 3000)
train_data <- credit_data[ in_train,]
test_data  <- credit_data[-in_train,]

### Simple C5.0 decision tree
library(C50)
tree_mod <- C5.0(x = train_data[, vars], y = train_data$Status)
tree_mod
summary(tree_mod)
plot(tree_mod)

### 3 iterations of boosting in C5.0 decision tree
tree_boost <- C5.0(x = train_data[, vars], y = train_data$Status, trials = 3)
summary(tree_boost)
plot(tree_boost,trial =2)

predict(tree_boost, newdata = test_data[1:3, vars], type = "prob")

### Cost Sensitive Models (Note: Penalizing bad ones in this case)
cost_mat <- matrix(c(0, 1, 2, 0), nrow = 2)
rownames(cost_mat) <- colnames(cost_mat) <- c("bad", "good")
cost_mat

cost_mod <- C5.0(x = train_data[, vars], y = train_data$Status, 
                 costs = cost_mat)
summary(cost_mod)
plot(cost_mod)

table(predict(cost_mod, test_data[, vars]))
table(predict(tree_mod, test_data[, vars]))
table(predict(tree_boost, test_data[, vars]))

## CHAID model 
#### Reading link : https://www.r-bloggers.com/chaid-and-r-when-you-need-explanation-may-15-2018/
####              : https://arxiv.org/pdf/1901.00251.pdf

install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(CHAID)

vars1 = c("Home","Marital","Status")
ctrl1 <- chaid_control(minsplit = 200, minprob = 0.1)
chaidUS1 <- chaid(Status ~ ., data = train_data[,vars1], control = ctrl1)

print(chaidUS1)
plot(chaidUS1)


## Random Forest
library(randomForest)
rf_fit = randomForest(Response ~ ., data = train_data)#, ntree = 500, mtry = 6, importance = TRUE)
predTrain = predict(rf_fit, test_data, type = "class")

### Hyperparameter tuning in RF : https://rpubs.com/chidungkt/300153 and https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

## GBM Code
library(gbm)
set.seed(123)

gbm.fit <- gbm(
  formula = Status ~ .,
  distribution = "gaussian",
  data = train_data[,c(1:3)],
  n.trees = 100,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

print(gbm.fit)

sqrt(min(gbm.fit$cv.error))
gbm.perf(gbm.fit, method = "cv")

## Hyperparameter tuning for GBM : http://uc-r.github.io/gbm_regression


## Logit Boost
### Reading link : https://www.datatechnotes.com/2018/07/logisboost-classification-sample-in-r.html
###              : https://www.rdocumentation.org/packages/caTools/versions/1.17.1/topics/LogitBoost
###              : https://qizeresearch.wordpress.com/2013/12/12/logit-boost-example/

library(caTools)
set.seed(123)

fit_lb = LogitBoost(train_data[,c("Seniority","Time")],train_data$Status,10)
pred_test <- predict(fit_lb,test_data[,c("Seniority","Time")])
table(test_data$Status,pred_test)

#### Evaluation
library(caret)
pred_test = ifelse(pred_test=="good",'1','0')
test_data$Status = ifelse(test_data$Status=="good",'1','0')

pred_test = as.factor(pred_test)
test_data$Status = as.factor(test_data$Status)

precision <- posPredValue(pred_test, test_data$Status, positive="1")
recall <- sensitivity(pred_test, test_data$Status, positive="1")
F1 <- (2 * precision * recall) / (precision + recall)


## Xgboost Code Snippet
library(xgboost)
dtrain = xgb.DMatrix(data = data.matrix(train1[,c(3:12,14,15)]),label=train1$Outcome)
dtest = xgb.DMatrix(data = data.matrix(val[,c(3:12,14,15)]),label=val$Outcome)


watch = list(train = dtrain,test = dtest)

xgb <- xgb.train(data =dtrain, 
                 eta = 0.01,
                 nround=500,
                 set.seed = 1,
                 max.depth = 8,
                 eval_metric = "logloss",
                 watchlist = watch,
                 early_stopping_round = 10,
                 subsample = 0.9,
                 colsample  = 0.9,
                 objective = "binary:logistic"
                 
                 
)

test$Outcome = predict(xgb,data.matrix(test[,c(3:14)]))

### Parameter Tuning
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 3, 
                        #summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        allowParallel=T)

xgb.grid <- expand.grid(nrounds = 1000,
                        eta = c(0.01,0.05,0.1),
                        max_depth = c(2,4,6,8,10,14)
)
set.seed(45)
xgb_tune <-train(formula,
                 data=train,
                 method="xgbTree",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 verbose=T,
                 metric="Kappa",
                 nthread =3
)

### Link: 
#### https://stats.stackexchange.com/questions/171043/how-to-tune-hyperparameters-of-xgboost-trees
#### https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/

## Regularized Greedy Forest (RGF)
### Reading Link: https://www.analyticsvidhya.com/blog/2018/02/introductory-guide-regularized-greedy-forests-rgf-python/

### Installation Command :  pip install rgf_python -v
library(RGF)
set.seed(1)

x = matrix(runif(1000), nrow = 100, ncol = 10)

y = sample(1:2, 100, replace = TRUE)

RGF_class = RGF_Classifier$new(max_leaf = 50)

RGF_class$fit(x, y)

preds = RGF_class$predict_proba(x)


if (reticulate::py_available() && reticulate::py_module_available("rgf.sklearn")) {
  
  library(RGF)
  
  set.seed(1)
  x = matrix(runif(1000), nrow = 100, ncol = 10)
  
  y = sample(1:2, 100, replace = TRUE)
  
  RGF_class = FastRGF_Classifier$new(max_leaf = 50)
  
  RGF_class$fit(x, y)
  
  preds = RGF_class$predict_proba(x)
}

## Kernel Random Forest
#### Link
##### https://uwaterloo.ca/statistics-and-actuarial-science/sites/ca.statistics-and-actuarial-science/files/uploads/files/2009-06.pdf
##### https://rmarcus.info/blog/2017/10/04/rfk.html
##### https://cran.r-project.org/package=kernelFactory
##### https://rdrr.io/cran/kernelFactory/

##### http://wps-feb.ugent.be/Papers/wp_12_825.pdf

library(kernelFactory)
library(caTools)

data("Credit")
sample = sample.split(Credit$Response, SplitRatio = .75)
train_data = subset(Credit, sample == TRUE)
test_data    = subset(Credit, sample == FALSE)

fit_krf = kernelFactory(x=train_data[,-16],y=train_data$Response,
                        method = "random", ntree = 500)

pred_test_krf<- predict(fit_krf, test_data)
pred_test_krf = ifelse(pred_test_krf>0.5,"1","0")

table(test_data$Response,pred_test_krf)


## Sampling technique links :

### https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/
### https://stackoverflow.com/questions/36651596/how-to-balance-unbalanced-classification-11-with-smote-in-r
