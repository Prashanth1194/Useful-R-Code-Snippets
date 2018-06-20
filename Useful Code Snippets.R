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

## Decision Tree Code Snippet
library(rpart)
fit <- rpart(dv ~ iv1 + iv2 + iv3,method="class", data=train)

printcp(fit) 
plotcp(fit) 
summary(fit)

plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

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
