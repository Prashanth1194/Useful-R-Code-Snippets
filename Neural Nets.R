## Loading the libraries
library(readr)
library(neuralnet)
library(DMwR)
library(boot)
library(plyr)
library(matrixStats)

## Reading the data
data = read_csv("cereals.csv")

## Random sampling
samplesize = 0.60 * nrow(data)
set.seed(80)
index = sample(seq_len(nrow(data)), size = samplesize)

## Create training and test set
train = data[ index, ]
test = data[ -index, ]

## Scaling the data
max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))

## Creating training and test set
train_Neural = scaled[index , ]
test_Neural = scaled[-index , ]

## Fit neural network
set.seed(2)
NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, train_Neural, hidden = c(5,4,3) , linear.output = T )

## Plot neural network
plot(NN)

## Making Prediction
predict_testNN = compute(NN, test_Neural[,c(1:5)])
predict_testNN = (predict_testNN$net.result * (max(data$rating) - min(data$rating))) + min(data$rating)

plot(test$rating, predict_testNN, col='blue', pch=16, ylab = "Predicted Rating", xlab = "Actual Rating")

abline(0,1)

## Checking error metrics
regr.eval(test$rating,predict_testNN)

# Cross-Validation

## Initialize variables
set.seed(100)
k = 100
RMSE.NN = NULL

List = list( )

for(j in 10:65){
  for (i in 1:k) {
    index = sample(1:nrow(data),j )
    
    trainNN = scaled[index,]
    testNN = scaled[-index,]
    datatest = data[-index,]
    
    NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN, hidden = 3, linear.output= T)
    predict_testNN = compute(NN,testNN[,c(1:5)])
    predict_testNN = (predict_testNN$net.result*(max(data$rating)-min(data$rating)))+min(data$rating)
    
    RMSE.NN [i]<- (sum((datatest$rating - predict_testNN)^2)/nrow(datatest))^0.5
  }
  List[[j]] = RMSE.NN
}

Matrix.RMSE = do.call(cbind, List)

## Box-Plot of the RMSE values obtained
boxplot(Matrix.RMSE[,56], ylab = "RMSE", main = "RMSE BoxPlot (length of traning set = 65)")

## Checking variation of RMSE with training set size
med = colMedians(Matrix.RMSE)
X = seq(10,65)
plot (med~X, type = "l", xlab = "length of training set", ylab = "median RMSE", main = "Variation of RMSE with length of training set")
