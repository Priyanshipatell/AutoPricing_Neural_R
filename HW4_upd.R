## Authors: Priyanshi Patel(650927804) 
          # Ajay Pawar 
          # Darshan Radadiya
#Importing libraries
library(readxl)
library(caret)
#library(h2o)
library(neuralnet)
library(remotes)

#Read data from excel file
hw4Data <- read_excel("C:/Users/DELL/Desktop/UIC/Sem 3/IDS 572/Case Study/HW4 CS/HW4_Data.xlsx",
                      sheet = "draft")

str(hw4Data)

#Encoding Categorical variable as factor
hw4Data$Colour <- as.numeric(factor(hw4Data$Colour,
                             levels = c("Silver", "Red", "Black", "Grey", "Blue", "Green"),
                             labels = c(1, 2, 3, 4, 5, 6)))

hw4Data$Fuel <- as.numeric(factor(hw4Data$Fuel,
                                  levels = c("Petrol"),
                                  labels = c(1)))
str(hw4Data)
as.factor(trainingSet$Price)

#Divide the data set into Training and Test instances
set.seed(1234)
splitIndex <- createDataPartition(hw4Data$Price, p = .70,
                                  list = FALSE, times = 1)

trainingSet <- hw4Data[splitIndex, ]
testingSet <- hw4Data[-splitIndex, ]

View(trainingSet)

#Scaling
trainingSet$Price <- (trainingSet$Price - min(trainingSet$Price))/(max(trainingSet$Price) - min(trainingSet$Price))
testingSet$Price <- (testingSet$Price - min(testingSet$Price))/(max(testingSet$Price) - min(testingSet$Price))

View(testingSet)
model1 <- neuralnet(Price ~ Age+KM+HP+Mfr_G+ABS+Abag_1+Abag_2+AC+Pwi+PStr,
                    data = trainingSet, hidden = 2, err.fct = "sse", threshold = 0.05,
                    linear.output = FALSE)

model1

model1$result.matrix
plot(model1)

trainingSet$Price
model1$net.result[[1]]

# Validating using different Hidden Neurons 
model2 <- neuralnet(Price ~ Age+KM+HP+Mfr_G+ABS+Abag_1+Abag_2+AC+Pwi+PStr,
                    data = trainingSet, hidden = 5, err.fct = "sse", threshold = 0.05,
                    linear.output = FALSE)
model2


# Predict using Test data for model1
pred = as.factor(predict(model1, testingSet, type = "class"))
TP <- table(pred, testingSet$Price, dnn = c("predicted","actual"))
#confusionMatrix(TP,positive = "1")
print(TP)

res <- compute(model1, testingSet)
res$net.result
head(trainingSet[1,])

# Decay Parameter using Validation set
set.seed(156)
indx <- sample(2, nrow(trainingSet), replace = T, prob = c(0.7, 0.3))
training2 <- trainingSet[indx == 1, ]
validation <- trainingSet[indx == 2, ]
err <- vector("numeric", 100)
d <- seq(0.0001, 1, length.out=100)
k = 1
for(i in d) {
  validmodel <- nnet(Price ~., data = training2, decay = i, size = 10, maxit = 1000)
  pred.class <- predict(validmodel, newdata = validation, type = "class")
  err[k] <- mean(pred.class != validation$Price)
  k <- k +1
}
plot(d, err)


##### Linear Regression Model ######

DataL <- read_excel("C:/Users/DELL/Desktop/UIC/Sem 3/IDS 572/Case Study/HW4 CS/HW4_Data.xlsx",
                      sheet = "draft")
DataL$Colour <- as.numeric(factor(DataL$Colour,
                                    levels = c("Silver", "Red", "Black", "Grey", "Blue", "Green"),
                                    labels = c(1, 2, 3, 4, 5, 6)))

DataL$Fuel <- as.numeric(factor(DataL$Fuel,
                                  levels = c("Petrol"),
                                  labels = c(1)))
# Creating Test and Train instances for Linear Model
str(DataL)
set.seed(154)
indx <- sample(2, nrow(DataL), replace = T, prob = c(0.7, 0.3))
trainL <- DataL[indx == 1, ]
testL <- DataL[indx == 2, ]
# Construct Linear model
lmmod <- lm(Price ~., data = trainL)
summary(lmmod)
coefficients(lmmod)
confint(lmmod, level = 0.95)
residuals(lmmod)
# Plot Histogram 
hist(lmmod$residuals, main = "histogram of Residuals", xlab = "Residuals")
plot(lmmod)

# Checking Performance on Test Data
predL <- predict(lmmod, newdata = testL)
meanL <- mean(testL$Price - predL)^2
meanL
