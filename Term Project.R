
#FINAL PROJECT
#Group Members: Nella Khachian, Grant Cavan, Arman Sood, Anika Nguyenkhoa



library(tidyverse)
library(ggplot2)
library(rsample)
library(forcats)
library(caret)
library(yardstick)
library(glmnet)
library(glmnetUtils)
library(plotROC)
library(coefplot)
library(ggcorrplot)


data_1 <- Maths


data_2 <- Portuguese





library(dplyr)
new_data <- full_join(data_1, data_2)



summary(new_data)


glimpse(new_data)


view(new_data)



library(tree)

summary(new_data)



# Split/Train/Test Stuff
set.seed(310)
alc_split <- initial_split(new_data, prop = 0.8)
alc_train <- training(alc_split)
alc_test <- testing(alc_split)

# Makes it Classification rather than Regression 
alc_train$studytime <- factor(alc_train$studytime)

# Random Forest
model <- randomForest(studytime ~ Mjob + Fjob + Medu + Fedu + Pstatus + famsup, 
                      data = alc_train)

dim(alc_split)

alc_bag <- randomForest(studytime ~ .,
                        data = alc_train,
                        ntree = 500,
                        ntr = 5,
                        importance = TRUE)

print(alc_bag)
plot(alc_bag)

rf_alc <- randomForest(studytime ~ .,
                       data = alc_train,
                       ntree = 1000,
                       mtry = 4,
                       importance = TRUE)

print(rf_alc)
plot(rf_alc)

# Tree stuff
alc_tree <- tree(studytime ~ Mjob + Fjob + Medu + Fedu + Pstatus + famsup, data = alc_train)

# Results
summary(alc_tree)
print(alc_tree)

# Plot the tree
plot(alc_tree)
text(alc_tree, pretty = 0)




library(tidyverse)
new_data %>% glimpse
clean <- new_data %>% 
  mutate(studytime = factor(studytime))
clean %>% glimpse

#splitting the data set into training (80%) and test (20%)
library(rsample)
set.seed(310)
data_split <- initial_split(clean, prop = 0.8)
data_train <- training(data_split)
data_test <- testing(data_split)

dim(data_split)

# build a logistic (logit) model : 
# default ~ balance
logit1 <- glm(studytime ~ ., data = data_train, 
              family=binomial) 
logit1
summary(logit1)

# exponentiate the coefficients
exp(logit1$coefficients)


# 9 - Predict the probability of the index going UP
index_train <- predict(logit1, type="response")
head(index_train)

index_test <- predict(logit1, type="response", newdata = data_test)
head(index_test)


table(clean$studytime)




#Load the dataset
Maths <- read.csv("datasets/Maths.csv")
Portuguese <- read.csv("datasets/Portuguese.csv")

data_1 <- Maths
data_2<- Portuguese

library(dplyr)
new_data <- full_join(data_1, data_2)

library(tidyverse)
new_data %>% glimpse

library(forcats)

clean <- new_data %>% 
  mutate(studytime = factor(studytime), 
         studytime = fct_lump(studytime, n = 2, other_level = "2")) 
  

clean %>% glimpse

#splitting the data set into training (80%) and test (20%)
library(rsample)
set.seed(310)
data_split <- initial_split(clean, prop = 0.8)
data_train <- training(data_split)
data_test <- testing(data_split)

dim(data_split)

# build a logistic (logit) model : 
# default ~ balance
logit1 <- glm(studytime ~ ., data = data_train, 
              family=binomial) 
logit1
summary(logit1)

# exponentiate the coefficients
exp(logit1$coefficients)

# Make predictions on the testing set
predictions <- predict(logit1, newdata=data_test, type="response")

# Convert the probabilities to binary predictions
binary_predictions <- ifelse(predictions > 0.5, "more than 5 hours", "less than 5 hours")

# Create the confusion matrix
table(true=data_test$studytime, predicted=binary_predictions)

TN <- 122
TP <- 28
FN <- 22
FP <- 37

accuracy = (TN+TP)/(TN+TP+FN+FP)
print(accuracy)

sensitivity <- TP / (TP+FN)
print(sensitivity)

specificity = TN / (TN + FP)
print(specificity)

# 12 - ROC curve
library(plotROC)
library(ggplot2)
library(pROC)

roc_data <- roc(data_test$studytime, predictions)

ggroc(roc_data) + ggtitle("ROC Curve")

# AUC
auc(roc_data)








