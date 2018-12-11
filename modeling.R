library(dplyr)
library(tidyr)
library(purrr)
library(BAS)
library(randomForest)
library(neuralnet)
library(stringr)

load("county_data.RData")
# drop state, county
county_data = county_data %>% 
    select(- County, - State)
# rescale mean 0 sd 1
rescale_mean = apply(county_data, 2, mean)
rescale_sd = apply(county_data, 2, sd)
county_data = county_data %>%
    sweep(2, rescale_mean, "-") %>%
    sweep(2, rescale_sd, "/")
# train test split 3:1
set.seed(0)
train_index = sample(1:dim(county_data)[1], floor(dim(county_data)[1] * 0.75))
train = county_data[train_index, ]
test = county_data[- train_index, ]
# R-square function
Rsq = function(y_pred, y){
    1 - sum((y_pred - y) ^ 2) / sum((y - mean(y)) ^ 2)
}
# linear model
lm1 = lm(price ~ ., train)
print(Rsq(predict(lm1, train, type = "response"), train$price))
print(Rsq(predict(lm1, test, type = "response"), test$price))
# linear model with interaction
lm2 = lm(price ~ . ^ 2, train)
print(Rsq(predict(lm2, train, type = "response"), train$price))
print(Rsq(predict(lm2, test, type = "response"), test$price))
### AIC/BIC selection could be used here ###
# Bayesian Model Averaging
bma1 = bas.lm(price ~ ., train)
# predictors with posterior probability large than 0.1:
summary(bma1)[, 1] %>%
    .[. > 0.1 & !is.na(.)] %>%
    names() %>%
    print()
# select useful variables from BMA
cols = summary(bma1)[, 1] %>%
    .[. > 0.1 & !is.na(.)] %>%
    names() %>%
    {c("price", .[-1])}
train2 = train %>% select(cols)
test2 = test %>% select(cols)
# linear model again
lm3 = lm(price ~ ., train2)
print(Rsq(predict(lm3, train2, type = "response"), train2$price))
print(Rsq(predict(lm3, test2, type = "response"), test2$price))
# with interactions again
lm4 = lm(price ~ . ^ 2, train2)
print(Rsq(predict(lm4, train2, type = "response"), train2$price))
print(Rsq(predict(lm4, test2, type = "response"), test2$price))
### AIC/BIC selection for interactions here ###
# random forest
rf1 = randomForest(price ~ ., data = train2, ntree = 3000)
print(Rsq(predict(rf1, train2, type = "response"), train2$price))
print(Rsq(predict(rf1, test2, type = "response"), test2$price))
# fully connected neural networks + early stopping + bagging (no bootstrapping)
f = do.call(paste, as.list(cols)) %>% 
    str_replace_all(" ", " + ") %>%
    str_replace("\\+", "~") %>%
    as.formula()
# bagging here
nbags = 100
# early stoppping by setting threshold
nn2 = map(1:nbags, function(i){neuralnet(f, train2, hidden = 10, threshold = 1.5)})
y_train_pred = nn2 %>%
    map(function(nn){compute(nn, train2[, -1])$net.result}) %>%
    unlist() %>%
    matrix(ncol = nbags) %>%
    rowMeans()
y_test_pred = nn2 %>%
    map(function(nn){compute(nn, test2[, -1])$net.result}) %>%
    unlist() %>%
    matrix(ncol = nbags) %>%
    rowMeans()
print(Rsq(y_train_pred, train2$price))
print(Rsq(y_test_pred, test2$price))
### neural net + early stopping + bagging is slightly better than random forest
# parameters for neural nets are yet to be tunned, if time allows
