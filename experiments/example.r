library(vest)

# a small dataset for illustrative purposes
data(AirPassengers)

dataset <- my_embedd(AirPassengers, 10, h=1)
form <- target ~.
freq <- frequency(AirPassengers)


train <- head(dataset, -5)
test <- tail(dataset, 5)

train_feats <-
  feature_engineering(x = train,
                      targets = "target",
                      freq = freq)

train_dyns <- feat_select_corr(x = train_feats)
test_dyns <- predict(train_feats, test)

trainf <- cbind.data.frame(train_dyns, train)
testf <- cbind.data.frame(test_dyns, test)

model <- LASSO.train(form, trainf)
preds <- LASSO.train(model, testf)
