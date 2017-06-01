train <- read.csv("train.csv", stringsAsFactors = FALSE)
train <- train[, -1]

set.seed(1)
train.ind <- sample(1:dim(train)[1], dim(train)[1] * 0.7)
train.data <- train[train.ind, ]
test.data <- train[-train.ind, ]

# decision tree
# https://www.r-bloggers.com/a-brief-tour-of-the-trees-and-forests/
library(rpart)
# formula <- paste("SalePrice ~ ", paste(colnames(train)[-length(colnames(train))], collapse = " + "))
formula <- paste("log(SalePrice) ~.-SalePrice ")
# step 1 start with a small cp (complexity parameter, alpha)
set.seed(1)
tree1 <- rpart(formula, method = 'anova', data = train.data, 
               control=rpart.control(cp=0)) # cp = 1, 0.1, 0.01, 0
# method = 'class' for classification
# method = 'anova' for regression

tree1
# deviance is determined as Sum(observed_i - mu_i)^2, sum of squared error of data in that node
# http://stats.stackexchange.com/questions/6581/what-is-deviance-specifically-in-cart-rpart
mean(log(train.data$SalePrice)) # yi is actually mu_i

printcp(tree1)
# display the cross validation result
# rel error = sum of squared over all leaf nodes of train data / sum of squared at root node
# xerror = sum of squared over all leaf nodes of left over data / sum of squared at root node

plotcp(tree1) 
# plot xerror

# step 2 Pick the tree size that minimizes xerror.
# Hence we want the cp value that minimizes the xerror.
bestcp <- tree1$cptable[which.min(tree1$cptable[,"xerror"]), "CP"]
# But simpler model, as long as the xerror - 1std, xerror + 1std contains xerror_min
cp.tab <- as.data.frame(tree1$cptable)
with(cp.tab, min(which(xerror - xstd < min(xerror))))
bestcp <- cp.tab$CP[with(cp.tab, min(which(xerror - xstd < min(xerror))))]

# Step 3: Prune the tree using the best cp.
tree.pruned <- prune(tree1, cp = bestcp)
tree.pruned

test.pred <- predict(tree.pruned, test.data)
# error becuz test.data has new level, not found in train.data, for example Condition2
# what should we do? impute it as NA
for(i in 1:dim(train.data)[2]) {
  if(is.character(train.data[, i]) & 
     length(which(!unique(test.data[, i]) %in% unique(train.data[, i]))) > 0) {
    print(paste("this column: ", colnames(train.data)[i], "has new levels in test"))
  } 
}

test.data$Condition2[which(!test.data$Condition2 %in% train.data$Condition2)] <- NA
test.data$RoofMatl[which(!test.data$RoofMatl %in% train.data$RoofMatl)] <- NA
test.data$Heating[which(!test.data$Heating %in% train.data$Heating)] <- NA
test.data$Functional[which(!test.data$Functional %in% train.data$Functional)] <- NA
test.data$MiscFeature[which(!test.data$MiscFeature %in% train.data$MiscFeature)] <- NA

test.pred <- predict(tree.pruned, test.data)
# how is NA handled in rpart? https://cran.r-project.org/web/packages/rpart/vignettes/longintro.pdf
# it uses surrogate variable when split feature is missing.
# check other data without missing value in Condition2, and what else feature and value pair could replace the Condition2 for the split node. 
sum((test.pred - log(test.data$SalePrice))^2) #17.7

# plot tree
plot(tree.pruned)
# no uniform, the length of branch indicates decrease of deviance
plot(tree.pruned, uniform = TRUE)
# Since labels often extend outside the plot region it can be helpful to specify xpd = TRUE
text(tree.pruned, cex = 0.5, use.n = TRUE, xpd = TRUE)
# They can be quite ugly and hard to read, especially when you 
# have many levels for a factor since the plot automatically labels them using alphabets.

library(rpart.plot)
# http://blog.revolutionanalytics.com/2013/06/plotting-classification-and-regression-trees-with-plotrpart.html
# plot rpart model
# cex is the text size
prp(tree.pruned, faclen = 0, cex = 0.5)
# http://www.milbo.org/rpart-plot/prp.pdf

# random forest
library(randomForest)
set.seed(2)
rf.formula <- paste("log(SalePrice) ~ .-SalePrice ")
rf.formula
rf <- randomForest(as.formula(rf.formula), data = train.data, importance = TRUE, ntree = 2000)
# note that rf doesn't take missing value like rpart. Lets get rid of missing value.

sum.na <- sort(sapply(train, function(x) { sum(is.na(x)) }), decreasing=TRUE)
keep.col <- names(which(sum.na == 0))
length(keep.col)
train <- train[, keep.col]

train.data <- train[train.ind, ]
test.data <- train[-train.ind, ]

rf <- randomForest(as.formula(rf.formula), data = train.data, importance = TRUE, ntree = 500)
# Still error, why? bcuz character variable in random forest, has to be changed to factor type (google)
# Change all the categorical features to factor type.
for(i in 1:dim(train)[2]) {
  if(is.character(train[, i])) {
    train[, i] <- as.factor(train[, i])
  }
}
train.data <- train[train.ind, ]
test.data <- train[-train.ind, ]
rf <- randomForest(as.formula(rf.formula), data = train.data, importance = TRUE, ntree = 500)

getTree(rf, k = 1, labelVar = TRUE) # output tree 1 for example with variable labeled

par(mar=rep(2,4)) # change margin in plot setting.
# check the setting in par(), like 
par()$mfrow
par(mfrow = c(1,1))

varImpPlot(rf)
# %IncMSE shows if a variable is assigned values by random permutation, how much will the MSE increase?
# 1 train forest
# 2 measure out-of-bag accuracy -> OOB_acc_base
# 3 permute variable i
# 4 measure out-of-bag accuracy -> OOB_acc_perm_i
# 5 VI_i = - (OOB_acc_perm_i - OOB_acc_base)
# So Higher the value, higher the variable importance.
# IncNodePurity is measured by the difference between RSS before and after the split on that variable.

importance(rf, type = 1) # 2)
# Both accuracy one tests to see how worse the model performs without each variable
# The Gini one digs into the mathematics behind decision trees, 
# but essentially measures how pure the nodes are at the end of the tree. 
importanceOrder= order(-rf$importance[, "%IncMSE"])
names=rownames(rf$importance)[importanceOrder]
for (name in names[1:2]) {
  partialPlot(rf, train.data, eval(name), main=name, xlab=name)
}

plot(rf) # see oob error
# Black solid line for overall OOB error. If classification problem, see a bunch of colour lines, one for each class' error
# Can rf get overfitting? Yes.
# Solution: there is also RRF library, or control for how deep the tree could grow,
# say nodesize (Minimum size of terminal nodes) and maxnodes(Maximum number of terminal nodes trees)

test.pred <- predict(rf, test.data) 
sum((test.pred - log(test.data$SalePrice))^2)

# xgboost
library(xgboost) #http://xgboost.readthedocs.io/en/latest/
train.label <- log(train.data$SalePrice)
test.label <- log(test.data$SalePrice)

gbt <- xgboost(data = train.data[, -dim(train.data)[2]], 
               label = train.label, 
               max_depth = 8, 
               nround = 20,
               objective = "reg:linear",
               nthread = 3,
               verbose = 2)
# Error bcuz Xgboost manages only numeric vectors.
# one hot encoding
# The purpose is to transform each value of each categorical feature into a binary feature {0, 1}.

feature.matrix <- model.matrix( ~ ., data = train.data[, -dim(train.data)[2]]) # no NA
set.seed(1)
gbt <- xgboost(data =  feature.matrix, 
               label = train.label, 
               max_depth = 8, # for each tree, how deep it goes
               nround = 20, # number of trees
               objective = "reg:linear",
               nthread = 3,
               verbose = 1)

importance <- xgb.importance(feature_names = colnames(feature.matrix), model = gbt)
head(importance)
# Gain: contribution of each feature to the model. improvement in accuracy brought by a feature to the branches it is on.
# For boosted tree model, gain of each feature in each branch of each tree is taken into account, 
#    then average per feature to give a vision of the entire model.
#    Highest percentage means important feature to predict the label used for the training.
# Cover: the number of observation through a branch using this feature as split feature 
# Frequency: counts the number of times a feature is used in all generated trees (often we don't use it).
# https://kaggle2.blob.core.windows.net/forum-message-attachments/76715/2435/Understanding%20XGBoost%20Model%20on%20Otto%20Dataset.html?sv=2015-12-11&sr=b&sig=Vk8PO2jTJ34csLNWepZ3VFMeF4Jw2h5waRJ9Pft73rA%3D&se=2017-02-22T01%3A15%3A01Z&sp=r

xgb.plot.importance(importance[1:6,])
# error due to library not installed
library(Ckmeans.1d.dp)
xgb.plot.importance(importance[1:6,])
# xgb.plot.tree(model = gbt)
# library("DiagrammeR")
# xgb.plot.tree(feature_names = colnames(feature.matrix), model = gbt, n_first_tree = 1)

# what's the optimal parameter, for example, number of trees?
par <- list( max_depth = 8,
             objective = "reg:linear",
             nthread = 3,
             verbose = 2)

gbt.cv <- xgb.cv(params = par,
                 data = feature.matrix, label = train.label,
                 nfold = 5, nrounds = 100)
plot(gbt.cv$evaluation_log$test_rmse_mean)
# gbt.cv is to choose best nrounds based on certain parameters set. 
# Because, too small nrounds is underfitting, and too large nrounds is overfitting
# But what about the other parameters? 
# See: http://stackoverflow.com/questions/35050846/xgboost-in-r-how-does-xgb-cv-pass-the-optimal-parameters-into-xgb-train

plot(gbt.cv$evaluation_log$train_rmse_mean, type = 'l')
lines(gbt.cv$evaluation_log$test_rmse_mean, col = 'red')
nround = which(gbt.cv$evaluation_log$test_rmse_mean == min(gbt.cv$evaluation_log$test_rmse_mean)) # 64
gbt <- xgboost(data = feature.matrix, 
               label = train.label,
               nround = nround,
               params = par)

# we also would like to choose a relative simple tree that test error is within 1std of minimum test error

# grid searching for parameters.
all_param = NULL
all_test_rmse = NULL
all_train_rmse = NULL

for (iter in 1:20) {
  param <- list(objective = "reg:linear",
                max_depth = sample(5:12, 1)
                #  eta = runif(1, .01, .3)
                #  gamma = runif(1, 0.0, 0.2), 
                #  subsample = runif(1, .6, .9),
                #  colsample_bytree = runif(1, .5, .8), 
                #  min_child_weight = sample(1:40, 1),
                #  max_delta_step = sample(1:10, 1)
  )
  cv.nround = 20
  cv.nfold = 5
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  mdcv <- xgb.cv(data=feature.matrix, label = train.label, params = param, nthread=6, 
                 nfold=cv.nfold, nrounds=cv.nround,
                 verbose = F, # early_stop_round=8, 
                 maximize=FALSE)
  min_train_rmse = min(mdcv$evaluation_log$train_rmse_mean)
  min_test_rmse = min(mdcv$evaluation_log$test_rmse_mean)
  
  all_param <- rbind(all_param, unlist(param)[-1])
  all_train_rmse <- c(all_train_rmse, min_train_rmse)
  all_test_rmse <- c(all_test_rmse, min_test_rmse)
}
all_param <- as.data.frame(as.numeric(all_param))
# then build the model with best parameter combination from the cross validation result.

# prediction
prediction <- predict(gbt, model.matrix( ~ ., data = test.data[, -dim(test.data)[2]]))
sum((prediction - log(test.data$SalePrice))^2)
sqrt(sum((prediction - log(test.data$SalePrice))^2)/dim(test.data)[1])
