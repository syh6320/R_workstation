
library(corrplot)
train_data <- read.csv("G:/kaggle/houseprice/data/train.csv")

head(train_data, n = 10)
str(train_data)
colnames(train_data)

contVar <- NULL
corr <- NULL
for(i in 1:dim(train_data)[2]) {
  if(is.numeric(train_data[, i])) {
    temcor <- cor(train_data[, i], train_data$SalePrice,
                  use = "pairwise.complete.obs")
    if(temcor > 0.5 | temcor < -0.5){
      # corr <- c(corr,temcor)
      contVar <- c(contVar, i)
    }
  }
}

correlation <- cor(train_data[,contVar], use = "pairwise.complete.obs")

corrplot(correlation, use = "pairwise.complete.obs")

for(i in 2:dim(train_data)[2]){
  if(is.factor(train_data[,i])){
    plot(train_data[,i], train_data$SalePrice, 
         xlab = names(train_data)[i],
         ylab = "sale price")
  }
}
