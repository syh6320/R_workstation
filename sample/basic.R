train_data <- read.csv("H:/kaggle/houseprice/data/train.csv")
summary(train_data)

which(is.na(train_data$Alley))
sapply(train_data, function(x) {table(x)})

density(train_data$LotArea)
head(train_data$LotArea, n = 5)
plot(density(train_data$LotArea))

typeof(train_data$OverallQual)
barplot(table(train_data$OverallQual))


library(lattice)
bwplot(OverallQual ~ SalePrice, data = train_data)
bwplot(Neighborhood ~ SalePrice, data = train_data)
bwplot(LotArea ~ SalePrice, data = train_data)

correlation <- NULL
correlation <- sapply(train_data, function(col){
  if(is.numeric(col)){
    c(correlation, cor(col,train_data$SalePrice, use = "pairwise.complete.obs"))
    }
  })

library(corrplot)
cor(train_data, use = "pairwise.complete.obs")
