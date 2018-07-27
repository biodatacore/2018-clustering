
# Library -----------------------------------------------------------------


library(dplyr)
library(tidyverse)




data <- read.csv('sample.csv')

data[data == 10] <- NA
data$NUT_SACH <- NULL

nutrientData <- data[, 215:427]
nutrientData$NUT_SACH <- NULL
nutrientData <- filter(nutrientData, complete.cases(nutrientData))

runKmeans <- function(data, cols, clusters){
  temp <- data[cols]
  cluster <- kmeans(temp, clusters)
  #print(cluster$centers)
  #print(cluster$withinss)
  cluster
}



kmeansAndPca <- function(data, cols, scale, numClust){
  using <- data[, which(names(data) %in% cols)]
  notUsing <- data[, -which(names(data) %in% cols)]
  if(scale){
    using <- as.data.frame(as.matrix(scale(using)))
  } else{
    using <- using
  }
  cluster <- runKmeans(using, cols, numClust)
  pca <- princomp(using)
  #View(pca$scores)
  tbl <- mutate(as.data.frame(pca$scores), grp = factor(cluster$cluster))
  tbl <- merge(tbl, notUsing)
  
  g <- ggplot(data = tbl) +
    geom_point(mapping = aes(x = Comp.1, y = Comp.2, color = grp))
  print(g)
  pca
}

goddamn <- function(data, cols, scale, numClust){
  using <- data[, which(names(data) %in% cols)]
  notUsing <- data[, -which(names(data) %in% cols)]
  cluster <- runKmeans(using, cols, numClust)
  pca <- princomp(using)
  #View(pca$scores)
  tbl <- mutate(as.data.frame(pca$scores), grp = factor(cluster$cluster))
  #tbl <- merge(tbl, notUsing)
  
  g <- ggplot(data = tbl) +
    geom_point(mapping = aes(x = Comp.1, y = Comp.2, color = grp))
  #print(g)
  #pca
}


head(data)
smh <- runKmeans(nutrientData, colnames(nutrientData), 3); smh

pca <- goddamn(nutrientData, colnames(nutrientData), FALSE, 4)





