
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

temp <- data
head(temp)
temp <- mutate(temp, totFruit = 1/7*rais + 1/2*prun + 3/4*ban + 9/8*cant + H20MEL + 2*apple + A_J + 1/3*orang + O_J + 1/2*grfrt + GRFRT_J +
                 OTH_F_J + 1/2*straw + 1/2*blue + 1/2*PEACH_CN)
fruits <- select(temp, rais, prun, ban, cant, H20MEL, apple, A_J, orang, O_J, grfrt, GRFRT_J, OTH_F_J, straw, blue, PEACH_CN)
fruits







datamash <- md
datamash <- mutate(fishCor, totFish = tuna + DK_FISH + OTH_FISH)
grpName <- 'totFish'


start_eic <- grep('mzid_231.176117_1.8182', colnames(datamash))
end_eic <- grep('mzid_395.241335_2.6439', colnames(datamash))
eicOnly <- start_eic:end_eic

results <- as.data.frame(matrix(nrow = 10, ncol = 3)); rowct <- 1
results[1:2 ,]
colnames(results) <- c('Eic', 'Food Group', 'Correlation')

for(i in eicOnly){
  val <- round(cor(datamash[i], datamash[[grpName]], method = 'spearman'), digits = 4)
  if(val > .2){
    print(paste('logt', colnames(datamash)[i], '(col', i, ') and', grpName, 'have cor', val, sep = ' '))
    results[rowct, ] <- c(colnames(datamash)[i], grpName, val); rowct <- rowct + 1 
  }
}
results
