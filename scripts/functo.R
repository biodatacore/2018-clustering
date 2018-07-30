library(dplyr)
library(tidyverse)
library(factoextra)
#library(cluster)
library(tibble)
library(gridExtra)
library(varhandle)
library(superheat)
library(BRRR)
library(ggplot2)
library(corrplot)
library(ppcor)


# Clustering Functions --------------------------------------------------------


pcaAndKmeans <- function(data, scale, numClust, cols = colnames(data)){
  using <- data[, which(names(data) %in% cols)]
  if(scale){
    using <- as.data.frame(as.matrix(scale(using)))
  }
  
  pca <- princomp(using)
  scores <- as.data.frame(pca$scores)
  cluster <- runKmeans(scores, numClust, cols = c('Comp.1', 'Comp.2', 'Comp.3', 'Comp.4'),)
  #View(pca$scores)
  tbl <- mutate(scores, grp = factor(cluster$cluster))
  #tbl <- merge(tbl, notUsing)
  
  g <- ggplot(data = tbl) +
    geom_point(mapping = aes(x = Comp.1, y = Comp.2, color = grp))
  print(g)
  #pca
}

runKmeans <- function(data, clusters, cols = colnames(data)){
  temp <- data[, cols]
  cluster <- kmeans(temp, clusters)
  #print(cluster$centers)
  #print(cluster$withinss)
  cluster
}

kmeansAndPca <- function(data, scale, numClust, cols = colnames(data)){
  using <- data[, which(names(data) %in% cols)]
  if(scale){
    using <- as.data.frame(as.matrix(scale(using)))
  } else{
    using <- using
  }
  cluster <- runKmeans(using, numClust, cols = cols)
  pca <- princomp(using)
  #View(pca$scores)
  tbl <- mutate(as.data.frame(pca$scores), grp = factor(cluster$cluster))
  #tbl <- merge(tbl, notUsing)
  
  g <- ggplot(data = tbl) +
    geom_point(mapping = aes(x = Comp.1, y = Comp.2, color = grp))
  print(g)
  c(pca, cluster)
}

pcaContribution <- function(pca){
  load <- with(pca, unclass(loadings))
  aload <- abs(load)
  a <- sweep(aload, 2, colSums(aload), "/")
  a <- as.data.frame(a[, 1 : min(4, length(a[1,]))])
  a[order(-a$Comp.1), ]
}

tryClust <- function(data, method, cut){
  clusters <- hclust(dist(scale(data)), method = method);
  plot(clusters);
  clustercut <- cutree(clusters, cut);
  table(clustercut)
}

# Plotting Assistance -----------------------------------------------------


vs_Afat <- function(data, col) {
  data[[col]] <- as.factor(data[[col]])
  
  p <-
    ggplot(data = data) +
    geom_boxplot(mapping = aes_string(x = col, y = 'NUT_AFAT'))
  
  print(p)
}

vs_Class <- function(colName){
  ggplot(data = classDiet) +
    geom_boxplot(mapping = aes_string(x = 'classification', y = colName))
}

addClassCol <- function(data, colName, critList, transformed = FALSE){
  low <- TRUE
  allNorm <- TRUE
  for(i in critList){
    low <- low & data[[i]] <= ifelse(transformed, .1, 2)
    allNorm <- allNorm & data[[i]] <= ifelse(transformed, 2.5 , 7)
  }
  data[[colName]] <- ifelse(low, 'Low', ifelse(allNorm, 'Normal', 'High'))
  data
}


# FFQ cleaning ------------------------------------------------------------



#this function only takes the raw framingham ffq data as an argument
transformFFQ <- function(data){
  ffqData <- data[, 42:174]
  ffqData <- unfactor(ffqData)
  blank <- c('dairypt', 'fruitpt', 'vegpt', 'eggspt', 'meatspt', 'breadspt', 'bevpt', 'sweetspt', 'otherspt', 'fatfpt', 
             'fatbpt', 'oilpt', 'mpt', 'sugpt', 'cerpt', 'fl', 'cer', 'oil')
  ffqData <- ffqData[ , -which(names(ffqData) %in% blank)]
  
  #print(ffqData$tofu)
  
  ffqData[ffqData == 1] <- 1/60
  ffqData[ffqData == 2] <- 1/15
  ffqData[ffqData == 3] <- 1/7
  ffqData[ffqData == 4] <- 3/7
  ffqData[ffqData == 5] <- 5.5/7
  ffqData[ffqData == 6] <- 1
  ffqData[ffqData == 7] <- 2.5
  ffqData[ffqData == 8] <- 4.5
  ffqData[ffqData == 9] <- 7
  ffqData[ffqData == 10] <- NA
  
  #print(ffqData$tofu)
  
  ffqData$ID = data$id
  ffqData
}



?min
?length























