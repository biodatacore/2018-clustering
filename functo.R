pcaAndKmeans <- function(data, cols, scale, numClust){
  using <- data[, which(names(data) %in% cols)]
  if(scale){
    using <- as.data.frame(as.matrix(scale(using)))
  }
  
  pca <- princomp(using)
  scores <- as.data.frame(pca$scores)
  cluster <- runKmeans(scores, c('Comp.1', 'Comp.2', 'Comp.3', 'Comp.4'), numClust)
  #View(pca$scores)
  tbl <- mutate(scores, grp = factor(cluster$cluster))
  #tbl <- merge(tbl, notUsing)
  
  g <- ggplot(data = tbl) +
    geom_point(mapping = aes(x = Comp.1, y = Comp.2, color = grp))
  print(g)
  #pca
}

runKmeans <- function(data, cols, clusters){
  temp <- data[cols]
  cluster <- kmeans(temp, clusters)
  #print(cluster$centers)
  #print(cluster$withinss)
  cluster
}

kmeansAndPca <- function(data, cols, scale, numClust){
  using <- data[, which(names(data) %in% cols)]
  if(scale){
    using <- as.data.frame(as.matrix(scale(using)))
  } else{
    using <- using
  }
  cluster <- runKmeans(using, cols, numClust)
  pca <- princomp(using)
  #View(pca$scores)
  tbl <- mutate(as.data.frame(pca$scores), grp = factor(cluster$cluster))
  #tbl <- merge(tbl, notUsing)
  
  g <- ggplot(data = tbl) +
    geom_point(mapping = aes(x = Comp.1, y = Comp.2, color = grp))
  print(g)
  pca
}

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
  temp <- TRUE
  for(i in critList){
    temp <- temp & data[[i]]<= ifelse(transformed, .1, 2)
  }
  data[[colName]] <- ifelse(temp, 'Low', 'Normal')
  data
}

ffqToDaily <- function(num){
  switch(as.character(num), '1' = 1/60, '2' = 1/15, '3' = 1/7, '4' = 3/7, '5' = 5.5/7, '6' = 1, '7' = 2.5, '8' = 4.5, '9' = 7)
}

pcaContribution <- function(pca){
  load <- with(pca, unclass(loadings))
  aload <- abs(load)
  a <- sweep(aload, 2, colSums(aload), "/")
  a <- as.data.frame(a[, 1 : min(4, length(a[1,]))])
  a[order(-a$Comp.1), ]
}

#this function only takes the raw framingham ffq data as an argument
transformFFQ <- function(data){
  ffqData <- data[, 42:174]
  ffqData <- unfactor(ffqData)
  blank <- c('dairypt', 'fruitpt', 'vegpt', 'eggspt', 'meatspt', 'breadspt', 'bevpt', 'sweetspt', 'otherspt', 'fatfpt', 
             'fatbpt', 'oilpt', 'mpt', 'sugpt', 'cerpt', 'fl', 'cer', 'oil')
  ffqData <- ffqData[ , -which(names(ffqData) %in% blank)]
  
  print(ffqData$tofu)
  
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
  
  print(ffqData$tofu)
  
  ffqData$ID = data$id
  ffqData
}

tryClust <- function(method, cut){
  clusters <- hclust(dist(scale(eicOnly)), method = method);
  plot(clusters);
  clustercut <- cutree(clusters, cut);
  table(clustercut)
}

?min
?length
























