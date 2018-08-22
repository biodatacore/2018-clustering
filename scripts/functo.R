library(dplyr)
library(tidyverse)
library(tibble)
library(BRRR)
library(ggplot2)
library(varhandle)


# Clustering Functions --------------------------------------------------------


pcaAndKmeans <- function(data, scale, numClust, cols = colnames(data)){
  using <- data[, which(names(data) %in% cols)]
  if(scale){
    using <- as.data.frame(as.matrix(scale(using)))
  }
  
  pca <- princomp(using)
  scores <- as.data.frame(pca$scores)
  cluster <- runKmeans(scores, numClust, cols = c('Comp.1', 'Comp.2', 'Comp.3', 'Comp.4'))
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
  blank <- c('dairypt', 'fruitpt', 'vegpt', 'eggspt', 'meatspt', 'breadspt', 'bevpt', 'sweetspt', 'otherspt', 'fatfpt', 
             'fatbpt', 'oilpt', 'mpt', 'sugpt', 'cerpt', 'fl', 'cer', 'oil')
  
  data[, 42:174] <- ffqData
  data <- data[ , -which(names(data) %in% blank)]
  data$AGE8 <- NULL
  data
}

runPcor <- function(data, array){ #runs pcor between all the columns in data and array with CVRs
  result <- numeric()
  for (rix in 1:ncol(data)) {
    col_name <- colnames(data)[rix]
    
    # from https://en.wikipedia.org/wiki/Partial_correlation#Using_linear_regression
    x <- md[datCols][[col_name]]
    y <- array
    plCol <- md$plate
    ageCol <- md$AGE8
    sexCol <- md$SEX
    bmiCol <- md$BMI8
    bpCol <- md$SBP8
    smkCol <- md$CURRSMK8
    diabCol <- md$curr_diab8
    tcCol <- md$TC8
    
    lm_x <- lm(x ~ plCol + ageCol + sexCol + bmiCol + bpCol + smkCol + diabCol + tcCol)
    lm_y <- lm(y ~ plCol + ageCol + sexCol + bmiCol + bpCol + smkCol + diabCol + tcCol)
    
    residuals_x <- residuals(lm_x)
    residuals_y <- residuals(lm_y)
    pcor <- cor(residuals_x, residuals_y, method = 'spearman')
    result[rix] <- pcor
  }
  result
}


runBCoeffs <- function(data, arrayName){
  posi <- 1
  result <- numeric()
  for(i in eicOnly){ # written super specifically for a data frame with all columns except the last being eicosanoids and the last being vegan, vegetarian, or pescetarian
    sformula <- paste(colnames(data)[i], '~', arrayName, '+ AGE8 + SEX + BMI8 + SBP8 + CURRSMK8 + curr_diab8 + TC8') #not currently adjusting for plate because it behaves strangely
    result[posi] <- lm(sformula, data = data)$coefficients[arrayName]
    posi <- posi + 1
  }
  result
}


computeHEI <- function(dat){ #only takes raw framingham ffq data as argument
  temp <- dat
  totcal <- dat$totcal
  #print(totcal)
  temp <- mutate(temp, totFruit = 1/7*rais + 1/2*prun + 3/4*ban + 9/8*cant + H20MEL + 2*apple + A_J + 1/3*orang + O_J + 1/2*grfrt + GRFRT_J +
                   OTH_F_J + 1/2*straw + 1/2*blue + 1/2*PEACH_CN)
  temp <- mutate(temp, totWFruit = 1/7*rais + ban + cant + H20MEL + apple + orang + grfrt + 1/2*straw + 1/2*blue + 1/2*PEACH_CN)
  temp <- mutate(temp, totVeg = tom + TOM_J + 1/2*TOM_S + 1/16*CHIL_SCE + 1/2*tofu + 1/2*ST_BEANS + 1/2*broc + 1/2*cabb + 1/2*caul +
                   1/2*brusl + 1/2*CARROT_R + 1/2*CARROT_C + 1/2*corn + 1/2*peas + 1/2*MIX_VEG + 1/2*beans + 1/2*YEL_SQS + 1/2*zuke + 1/2*yams +
                   1/2*SPIN_CKD + 1/2*SPIN_RAW + 1/2*kale + ICE_LET + ROM_LET + celery + 1/2*beet + 1/2*ALF_SPRT + garlic)
  temp <- mutate(temp, totGrn = 1/2*ST_BEANS + 1/2*broc + 1/2*cabb + 1/2*peas + 1/2*MIX_VEG + 1/2*beans + 1/2*zuke + 1/2*SPIN_CKD + 
                   1/2*SPIN_RAW + 1/2*kale + ROM_LET)
  temp <- mutate(temp, totGrain = 8* (CKD_OATS + DK_BR + BR_RICE + WH_RICE + grains) )
  temp <- mutate(temp, totDairy = skim + milk + 1/16*cream + 1/16*SOUR_CR + 1/48*COF_WHT + 1/2*sherb + 1/2*ICE_CR + yog + 1/2*COT_CH +
                   1/8*CR_CH + 1/8*OTH_CH + margarin + bu)
  temp <- mutate(temp, totProt = eggs + 5*CHIX_SK + 5*CHIX_NO + bacon + hotdog + PROC_MTS + 3.5*liver + 4*hamb + SAND_BF +
                   5*beef + 3.5*tuna + 3.5*DK_FISH + 4*OTH_FISH + shrimp + 3.5*tofu + nuts)
  temp <- mutate(temp, fishProt = 3.5*tuna + 3.5*DK_FISH + 4*OTH_FISH + shrimp + 3.5*tofu + nuts)
  temp <- mutate(temp, refGrain = 8* (COLD_CER + CKD_CER + WH_BR + ENG_MUFF + muff + WH_RICE + pasta + pancake + 1/2*FF_POT + POT_CHIP + crax + pizza
                                      + COOX_HOM + COOX_COM + brownie + donut + CAKE_HOM + CAKE_COM + S_ROLL_H + S_ROLL_C + PIE_HOME + PIE_COMM +
                                        popc)) #counting deserts like cookies as refined grain
  temp <- mutate(temp, totUFA = NUT_F161 + NUT_F181 + NUT_F183 + NUT_F201 + NUT_F204 + NUT_F205 + NUT_F225 + NUT_F226)
  temp <- mutate(temp, totFA = NUT_F40 + NUT_F60 + NUT_F80 + NUT_F100 + NUT_F120 + NUT_F140 + NUT_F160 + NUT_F180)
  temp <- mutate(temp, fatRatio = totUFA/totFA)
  temp <- mutate(temp, sodium = NUT_SODIUM/1000)
  temp <- mutate(temp, fatCal = 9*totFA)
  vegScore <- heiHelper(totcal, temp$totVeg, 5, 1.1)
  beanScore <- heiHelper(totcal, temp$totGrn, 5, 1.1)
  fruitScore <- heiHelper(totcal, temp$totFruit, 5, .8)
  wFruitScore <- heiHelper(totcal, temp$totWFruit, 5, .4)
  grainScore <- heiHelper(totcal, temp$totGrain, 10, 1.5)
  dairyScore <- heiHelper(totcal, temp$totDairy, 10, 1.3)
  proteinScore <- heiHelper(totcal, temp$totProt, 5, 2.5)
  fishProtScore <- heiHelper(totcal, temp$fishProt, 5, .8)
  fatScore <- ifelse(temp$fatRatio >= 2.5, 10, ifelse(temp$fatRatio <= 1.2, 0, 10 * ((temp$fatRatio - 1.2)/(2.5 - 1.2))))
  sodRatio <- temp$sodium / totcal; sodScore <- ifelse(sodRatio <= 1.1, 10, ifelse(sodRatio >= 2.0, 0, 10-(10*(sodRatio - 1.1)/(2.0 - 1.1))))
  refGrnRatio <- temp$refGrain / (totcal / 1000); refGrnScore <- ifelse(refGrnRatio <= 1.8, 10, ifelse(refGrnRatio >= 4.3, 0, 10-(10*(refGrnRatio-1.8)/(4.3-1.8))))             
  dat <- mutate(dat, HEI = vegScore + beanScore + fruitScore + wFruitScore + grainScore + dairyScore + proteinScore + fishProtScore + fatScore + sodScore + refGrnScore)
  dat
}

heiHelper <- function(totcal, array, max, mult, scale = 1000){
  ratio <- array / (totcal/scale)
  score <- ifelse(max * ratio / mult > max, max, max * ratio / mult)
  #print(head(ratio))
  #print(head(totcal))
  #print('lmfao')
  score
}

?min
?length
























