library(dplyr)
library(tidyverse)
library(factoextra)
#library(cluster)
library(tibble)
library(gridExtra)

getwd()
setwd('/Users/krao/Dropbox/2018 Applied Bioinformatics Work/Personal Folders/Kevin Rao')

data <- read.csv('sample.csv')
colnames(data)
#View(data)

trimMilk <- select(data, skim, milk, ca, cadur, cad)
validMilk <- filter(trimMilk, skim > 0, skim < 10, milk > 0, milk < 10)

#View(trimMilk)
#View(validMilk)

ggplot(data = validMilk, mapping = aes(x = skim + milk, y = cad)) +
  geom_point(na.rm = TRUE)

validMilk <- mutate(validMilk, mCons = skim + milk)
validMilk$mCons <- factor(validMilk$mCons)
validMilk$cad <- factor(validMilk$cad)
validMilk$ca <- factor(validMilk$ca)
ggplot(data = filter(validMilk, !is.na(ca))) +
  geom_bar(mapping = aes(x = ca, fill = mCons))
ggplot(data = filter(validMilk, cad != 5)) +
  geom_boxplot(mapping = aes(x = cad, y = mCons))
#no correlation

data[data == 10] <- NA
data$NUT_SACH <- NULL

data <- mutate(data, totMeat = CHIX_SK + CHIX_NO + bacon + hotdog + PROC_MTS + liver + hamb + SAND_BF + beef)

trimMeat <- select(data, bu, NUT_AFAT, eggs, CHIX_SK, CHIX_NO, bacon, hotdog, eggspt, PROC_MTS, liver, hamb, SAND_BF, beef, tuna, DK_FISH, OTH_FISH, shrimp)

#View(data$totMeat)

columns_to_factor <- c('bacon', 'hamb', 'bu')

trimMeat %>%
  mutate_at(columns_to_factor, as.factor)


vs_Afat(trimMeat, 'bacon')
vs_Afat(trimMeat, 'hamb')
vs_Afat(trimMeat, 'bu')


#trying to identify what vegans use as protein substitute
data$tofu <- factor(data$tofu)

ggplot(data = data) +
  geom_point(mapping = aes(x = tofu, y = NUT_AFAT))

data$P_BU <- factor(data$P_BU)

ggplot(data = data) +
  geom_point(mapping = aes(x = P_BU, y = NUT_AFAT, color = tofu, alpha = 1/1000))
#the one guy with 9 on peanut butter and almost zero afat is probably vegan also high tofu


trimSpinach <- select(mutate(data, totSpinach = SPIN_CKD + SPIN_RAW), totSpinach, P_BU, tofu, NUT_AFAT)
#View(trimSpinach)

trimSpinach$totSpinach <- factor(trimSpinach$totSpinach)
ggplot(data = trimSpinach) +
  geom_point(mapping = aes(x = totSpinach, y = NUT_AFAT, color = tofu))

classDiet <- select(data, skim, milk, cream, eggs, CHIX_SK, CHIX_NO, bacon, hotdog, PROC_MTS, liver, hamb, SAND_BF, beef, tuna, DK_FISH, OTH_FISH, shrimp)
classDiet <- data

classDiet <- addClassCol(classDiet, 'Dairy', c('skim', 'milk', 'cream'))
classDiet <- addClassCol(classDiet, 'Ovo', c('eggs'))
classDiet <- addClassCol(classDiet, 'Fish', c('tuna', 'DK_FISH', 'OTH_FISH', 'shrimp'))
classDiet <- addClassCol(classDiet, 'Meat', c('CHIX_SK', 'CHIX_NO', 'bacon', 'hotdog', 'PROC_MTS', 'liver', 'hamb', 'SAND_BF', 'beef'))

classDiet <- mutate(classDiet, classification = (Dairy == 'Normal') + (Ovo == 'Normal') * 2 + (Fish == 'Normal') * 4 + (Meat == 'Normal') * 8)

classDiet$classification <- factor(classDiet$classification)

ggplot(data = classDiet) +
  geom_bar(mapping = aes(x = classification))

byClass <- group_by(classDiet, classification)
avgAge <- summarize(byClass, avg = mean(AGE8))

ggplot(data = avgAge) +
  geom_point(mapping = aes(x = classification, y = avg))


afat <- vs_Class('NUT_AFAT')
copper <- vs_Class('NUT_CU')
cals <- vs_Class('totcal')
chol <- vs_Class('NUT_CHOLINE')
vs_Class('NUT_CHOL')

grid.arrange(afat, copper, cals, chol, ncol = 2)


ggplot(data = classDiet) +
  geom_boxplot(mapping = aes(x = classification, y = NUT_AFAT))


ggplot(data = classDiet) +
  geom_point(mapping = aes(x = NUT_AFAT, y = totcal, color = classification, alpha = 1/10))

#there are too many classifications of animal products in diet so
#we remove dairy as a factor as it appears to be the least impactful

tempClass <- mutate(classDiet, classification = (Ovo == 'Normal') + (Fish == 'Normal') * 2 + (Meat == 'Normal') * 4)
tempClass$classification <- factor(tempClass$classification)

ggplot(data = tempClass) +
  geom_point(mapping = aes(x = NUT_AFAT, y = totcal, color = classification, alpha = 1/10)) +
  facet_wrap(~ classification, nrow = 2)

ggplot(data = classDiet) +
  geom_point(mapping = aes(x = totMeat, y = NUT_AFAT, color = classification))





#K means clustering ========================================================

nutrientData <- data[, 215:427]
snake <- as.data.frame(as.matrix(scale(nutrientData)))
nutrientData$NUT_SACH <- NULL
nutrientData <- filter(nutrientData, complete.cases(nutrientData))




#unscaled k means

pcaunut4c <- kmeansAndPca(nutrientData, colnames(nutrientData), FALSE, 4)
#pca$loadings

fviz_eig(pca)
summary(pca)
load <- with(pca, unclass(loadings))
aload <-
  pca %>%
  with(unclass(loadings)) %>%
  abs
proportions <- as.data.frame(as.matrix(sweep(aload, 2, colSums(aload), "/")))
eigs <- pca$sdev^2
rbind(
  SD = sqrt(eigs),
  Proportion = eigs[1]/sum(eigs),
  Cumulative = cumsum(eigs)/sum(eigs))




#k means with nutrientdata scaled
pcasnut4c <- kmeansAndPca(nutrientData, colnames(nutrientData), TRUE, 4)

summary(pcale)
eigs <- pcale$sdev^2
rbind(
  SD = sqrt(eigs),
  Proportion = eigs/sum(eigs),
  Cumulative = cumsum(eigs)/sum(eigs))



#kmeans removing the first column of cal

noCal <- nutrientData
noCal$totcal <- NULL

pcaunoCal4c <- kmeansAndPca(noCal, colnames(noCal), FALSE, 4)

summary(pNoCal)
eigs <- pNoCal$sdev^2
rbind(
  SD = sqrt(eigs),
  Proportion = eigs/sum(eigs),
  Cumulative = cumsum(eigs)/sum(eigs))

#kmeans removing cal and scaling

noCal <- nutrientData
noCal$totcal <- NULL

pcasnoCal4c <- kmeansAndPca(noCal, colnames(noCal), TRUE, 4)

summary(pcale)
eigs <- pcale$sdev^2
rbind(
  SD = sqrt(eigs),
  Proportion = eigs/sum(eigs),
  Cumulative = cumsum(eigs)/sum(eigs))



#correlation matrix for nutrientdata

d2 <- nutrientData %>%
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1)

#View(cor(as.matrix(nutrientData)))

filter(d2, value > .9 & value != 1)

#list of variables to remove due to >.9 correlation
highCor = c('NUT_PROT', 'NUT_APROT', 'NUT_TFAT', 'NUT_GL', 'NUT_K',

             #USDA Totals
             'NUT_TOTUSFLAVAN3OL', 'NUT_TOTUSFLAVONOL', 'NUT_TOTUSFLAV', 'NUT_TOTUSPOLYFLAV', 'NUT_TOTUSFLAVONE', 'NUT_TOTUSANTH', 'NUT_TOTUSFLAVANONE',

             #amino acids
             'NUT_METH', 'NUT_TRYPTO', 'NUT_GLUT', 'NUT_SER', 'NUT_HIST', 'NUT_PRO', 'NUT_ALA',
             'NUT_GLY', 'NUT_ISO', 'NUT_THR', 'NUT_LYS', 'NUT_CYS', 'NUT_PHENYL', 'NUT_TYRO',
             'NUT_VAL', 'NUT_ARG', 'NUT_ASP', 'NUT_LEU', 'NUT_THR',

             #flavan-3-ol
             'NUT_UEGC', 'NUT_UTF', 'NUT_UTFG', 'NUT_UTF3G', 'NUT_UTFDG', 'NUT_UTRG', 'NUT_UEGCG'
             )

highCor = c('NUT_PROT', 'NUT_APROT', 'NUT_TFAT', 'NUT_GL', 'NUT_K',

             #USDA Totals
             'NUT_TOTUSFLAVAN3OL', 'NUT_TOTUSFLAVONOL', 'NUT_TOTUSFLAV', 'NUT_TOTUSPOLYFLAV', 'NUT_TOTUSFLAVONE', 'NUT_TOTUSANTH', 'NUT_TOTUSFLAVANONE'
)

#remove protien and animal protein since all amino acids are there
#remove total fats because animal fats trans fats and all kinds of fatty acids fall under it
#remove glycemic load because its measured under carbo
#remove potassium in favor for potassium without supplements
#temporarily removing all the amino acids to unblock data (remember to put them back in) (glutamine and asparagine are missing from the data)
#temporarily removing all flavan-3-ol molecules (remember to replace)
#removing TOTUSFLAVAN3OL, TOTUSFLAVONOL, TOTUSFLAV, TOTUSPOLYFLAV, TOTUSFLAVONE, TOTUSANTH, TOTUSFLAVANONE
  #since they're simply the totals other molecules (3-ol, flavonols, flavonoids, theaflavin and polymers proanthocyanidins, flavanon, anthocyanidins, flavones)

noCorData <- nutrientData[ , -which(names(nutrientData) %in% highCor)]
head(noCorData)

d2 <- noCorData %>%
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1)

filter(d2, value > .9 & value != 1)

kmeansAndPca(noCorData, colnames(noCorData), FALSE, 4)
kmeansAndPca(noCorData, colnames(noCorData), TRUE, 4)

summary(pca)
eigs <- pca$sdev^2
rbind(
  SD = sqrt(eigs),
  Proportion = eigs/sum(eigs),
  Cumulative = cumsum(eigs)/sum(eigs))

load <- with(pca, unclass(loadings))
aload <- abs(load)
proportions <- as.data.frame(as.matrix(sweep(aload, 2, colSums(aload), "/")))




#Hierarchical Clustering ===============================================

clusters <- hclust(dist(scale(noCorData)), method = "single"); plot(clusters)

clustercut <- cutree(clusters, 20)
table(clustercut)

#yea that failed pretty quickly


correlation <- as.data.frame(cor(noCorData, method = "spearman"))
correlation
kmeansAndPca(correlation, colnames(correlation), FALSE, 4)

#divisive hierarchical

#plot(diana(noCorData))

#also not interesting it seems like theres not a whole lot of large cluster merging just keep adding single or small groups to way bigger ones


#lets try using kmeans and pca but labelling the people on the graph by what kind of diet (vegan, vegetarian, etc) they have

classDiet <- data

classDiet <- addClassCol(classDiet, 'Dairy', c('skim', 'milk', 'cream'))
classDiet <- addClassCol(classDiet, 'Ovo', c('eggs'))
classDiet <- addClassCol(classDiet, 'Fish', c('tuna', 'DK_FISH', 'OTH_FISH', 'shrimp'))
classDiet <- addClassCol(classDiet, 'Meat', c('CHIX_SK', 'CHIX_NO', 'bacon', 'hotdog', 'PROC_MTS', 'liver', 'hamb', 'SAND_BF', 'beef'))

classDiet <- mutate(classDiet, classification = (Dairy == 'Normal') + (Ovo == 'Normal') * 2 + (Fish == 'Normal') * 4 + (Meat == 'Normal') * 8)

classDiet <- mutate(classDiet, type = ifelse(classification <= 1, 'vegan', ifelse(classification < 8, 'vegetarian', 'omnivore')))

nutAndClass <- mutate(data[, 215:427], type = classDiet$type)

head(nutAndClass)
nutAndClass <- filter(nutAndClass, complete.cases(nutAndClass))

using <- nutAndClass[, -which(names(nutAndClass) %in% c('type'))]
notUsing <- nutAndClass[, which(names(nutAndClass) %in% c('type'))]
using <- as.data.frame(scale(using))

cluster <- runKmeans(using, colnames(using), 4)
nutClassGrp <- mutate(nutAndClass, grp = factor(cluster$cluster))

grpPCA <- princomp(using)
pcaTable <- mutate(as.data.frame(grpPCA$scores), grp = nutClassGrp$grp, type = nutClassGrp$type)

kmeansAndPca(nutAndClass, colnames(nutAndClass), FALSE, 4)

a <- ggplot(data = pcaTable) +
  geom_point(mapping = aes(x = Comp.1, y = Comp.2, color = grp, shape = type))

vfocused <- filter(pcaTable, type != 'omnivore')
b <- ggplot(data = vfocused) +
  geom_point(mapping = aes(x = Comp.1, y = Comp.2, color = grp, shape = type))

ggplot(data = vfocused) +
  geom_bar(mapping = aes(x = grp, fill = type))

grid.arrange(a, b, ncol = 2)

#looks like pretty much all vegans and vegetarians are in the same two groups

#lets try running kmeans on pca


hold <- pcaAndKmeans(nutrientData, colnames(nutrientData), TRUE, 4)

ggplot(data = data) +
  geom_bar(mapping = aes(x = hamb))




#converting ffq data to daily doses

#grep('skim', colnames(data))
#grep('S_SHAKE', colnames(data))
ffqData <- data[, 42:198]
head(ffqData)
blank <- c('dairypt', 'fruitpt', 'vegpt', 'eggspt', 'meatspt', 'breadspt', 'bevpt', 'sweetspt', 'otherspt', 'fatfpt', 
           'fatbpt', 'oilpt', 'mpt', 'sugpt', 'cerpt', 'fl', 'cer', 'oil')
ffqData <- ffqData[ , -which(names(ffqData) %in% blank)]
head(ffqData)

servings <- colnames(ffqData)[1:126]

head(ffqData)
temp <- lapply(ffqData, ffqToDaily)
temp <- as.data.frame(sapply(as.matrix(ffqData), ffqToDaily))
View(temp)

sapply(matrix(c(1,2,3,4,5,6,7,8,9), ncol = 3), function(num){num * 2})

lapply(c(1,2,3,4,5), function(num){ num * 2 })

#these columns arent the simple ffqToDaily: 'vf', 'ffh', 'ffa', 'sugar', 'oil
#these columns are booleans 'fb', 'fm', 'fvo', 'fsh', 'bb', 'bm', 'bvo', 'bsh', 'bl', 'mn', 'ms', 'mls', 'mt', 'msp', mlt'

#fuck it im just going to only take columns that have values 0-9

ffqData <- data[, 42:174]
blank <- c('dairypt', 'fruitpt', 'vegpt', 'eggspt', 'meatspt', 'breadspt', 'bevpt', 'sweetspt', 'otherspt', 'fatfpt', 
                                    'fatbpt', 'oilpt', 'mpt', 'sugpt', 'cerpt', 'fl', 'cer', 'oil')
ffqData <- ffqData[ , -which(names(ffqData) %in% blank)]

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

head(ffqData)


#kmeans on the ffq data

complete_ffq <- filter(ffqData, complete.cases(ffqData))

pcauffq4c <- kmeansAndPca(complete_ffq, colnames(complete_ffq), FALSE, 4)
pcasffq4c <- kmeansAndPca(complete_ffq, colnames(complete_ffq), TRUE, 4)


#looks kind of interesting lets look at the scores
 
cont <- as.data.frame(pcaContribution(pcauffq4c))
cont <- cont[order(-cont$Comp.1), ]
head(cont)
cont <- cont[order(-cont$Comp.2), ]
head(cont)
#P_SHAKE, S_SHAKE, coff seem to be biggest variablility accounters

ggplot(data = complete_ffq) +
  geom_point(mapping = aes(x = coff, y = P_SHAKE, alpha = 1/10))

#View(pcauffq5c$loadings[, 1:3])

#hierarchical on ffq

clusters <- hclust(dist((complete_ffq)), method = "single"); plot(clusters)
clustercut <- cutree(clusters, 20)
table(clustercut)

#yea hierarchical is still fucked, clustering on nonscaled with complete method is the least uninteresting

pcaContribution(kmeansAndPca(complete_ffq, c('P_SHAKE', 'S_SHAKE', 'coff'), FALSE, 4))



?factor
?summarize
?geom_bar

?mutate
?mutate_all
?is.na

?group_by
?with
?kmeans
?select

?prcomp
?princomp
?cor

?hclust
?dist
?cor

?scale
?merge

?lapply
?apply
?grid.arrange


