library(gridExtra)

md <- readRDS('data/md.rds')

start_eic <- grep('mzid_231.176117_1.8182', colnames(md))
end_eic <- grep('mzid_395.241335_2.6439', colnames(md))
start_ffq <- grep('skim', colnames(md))
end_ffq <- grep('S_SHAKE', colnames(md))

eicOnly <- start_eic:end_eic
foodOnly <- start_ffq:end_ffq

pdf('plots/eicClusters.pdf', width = 7, height = 7)


pcauEicOnly4c <- kmeansAndPca(md[eicOnly], FALSE, 4) #WOAH
pcauEicOnly3c <- kmeansAndPca(md[eicOnly], FALSE, 3) #groups medium left two together most often
pcasEicOnly5c <- kmeansAndPca(md[eicOnly], TRUE, 5)

cont <- pcaContribution(pcauEicOnly4c)
head(cont)

temp <- pcaContribution(pcasEicOnly5c)
head(temp)
temp <- temp[order(-temp$Comp.2), ]
head(temp)

pcaAndKmeans(md[eicOnly], FALSE, 5)

tryClust(md[eicOnly], 'complete', 20)
tryClust(md[eicOnly], 'single', 20)
tryClust(md[eicOnly], 'average', 20)

#going back to roots lets try putting clusters together with classification of diet

forDiet <- md

forDiet <- addClassCol(forDiet, 'Dairy', c('skim', 'milk', 'cream'), TRUE)
forDiet <- addClassCol(forDiet, 'Ovo', c('eggs'), TRUE)
forDiet <- addClassCol(forDiet, 'Fish', c('tuna', 'DK_FISH', 'OTH_FISH', 'shrimp'), TRUE)
forDiet <- addClassCol(forDiet, 'Meat', c('CHIX_SK', 'CHIX_NO', 'bacon', 'hotdog', 'PROC_MTS', 'liver', 'hamb', 'SAND_BF', 'beef'), TRUE)
forDiet <- addClassCol(forDiet, 'RedMeat', c('bacon', 'hotdog', 'PROC_MTS', 'liver', 'hamb', 'SAND_BF', 'beef'), TRUE)

forDiet <- mutate(forDiet, classification = (Dairy != 'Low') + (Ovo != 'Low') * 2 + (Fish != 'Low') * 4 + (Meat != 'Low') * 8)

forDiet <- mutate(forDiet, type = ifelse(classification <= 1, 'vegan', ifelse(classification < 8, 'vegetarian', 'omnivore')))


eicCluster <- runKmeans(forDiet[eicOnly], 4)
foodCluster <- runKmeans(forDiet[foodOnly], 4)

grpPCA <- princomp(forDiet[eicOnly])
#View(grpPCA$scores)
pcaTable <- mutate(as.data.frame(grpPCA$scores), grp = factor(eicCluster$cluster), type = forDiet$type)

a <- ggplot(data = pcaTable) +
  geom_point(mapping = aes(x = Comp.1, y = Comp.2, color = grp, shape = type))

vfocused <- filter(pcaTable, type != 'omnivore')
b <- ggplot(data = vfocused) +
  geom_point(mapping = aes(x = Comp.1, y = Comp.2, color = grp, shape = type))

ggplot(data = vfocused) +
  geom_bar(mapping = aes(x = grp, fill = type))

grid.arrange(a, b, ncol = 2)


#try plotting and coloring by nuts

eicCluster <- runKmeans(forDiet[eicOnly], 4)
theseNuts <- as.data.frame(pcauEicOnly4c$scores)
theseNuts <- mutate(theseNuts, nuts = forDiet$nuts, grp = eicCluster$cluster)
#View(cluster$cluster)


ggplot(data = theseNuts) +
  geom_point(mapping = aes(x = Comp.1, y = Comp.2, color = factor(nuts))) + 
  ggtitle("These Nuts")

#colnames(md)

dev.off()



?pct_change
?kmeans



