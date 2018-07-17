colnames(md)
grep('points', colnames(md))
grep('numID', colnames(md))

eicOnly <- 12:534
dietOnly <- 536:661

pcauEicOnly4c <- kmeansAndPca(md[eicOnly], colnames(md)[eicOnly], FALSE, 4)
pcasEicOnly5c <- kmeansAndPca(md[eicOnly], colnames(md)[eicOnly], TRUE, 5)

pcauEicOnly4c

temp <- pcaContribution(pcasEicOnly5c)
head(temp)
temp <- temp[order(-temp$Comp.2), ]
head(temp)

pcaAndKmeans(md[eicOnly], colnames(md[eicOnly]), FALSE, 5)

tryClust('complete', 20)
tryClust('single', 20)
tryClust('average', 20)

#going back to roots lets try putting clusters together with classification of diet

forDiet <- md

forDiet <- addClassCol(forDiet, 'Dairy', c('skim', 'milk', 'cream'), TRUE)
forDiet <- addClassCol(forDiet, 'Ovo', c('eggs'), TRUE)
forDiet <- addClassCol(forDiet, 'Fish', c('tuna', 'DK_FISH', 'OTH_FISH', 'shrimp'), TRUE)
forDiet <- addClassCol(forDiet, 'Meat', c('CHIX_SK', 'CHIX_NO', 'bacon', 'hotdog', 'PROC_MTS', 'liver', 'hamb', 'SAND_BF', 'beef'), TRUE)
forDiet <- addClassCol(forDiet, 'RedMeat', c('bacon', 'hotdog', 'PROC_MTS', 'liver', 'hamb', 'SAND_BF', 'beef'), TRUE)

forDiet <- mutate(forDiet, classification = (Dairy != 'Low') + (Ovo != 'Low') * 2 + (Fish != 'Low') * 4 + (Meat != 'Low') * 8)

forDiet <- mutate(forDiet, type = ifelse(classification <= 1, 'vegan', ifelse(classification < 8, 'vegetarian', 'omnivore')))


eicCluster <- runKmeans(forDiet[eicOnly], colnames(forDiet)[eicOnly], 4)
complete.cases(forDiet[eicOnly])
dietCluster <- runKmeans(forDiet[dietOnly], colnames(forDiet)[dietOnly], 4)

grpPCA <- princomp(forDiet[eicOnly])
#View(grpPCA$scores)
pcaTable <- mutate(as.data.frame(grpPCA$scores), grp = factor(cluster$cluster), type = forDiet$type)

a <- ggplot(data = pcaTable) +
  geom_point(mapping = aes(x = Comp.1, y = Comp.2, color = grp, shape = type))

vfocused <- filter(pcaTable, type != 'omnivore')
b <- ggplot(data = vfocused) +
  geom_point(mapping = aes(x = Comp.1, y = Comp.2, color = grp, shape = type))

ggplot(data = vfocused) +
  geom_bar(mapping = aes(x = grp, fill = type))

grid.arrange(a, b, ncol = 2)


#try plotting and coloring by nuts

theseNuts <- as.data.frame(pcauEicOnly4c$scores)
theseNuts <- mutate(theseNuts, nuts = forDiet$nuts, grp = cluster$cluster)
View(cluster$cluster)


ggplot(data = theseNuts) +
  geom_point(mapping = aes(x = Comp.1, y = Comp.2, color = factor(nuts))) + 
  ggtitle("These Nuts")

colnames(md)





?pct_change
?kmeans



