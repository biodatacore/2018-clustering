numClust <- 4

pcauEicOnly4c <- kmeansAndPca(md[eicOnly], FALSE, numClust) #WOAH

tbl <- mutate(as.data.frame(pcauEicOnly4c$scores), grp = factor(pcauEicOnly4c$cluster))
#tbl <- merge(tbl, notUsing)



eicCluster <- pcauEicOnly4c$cluster
foodCluster <- runKmeans(md[foodOnly], numClust) #when you figure out how ot fill in NA for ffq data do a tile to see which clusters ppl are in
eicClustered <- mutate(md, eicGrp = factor(eicCluster))
head(eicClustered)

eicClustered <- addClassCol(eicClustered, 'Dairy', c('skim', 'milk', 'cream'), TRUE)
eicClustered <- addClassCol(eicClustered, 'Ovo', c('eggs'), TRUE)
eicClustered <- addClassCol(eicClustered, 'Fish', c('tuna', 'DK_FISH', 'OTH_FISH', 'shrimp'), TRUE)
eicClustered <- addClassCol(eicClustered, 'Meat', c('CHIX_SK', 'CHIX_NO', 'bacon', 'hotdog', 'PROC_MTS', 'liver', 'hamb', 'SAND_BF', 'beef'), TRUE)
eicClustered <- addClassCol(eicClustered, 'RedMeat', c('bacon', 'hotdog', 'PROC_MTS', 'liver', 'hamb', 'SAND_BF', 'beef'), TRUE)


eicClustered <- mutate(eicClustered, classification = (Dairy != 'Low') + (Ovo != 'Low') * 2 + (Fish != 'Low') * 4 + (Meat != 'Low') * 8)

ggplot(data = eicClustered) +
  geom_bar(mapping = aes(x = classification, fill = eicGrp), position = 'fill')

pplPlot <- ggplot(data = tbl) +
  geom_point(mapping = aes(x = Comp.1, y = Comp.2, color = grp))
pplPlot

vsFish <- ggplot(data = eicClustered) +
  geom_bar(mapping = aes(x = eicGrp, fill = Fish), position = 'fill')
vsRed <- ggplot(data = eicClustered) +
  geom_bar(mapping = aes(x = eicGrp, fill = RedMeat), position = 'fill')
vsDairy <- ggplot(data = eicClustered) +
  geom_bar(mapping = aes(x = eicGrp, fill = Dairy), position = 'fill')
vsOvo <- ggplot(data = eicClustered) +
  geom_bar(mapping = aes(x = eicGrp, fill = Ovo), position = 'fill')
vsMeat <- ggplot(data = eicClustered) +
  geom_bar(mapping = aes(x = eicGrp, fill = Meat), position = 'fill')

grid.arrange(pplPlot, vsFish, vsRed, vsDairy, vsOvo, vsMeat, nrow = 3)

ggplot(data = eicClustered) +
  geom_bar(mapping = aes(x = eicGrp, fill = factor(coff)))

length(which(eicClustered$Fish == 'High'))


ggplot(data = eicClustered) +
  geom_histogram(mapping = aes(x = AGE8, color = eicGrp))

#look at the group in upper right compared to other

#try clustering on principal components instead

#look at eigenvalues to determine how much of the correlation is explained by first few components

