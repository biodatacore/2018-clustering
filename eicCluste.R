colnames(md)
grep('points', colnames(md))
grep('numID', colnames(md))

eicOnly <- select(md, 12:534)

pcauEicOnly4c <- kmeansAndPca(eicOnly, colnames(eicOnly), FALSE, 4)
pcasEicOnly5c <- kmeansAndPca(eicOnly, colnames(eicOnly), TRUE, 5)

temp <- pcaContribution(pcasEicOnly5c)
head(temp)
temp <- temp[order(-temp$Comp.2), ]
head(temp)

pcaAndKmeans(eicOnly, colnames(eicOnly), FALSE, 5)

tryClust('complete', 20)
tryClust('single', 20)
tryClust('average', 20)

#going back to roots lets try putting clusters together with classification of diet

forDiet <- md

forDiet <- addClassCol(forDiet, 'Dairy', c('skim', 'milk', 'cream'), TRUE)
forDiet <- addClassCol(forDiet, 'Ovo', c('eggs'), TRUE)
forDiet <- addClassCol(forDiet, 'Fish', c('tuna', 'DK_FISH', 'OTH_FISH', 'shrimp'), TRUE)
forDiet <- addClassCol(forDiet, 'Meat', c('CHIX_SK', 'CHIX_NO', 'bacon', 'hotdog', 'PROC_MTS', 'liver', 'hamb', 'SAND_BF', 'beef'), TRUE)

forDiet <- mutate(forDiet, classification = (Dairy == 'Normal') + (Ovo == 'Normal') * 2 + (Fish == 'Normal') * 4 + (Meat == 'Normal') * 8)

forDiet <- mutate(forDiet, type = ifelse(classification <= 1, 'vegan', ifelse(classification < 8, 'vegetarian', 'omnivore')))


cluster <- runKmeans(eicOnly, colnames(eicOnly), 4)

grpPCA <- princomp(eicOnly)
pcaTable <- mutate(as.data.frame(grpPCA$scores), grp = factor(cluster$cluster), type = forDiet$type)

a <- ggplot(data = pcaTable) +
  geom_point(mapping = aes(x = Comp.1, y = Comp.2, color = grp, shape = type))

vfocused <- filter(pcaTable, type != 'omnivore')
b <- ggplot(data = vfocused) +
  geom_point(mapping = aes(x = Comp.1, y = Comp.2, color = grp, shape = type))

ggplot(data = vfocused) +
  geom_bar(mapping = aes(x = grp, fill = type))

grid.arrange(a, b, ncol = 2)

