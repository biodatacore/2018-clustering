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

forDiet <- addClassCol(forDiet, 'Dairy', c('skim', 'milk', 'cream'))
forDiet <- addClassCol(forDiet, 'Ovo', c('eggs'))
forDiet <- addClassCol(forDiet, 'Fish', c('tuna', 'DK_FISH', 'OTH_FISH', 'shrimp'))
forDiet <- addClassCol(forDiet, 'Meat', c('CHIX_SK', 'CHIX_NO', 'bacon', 'hotdog', 'PROC_MTS', 'liver', 'hamb', 'SAND_BF', 'beef'))

forDiet <- mutate(forDiet, classification = (Dairy == 'Normal') + (Ovo == 'Normal') * 2 + (Fish == 'Normal') * 4 + (Meat == 'Normal') * 8)

forDiet <- mutate(forDiet, type = ifelse(classification <= 1, 'vegan', ifelse(classification < 8, 'vegetarian', 'omnivore')))





