colnames(md)
grep('points', colnames(md))
grep('numID', colnames(md))

eicOnly <- select(md, 12:534)

pcauEicOnly5c <- kmeansAndPca(eicOnly, colnames(eicOnly), FALSE, 5)
pcasEicOnly5c <- kmeansAndPca(eicOnly, colnames(eicOnly), TRUE, 5)

pcaAndKmeans(eicOnly, colnames(eicOnly), FALSE, 5)

tryClust('complete', 20)
tryClust('single', 20)
tryClust('average', 20)

