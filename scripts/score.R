
HEICorBeta <- matrix(nrow = ncol(md[eicOnly]), ncol = 2)
rownames(HEICorBeta) <- names(md[eicOnly])
colnames(HEICorBeta) <- c('Correlation', 'Beta')
HEICorBeta[, 1] <- runPcor(md[eicOnly], md$HEI)
HEICorBeta[, 2] <- runBCoeffs(md, 'HEI')

sortedHEI <- as.data.frame(HEICorBeta)
sortedHEI <- sortedHEI[order(-abs(sortedHEI$Correlation)), ]

View(sortedHEI)

ggplot(data = md) + 
  geom_point(mapping = aes(x = HEI, y = mzid_606.284422_4.6675))






