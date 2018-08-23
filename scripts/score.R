md <- readRDS('data/md.rds')

start_eic <- grep('mzid_231.176117_1.8182', colnames(md))
end_eic <- grep('mzid_395.241335_2.6439', colnames(md))
start_ffq <- grep('skim', colnames(md))
end_ffq <- grep('S_SHAKE', colnames(md))

eicOnly <- start_eic:end_eic
foodOnly <- start_ffq:end_ffq


HEICorBeta <- matrix(nrow = ncol(md[eicOnly]), ncol = 2)
rownames(HEICorBeta) <- names(md[eicOnly])
colnames(HEICorBeta) <- c('Correlation', 'Beta')
HEICorBeta[, 1] <- runPcor(md[eicOnly], md$HEI)
HEICorBeta[, 2] <- runBCoeffs(md, 'HEI')

sortedHEI <- as.data.frame(HEICorBeta)
sortedHEI <- sortedHEI[order(-abs(sortedHEI$Correlation)), ]

View(sortedHEI)

saveRDS(sortedHEI, 'correlations/heiCorBeta.rds')






