
classDiet <- md

classDiet <- addClassCol(classDiet, 'Dairy', c('skim', 'milk', 'cream'), TRUE)
classDiet <- addClassCol(classDiet, 'Ovo', c('eggs'), TRUE)
classDiet <- addClassCol(classDiet, 'Fish', c('tuna', 'DK_FISH', 'OTH_FISH', 'shrimp'), TRUE)
classDiet <- addClassCol(classDiet, 'Meat', c('CHIX_SK', 'CHIX_NO', 'bacon', 'hotdog', 'PROC_MTS', 'liver', 'hamb', 'SAND_BF', 'beef'), TRUE)

classDiet <- mutate(classDiet, classification = (Dairy != 'Low') + (Ovo != 'Low') * 2 + (Fish != 'Low') * 4 + (Meat != 'Low') * 8)


classDiet$vegan <- as.integer(classDiet$classification == 0)
classDiet$vegetarian <- as.integer(classDiet$classification < 4)
classDiet$pesc <- as.integer(classDiet$classification < 8)

View(select(classDiet, Dairy, Ovo, Fish, Meat, classification, vegan, vegetarian, pesc))

vegCorBeta <- matrix(nrow = ncol(md[eicOnly]), ncol = 2) #vegan
vgCorBeta <- matrix(nrow = ncol(md[eicOnly]), ncol = 2) #vegetarian
pescCorBeta <- matrix(nrow = ncol(md[eicOnly]), ncol = 2) #pescatarian


rownames(vegCorBeta) <- rownames(vgCorBeta) <- rownames(pescCorBeta) <- names(md[eicOnly])
colnames(vegCorBeta) <- colnames(vgCorBeta) <- colnames(pescCorBeta) <- c('Correlation', 'Beta Coefficient')



vegCorBeta[, 1] <- runPcor(md[eicOnly], classDiet$vegan)
vgCorBeta[, 1] <- runPcor(md[eicOnly], classDiet$vegetarian)
pescCorBeta[, 1] <- runPcor(md[eicOnly], classDiet$pesc)

vegCorBeta[, 2] <- runBCoeffs(classDiet, 'vegan')
vgCorBeta[, 2] <- runBCoeffs(classDiet, 'vegetarian')
pescCorBeta[, 2] <- runBCoeffs(classDiet, 'pesc')

viewcorbeta <- function(dat){
  srt <- as.data.frame(dat)
  srt <- srt[order(-abs(srt$Correlation)), ]
  srt
}
veg <- viewcorbeta(vegCorBeta); View(veg)
vg <- viewcorbeta(vgCorBeta); View(vg)
pesc <- viewcorbeta(pescCorBeta); View(pesc)

setwd('/Users/krao/Documents/BDC/')
saveRDS(veg, 'correlations/veganCorBeta.rds')
saveRDS(vg, 'correlations/vegetarianCorBeta.rds')
saveRDS(pesc, 'correlations/pescCorBeta.rds')





