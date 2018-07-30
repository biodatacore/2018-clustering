source('functo.R')

start_eic <- grep('mzid_231.176117_1.8182', colnames(md))
end_eic <- grep('mzid_395.241335_2.6439', colnames(md))
start_ffq <- grep('skim', colnames(md))
end_ffq <- grep('S_SHAKE', colnames(md))

eicOnly <- start_eic:end_eic
foodOnly <- start_ffq:end_ffq
datCols <- c(eicOnly, foodOnly)

bCoeffs <- matrix(ncol = length(eicOnly), nrow = length(foodOnly))
posi <- posj <- 1

for(i in eicOnly){
  for(j in foodOnly){
    #bCoeffs[j - start_ffq + 1, 'food'] <- colnames(md)[j]
    sformula <- paste(colnames(md)[i], '~', colnames(md)[j], '+ AGE8 + SEX + BMI8 + SBP8 + CURRSMK8 + curr_diab8 + TC8') #not currently adjusting for plate because it behaves strangely
    bCoeffs[posj, posi] <- lm(sformula, data = md)$coefficients[colnames(md)[j]]
    posj <- posj + 1
  }
  posi <- posi + 1
  posj <- 1
}

?heatmap

corrplot(bCoeffs, method = 'color', tl.pos = 'n', order = 'hclust', hclust.method = 'ward.D2', col = rev(colorRampPalette(col2)(200)))

heatmap(bCoeffs, Rowv= as.dendrogram(hclust(dist(bCoeffs))), Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))

ggplot(data = as.data.frame(bCoeffs)) + 
  geom_tile(aes(fill = rescale), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue")

fullCor <- pcor.test(md[datCols], method = 'spearman')

col2 <- c(
  "#67001F",
  "#B2182B",
  "#D6604D",
  "#F4A582",
  "#FDDBC7",
  "#FFFFFF",
  "#D1E5F0",
  "#92C5DE",
  "#4393C3",
  "#2166AC",
  "#053061"
)


# Output Corrplot ---------------------------------------------------------



min(fullCor$estimate)

View(md[foodOnly])


adjusted_correlations_spearman <-
  matrix(nrow = ncol(md[foodOnly]), ncol = ncol(md[eicOnly]))

adjusted_correlations_spearman[1, 1] <- 1

rownames(adjusted_correlations_spearman) <- names(md[foodOnly])
colnames(adjusted_correlations_spearman) <- names(md[eicOnly])

for (rix in 1:(nrow(adjusted_correlations_spearman) - 1)) {
  t1_r <- Sys.time()
  for (cix in (rix + 1):nrow(adjusted_correlations_spearman)) {
    t1_c = Sys.time()
    foodName <- rownames(adjusted_correlations_spearman)[rix]
    eicName <- colnames(adjusted_correlations_spearman)[cix]
    
    # from https://en.wikipedia.org/wiki/Partial_correlation#Using_linear_regression
    x <- md[datCols][[foodName]]
    y <- md[datCols][[eicName]]
    plCol <- md$plate
    ageCol <- md$AGE8
    sexCol <- md$SEX
    bmiCol <- md$BMI8
    bpCol <- md$SBP8
    smkCol <- md$CURRSMK8
    diabCol <- md$curr_diab8
    tcCol <- md$TC8
    
    lm_x <- lm(x ~ plCol + ageCol + sexCol + bmiCol + bpCol + smkCol + diabCol + tcCol)
    lm_y <- lm(y ~ plCol + ageCol + sexCol + bmiCol + bpCol + smkCol + diabCol + tcCol)
    
    residuals_x <- residuals(lm_x)
    residuals_y <- residuals(lm_y)
    pcor <- cor(residuals_x, residuals_y, method = 'spearman')
    adjusted_correlations_spearman[rix, cix] <- adjusted_correlations_spearman[cix, rix] <- pcor
    t2_c = Sys.time()
    # print(glue::glue('The rix is {rix} the cix is {cix} and the pcor is {pcor} and it took {t2_c-t1_c} to run'))
    
    t2_c = Sys.time()
  }
  
  t2_r = Sys.time()
  
  print(glue::glue('It took {t2_r - t1_r} to run that row! {rix} row(s) down {nrow(adjusted_correlations_spearman) - rix} to go!'))
}


?cor

setwd('/Users/krao/Documents/BDC/2018-clustering')
pdf('plots/heat.pdf', width = 7, height = 7)
corrplot(fullCor, method = 'color', tl.pos = 'n', order = 'hclust', hclust.method = 'ward.D2', col = rev(colorRampPalette(col2)(200)))
corrplot(adjusted_correlations, method = 'color', tl.pos = 'n', order = 'hclust', hclust.method = 'ward.D2', col = rev(colorRampPalette(col2)(200)))
corrplot(adjusted_correlations_spearman, method = 'color', tl.pos = 'n', order = 'hclust', hclust.method = 'ward.D2', col = rev(colorRampPalette(col2)(200)))
dev.off()

grid.arrange(a, b, ncol = 2)

View(adjusted_correlations)

head(adjusted_correlations)

skrrrahh(16)

?pdf
