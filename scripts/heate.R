source('functo.R')

start_eic <- grep('mzid_231.176117_1.8182', colnames(md))
end_eic <- grep('mzid_395.241335_2.6439', colnames(md))
start_ffq <- grep('skim', colnames(md))
end_ffq <- grep('S_SHAKE', colnames(md))

eicOnly <- start_eic:end_eic
foodOnly <- start_ffq:end_ffq
datCols <- c(eicOnly, foodOnly)
bCoeffs <- setNames(data.frame(matrix(ncol = length(datCols), nrow = length(datCols))), colnames(md)[c(eicOnly, foodOnly)])
posi <- posj <- 1

for(i in datCols){
  for(j in datCols){
    #bCoeffs[j - start_ffq + 1, 'food'] <- colnames(md)[j]
    sformula <- paste(colnames(md)[i], '~', colnames(md)[j], '+ AGE8 + SEX + BMI8 + SBP8 + CURRSMK8 + curr_diab8 + TC8') #not currently adjusting for plate because it behaves strangely
    if(posi == posj){
      bCoeffs[posi, posj] <- 1
    }
    else{
      bCoeffs[posi, posj] <- lm(sformula, data = md)$coefficients[colnames(md)[j]]
      #print(paste(sformula, lm(sformula, data = md)$coefficients))
    }
    posj <- posj + 1
  }
  posi <- posi + 1
  posj <- 1
}

superheat(bCoeffs)

corrplot(bCoeffs, method = 'color', tl.pos = 'n', order = 'hclust', hclust.method = 'ward.D2', col = rev(colorRampPalette(col2)(200)))

fullCor <- cor(md[datCols], method = 'spearman')
fullCor[94, 621]

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


adjusted_correlations <-
  matrix(nrow = ncol(md[datCols]), ncol = ncol(md[datCols]))

adjusted_correlations[1, 1] <- 1

rownames(adjusted_correlations) <- colnames(adjusted_correlations) <- names(md[datCols])

for (rix in 1:(nrow(adjusted_correlations) - 1)) {
  t1_r <- Sys.time()
  adjusted_correlations[rix, rix] <- 1
  for (cix in (rix + 1):nrow(adjusted_correlations)) {
    t1_c = Sys.time()
    mzid_r <- rownames(adjusted_correlations)[rix]
    mzid_c <- colnames(adjusted_correlations)[cix]
    
    # from https://en.wikipedia.org/wiki/Partial_correlation#Using_linear_regression
    x <- md[datCols][[mzid_r]]
    y <- md[datCols][[mzid_c]]
    z <- md$plate
    
    lm_xz <- lm(x ~ z)
    lm_yz <- lm(y ~ z)
    
    residuals_xz <- residuals(lm_xz)
    residuals_yz <- residuals(lm_yz)
    pcor <- cor(residuals_xz, residuals_yz)
    adjusted_correlations[rix, cix] <- adjusted_correlations[cix, rix] <- pcor
    t2_c = Sys.time()
    # print(glue::glue('The rix is {rix} the cix is {cix} and the pcor is {pcor} and it took {t2_c-t1_c} to run'))
    
    t2_c = Sys.time()
  }
  
  t2_r = Sys.time()
  
  print(glue::glue('It took {t2_r - t1_r} to run that row! {rix} row(s) down {nrow(adjusted_correlations) - rix} to go!'))
}

setwd('/Users/krao/Documents/BDC/2018-clustering')
pdf('plots/heat.pdf', width = 7, height = 7)
corrplot(fullCor, method = 'color', tl.pos = 'n', order = 'hclust', hclust.method = 'ward.D2', col = rev(colorRampPalette(col2)(200)))
corrplot(adjusted_correlations, method = 'color', tl.pos = 'n', order = 'hclust', hclust.method = 'ward.D2', col = rev(colorRampPalette(col2)(200)))
dev.off()

grid.arrange(a, b, ncol = 2)

View(adjusted_correlations)

head(adjusted_correlations)

skrrrahh(16)

?pdf
