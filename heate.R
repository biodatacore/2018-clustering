library(corrplot)
library(ppcor)


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


lm(coff ~ AGE8, data = md)

View(md[, c(20, 100, 600)])

bCoeffs[1,2]
l <- lm(paste(colnames(md)[19], '~', colnames(md)[20], '+ AGE8 + SEX + BMI8 + SBP8 + CURRSMK8 + curr_diab8 + TC8'), data = md)
l$na.action

dim(bCoeffs)
superheat(bCoeffs)

corrplot(bCoeffs, method = 'ellipse')

noNaMd <- filter(md[datCols], complete.cases(md[datCols]))
fullCor <- pcor(noNaMd, method = 'spearman')
View(fullCor$estimate)

corrplot(fullCor$estimate, method = 'square')

corrplot(fullCor$estimate, method = 'color', tl.pos = 'n', order = 'hclust', hclust.method = 'ward.D2', col = rev(colorRampPalette(col2)(200)))




skrrrahh(16)
