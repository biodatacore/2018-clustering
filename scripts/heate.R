library(plyr)
library(reshape2)
library(corrplot)

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

setwd('/Users/krao/Dropbox (Partners HealthCare)/2018 Applied Bioinformatics Work/Personal folders/Kevin Rao')
write.csv(bCoeffs, 'betaCoeffs.csv')

corrplot(bCoeffs, method = 'color', tl.pos = 'n', order = 'hclust', hclust.method = 'ward.D2', col = rev(colorRampPalette(col2)(200)))


row.names(bCoeffs) <- colnames(md)[foodOnly]
heatmap(bCoeffs, Rowv= as.dendrogram(hclust(dist(bCoeffs))), Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10), main = 'Beta coefficients of foods predicting eicosanoids')

bCoeffs.m <- melt(bCoeffs)
bCoeffs.m <- ddply(bCoeffs.m, rownames(bCoeffs.m), rescale = rescale(value))
class(bCoeffs.m)
View(bCoeffs.m)
ggplot(data = as.data.frame(bCoeffs.m)) + 
  geom_tile(aes(fill = rescale), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue")

fullCor <- pcor.test(md[datCols], method = 'spearman')

# Output Corrplot ---------------------------------------------------------



min(fullCor$estimate)

View(md[foodOnly])

adj_cors <-
  matrix(nrow = ncol(md[foodOnly]), ncol = ncol(md[eicOnly]))

rownames(adj_cors) <- names(md[foodOnly])
colnames(adj_cors) <- names(md[eicOnly])

for (rix in 1:nrow(adj_cors)) {
  t1_r <- Sys.time()
  for (cix in 1:ncol(adj_cors)) {
    t1_c = Sys.time()
    foodName <- rownames(adj_cors)[rix]
    eicName <- colnames(adj_cors)[cix]
    
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
    adj_cors[rix, cix] <- pcor
    t2_c = Sys.time()
    # print(glue::glue('The rix is {rix} the cix is {cix} and the pcor is {pcor} and it took {t2_c-t1_c} to run'))
    
    t2_c = Sys.time()
  }
  
  t2_r = Sys.time()
  
  print(glue::glue('It took {t2_r - t1_r} to run that row! {rix} row(s) down {nrow(adj_cors) - rix} to go!'))
}

col2 <- c("#67001F","#B2182B","#D6604D","#F4A582","#FDDBC7","#FFFFFF","#D1E5F0","#92C5DE","#4393C3","#2166AC","#053061")


write.csv(adj_cors, 'adjusted_correlations.csv')


setwd('/Users/krao/Documents/BDC/2018-clustering')
pdf('plots/heat.pdf', width = 7, height = 7)
corrplot(fullCor, method = 'color', tl.pos = 'n', order = 'hclust', hclust.method = 'ward.D2', col = rev(colorRampPalette(col2)(200)))
corrplot(adj_cors, method = 'color', tl.pos = 'n', order = 'hclust', hclust.method = 'ward.D2', col = rev(colorRampPalette(col2)(200)))
dev.off()

heatmap(adj_cors, Rowv= as.dendrogram(hclust(dist(adj_cors))), Colv=NA, col = cm.colors(256), scale="column", main = 'Adjusted correlations Food vs Eic')

numMed = rowSums(ifelse( abs((adj_cors[, 1:ncol(adj_cors)])) > .2, 1, 0))
numHigh = rowSums(ifelse( abs((adj_cors[, 1:ncol(adj_cors)])) > .3, 1, 0))

numMedEic = colSums(ifelse( abs((adj_cors[1:nrow(adj_cors), ])) > .2, 1, 0))
numHighEic = rowSums(ifelse( abs((adj_cors[1:nrow(adj_cors), ])) > .3, 1, 0))

View(adj_cors)
importantFood = which(numMed > 0)
vImportantFood = which(numHigh > 0)
heatmap(adj_cors[importantFood, ], Rowv= as.dendrogram(hclust(dist(adj_cors[importantFood, ]))), Colv=NA, col = cm.colors(256), scale="column", main = 'Adjusted correlations Food vs Eic, Cor > .2')

importantEic = which(numMedEic > 0)
vImportantEic = which(numHighEic > 0)
heatmap(adj_cors[ ,importantEic], Rowv= as.dendrogram(hclust(dist(adj_cors[ ,importantEic]))), Colv=NA, col = cm.colors(256), scale="column", main = 'Adjusted correlations Food vs Eic, Cor > .2')

heatmap(adj_cors[importantFood, importantEic], Rowv= as.dendrogram(hclust(dist(adj_cors[importantFood, importantEic]))), Colv=NA, col = cm.colors(256), scale="column", main = 'Adjusted correlations Food vs Eic, Cor > .2')


melted <- melt(adj_cors[importantFood, importantEic], id.vars = 'Eicosanoids')
head(melted)

ggplot(melted, aes(Var2, Var1)) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  ylab("Eicosanoids") +
  xlab("Foods") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Correlation")


clustOrder <- hclust(dist(adj_cors[importantFood, importantEic], method = 'euclidean'), method = 'ward.D')$order
clustOrder

clustOrder.m <- melt(adj_cors[importantFood, importantEic], id.vars = 'Eicosanoids', variable.name = 'Foods')

head(clustOrder.m)

ggplot(clustOrder.m, aes(Var2, Var1)) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  ylab('Food Groups') +
  xlab('Eicosanoids') +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Correlation")

skrrrahh(16)

?pdf
