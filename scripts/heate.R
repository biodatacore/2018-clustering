library(plyr)
library(reshape2)
library(corrplot)

md <- readRDS('data/md.rds')
adj_cors <- readRDS('correlations/adjusted_correlations.rds')

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

row.names(bCoeffs) <- colnames(md)[foodOnly]
heatmap(bCoeffs, Rowv= as.dendrogram(hclust(dist(bCoeffs))), Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10), main = 'Beta coefficients of foods predicting eicosanoids')

# bCoeffs.m <- melt(bCoeffs)
# bCoeffs.m <- ddply(bCoeffs.m, rownames(bCoeffs.m), rescale = rescale(value))
# class(bCoeffs.m)
# View(bCoeffs.m)
# ggplot(data = as.data.frame(bCoeffs.m)) + 
#   geom_tile(aes(fill = rescale), colour = "white") + 
#   scale_fill_gradient(low = "white", high = "steelblue")
# 
# fullCor <- pcor.test(md[datCols], method = 'spearman')

# Output Corrplot ---------------------------------------------------------



col2 <- c("#67001F","#B2182B","#D6604D","#F4A582","#FDDBC7","#FFFFFF","#D1E5F0","#92C5DE","#4393C3","#2166AC","#053061")

pdf('plots/heat.pdf', width = 7, height = 7)
heatmap(adj_cors, Rowv= as.dendrogram(hclust(dist(adj_cors))), Colv=NA, col = cm.colors(256), scale="column", main = 'Adjusted correlations Food vs Eic')


numMed = rowSums(ifelse( abs((adj_cors[, 1:ncol(adj_cors)])) > .2, 1, 0))
numHigh = rowSums(ifelse( abs((adj_cors[, 1:ncol(adj_cors)])) > .3, 1, 0))

numMedEic = colSums(ifelse( abs((adj_cors[1:nrow(adj_cors), ])) > .2, 1, 0))
numHighEic = rowSums(ifelse( abs((adj_cors[1:nrow(adj_cors), ])) > .3, 1, 0))

importantFood = which(numMed > 0)
vImportantFood = which(numHigh > 0)
heatmap(adj_cors[importantFood, ], Rowv= as.dendrogram(hclust(dist(adj_cors[importantFood, ]))), Colv=NA, col = cm.colors(256), scale="column", main = 'Adjusted correlations Impt. Food vs Eic, Cor > .2')

importantEic = which(numMedEic > 0)
vImportantEic = which(numHighEic > 0)
heatmap(adj_cors[ ,importantEic], Rowv= as.dendrogram(hclust(dist(adj_cors[ ,importantEic]))), Colv=NA, col = cm.colors(256), scale="column", main = 'Adjusted correlations Food vs Impt. Eic, Cor > .2')

heatmap(adj_cors[importantFood, importantEic], Rowv= as.dendrogram(hclust(dist(adj_cors[importantFood, importantEic]))), Colv=NA, col = cm.colors(256), scale="column", main = 'Adjusted correlations Impt. Food vs Impt. Eic, Cor > .2')

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

dev.off()


skrrrahh(16)

?pdf
