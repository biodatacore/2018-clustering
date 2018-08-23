md <- readRDS('data/md.rds')

#Run foood group correlations with eicosanoids

results <- as.data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c('Eic', 'Food Group', 'Correlation')

fishCor <- md
fishCor <- mutate(fishCor, totFish = tuna + DK_FISH + OTH_FISH)

results <- rbind(results, corFoodGroup(fishCor, 'totFish'))

rdCor <- md
rdCor <- mutate(rdCor, totRed = bacon + hotdog + PROC_MTS + liver + hamb + SAND_BF + beef)

results <- rbind(results, corFoodGroup(rdCor, 'totRed'))

nutCor <- md

results <- rbind(results, corFoodGroup(nutCor, 'nuts'))

starchCor <- md
starchCor <- mutate(starchCor, totStarch = COLD_CER + CKD_OATS + CKD_CER + WH_BR + 
                      DK_BR + ENG_MUFF + muff + BR_RICE + WH_RICE + pasta + grains + pancake + FF_POT + MASH_POT + POT_CHIP + crax + pizza)

results <- rbind(results, corFoodGroup(starchCor, 'totStarch'))

sweetsCor <- md
sweetsCor <- mutate(sweetsCor, totSweet = choc + candynut + candy + COOX_HOM + COOX_COM + brownie + donut + CAKE_HOM + CAKE_COM + 
                      S_ROLL_H + S_ROLL_C + PIE_HOME + PIE_COMM + jam + P_BU)

results <- rbind(results, corFoodGroup(sweetsCor, 'totSweet'))

results$Correlation <- as.double(results$Correlation)
results <- results[order(-abs(results$Correlation)), ]


saveRDS(results, 'correlations/food_Grp.rds')
