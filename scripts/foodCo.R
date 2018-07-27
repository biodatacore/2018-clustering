source('functo.R')


#Run foood group correlations with eicosanoids

fishCor <- md
fishCor <- mutate(fishCor, totFish = tuna + DK_FISH + OTH_FISH)
fishCors = numeric(); ct <- 1

for(i in eicOnly){
  temp <- dplyr::select(fishCor, i, 'totFish')
  temp <- filter(temp, complete.cases(temp))
  fishCors[ct] <- round(cor(temp[1], temp[2], method = 'spearman'), digits = 3)
  if(fishCors[ct] > .2){
    print(paste('logt', colnames(temp)[1], '(col', i, ') and totfish have cor', fishCors[ct], sep = ' '))
  }
  ct <- ct + 1
}

# [1] "mzid_606.283867_4.5146 and total fish are (c, r) ( 160 , 661 ) and have correlation 0.25722534075285"
# [1] "mzid_331.246878_3.2601 and total fish are (c, r) ( 265 , 661 ) and have correlation 0.224061299943633"
# [1] "mzid_343.226243_4.3057 and total fish are (c, r) ( 409 , 661 ) and have correlation 0.215111084249936"
# [1] "mzid_345.242983_5.9250 and total fish are (c, r) ( 425 , 661 ) and have correlation 0.219217415224351"
# [1] "logt mzid_606.283867_4.5146 (col 164 ) and totfish have cor 0.263"
# [1] "logt mzid_606.284422_4.6675 (col 165 ) and totfish have cor 0.203"
# [1] "logt mzid_291.194445_4.3834 (col 177 ) and totfish have cor 0.203"
# [1] "logt mzid_317.214641_4.9914 (col 296 ) and totfish have cor 0.217"
# [1] "logt mzid_343.226243_4.3057 (col 413 ) and totfish have cor 0.238"
# [1] "logt mzid_345.242983_5.9250 (col 429 ) and totfish have cor 0.243"
# [1] "logt mzid_273.186598_5.5421 (col 438 ) and totfish have cor 0.203"

rdCor <- md
rdCor <- mutate(rdCor, totRed = bacon + hotdog + PROC_MTS + liver + hamb + SAND_BF + beef)
rdCors = numeric(); ct <- 1

for(i in eicOnly){
  temp <- dplyr::select(rdCor, i, 'totRed')
  temp <- filter(temp, complete.cases(temp))
  rdCors[ct] <- round(cor(temp[1], temp[2], method = 'spearman'), digits = 3)
  if(rdCors[ct] > .2){
    print(paste('logt', colnames(temp)[1], '(col', i, ') and total red meat have correlation', rdCors[ct], sep = ' '))
  }
  ct <- ct + 1
}

#[1] "logt mzid_467.264563_2.0213 (col 122 ) and total red meat have correlation 0.218"

orderedRedCors <- rdCors[order(-abs(rdCors))]
head(orderedRedCors)


nutCor <- md
nutCors = numeric(); ct <- 1

for(i in eicOnly){
  temp <- dplyr::select(nutCor, i, 'nuts')
  temp <- filter(temp, complete.cases(temp))
  nutCors[ct] <- round(cor(temp[1], temp[2], method = 'spearman'), digits = 3)
  if(abs(nutCors[ct]) > .2){
    print(paste('logt', colnames(temp)[1], '(col', i, ') and these nuts have correlation', nutCors[ct], sep = ' '))
  }
  ct <- ct + 1
}

theseNutCors <- nutCors[order(-abs(nutCors))]
head(theseNutCors)



starchCor <- md
starchCor <- mutate(starchCor, totStarch = COLD_CER + CKD_OATS + CKD_CER + WH_BR + 
                      DK_BR + ENG_MUFF + muff + BR_RICE + WH_RICE + pasta + grains + pancake + FF_POT + MASH_POT + POT_CHIP + crax + pizza)
starchCors = numeric(); ct <- 1

for(i in eicOnly){
  temp <- dplyr::select(starchCor, i, 'totStarch')
  temp <- filter(temp, complete.cases(temp))
  starchCors[ct] <- round(cor(temp[1], temp[2], method = 'spearman'), digits = 3)
  if(abs(starchCors[ct]) > .2){
    print(paste('logt', colnames(temp)[1], '(col', i, ') and total starch have correlation', starchCors[ct], sep = ' '))
  }
  ct <- ct + 1
}

orderedStarchCor <- starchCors[order(-abs(starchCors))]
head(orderedStarchCor)

colnames(md)

sweetsCor <- md
sweetsCor <- mutate(sweetsCor, totSweet = choc + candynut + candy + COOX_HOM + COOX_COM + brownie + donut + CAKE_HOM + CAKE_COM + 
                      S_ROLL_H + S_ROLL_C + PIE_HOME + PIE_COMM + jam + P_BU)
sweetsCors = numeric(); ct <- 1

for(i in eicOnly){
  temp <- dplyr::select(sweetsCor, i, 'totSweet')
  temp <- filter(temp, complete.cases(temp))
  sweetsCors[ct] <- round(cor(temp[1], temp[2], method = 'spearman'), digits = 3)
  if(abs(sweetsCors[ct]) > .2){
    print(paste('logt', colnames(temp)[1], '(col', i, ') and total sweets have correlation', sweetsCors[ct], sep = ' '))
  }
  ct <- ct + 1
}

orderedSweetsCor <- sweetsCors[order(-abs(sweetsCors))]
head(orderedSweetsCor)

