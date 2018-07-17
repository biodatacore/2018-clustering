grep('points', colnames(md))
grep('numID', colnames(md))

eicOnly <- 12:534
foodOnly <- 536:661

pairCors = numeric()
ct <- 1
cors <- setNames(data.frame(matrix(ncol = length(eicOnly), nrow = length(foodOnly))), colnames(md)[eicOnly])
cors <- mutate(cors, food = '', corTotal = 0, numHigh = 0)

for(i in eicOnly){
  for(j in foodOnly){
    cors[j - 535, 'food'] <- colnames(md)[j]
    temp <- select(md, i, j)
    temp <- filter(temp, complete.cases(temp))
    cors[j - 535, i - 11] <- pairCors[ct] <- cor(temp[1], temp[2])
    if(abs(pairCors[ct]) > .2){
      print(paste(colnames(temp)[1], 'and', colnames(temp)[2], 'are (c, r) (', i, ',', j, ') and have correlation', pairCors[ct], sep = ' '))
    }
    ct <- ct + 1
  }
}

orderedCors <- pairCors[order(-abs(pairCors))]
head(orderedCors)
View(orderedCors)
#View(cors)

cors$corTotal <- rowSums(abs(cors[1:grep('food', colnames(cors))-1]))
cors$numMed <- rowSums(ifelse( abs((cors[1:grep('food', colnames(cors))-1])) > .2, 1, 0))
cors$numHigh <- rowSums(ifelse( abs((cors[1:grep('food', colnames(cors))-1])) > .3, 1, 0))
#View(cors$corTotal)
#View(select(cors, food, numHigh))

cors <- cors[order(-cors$numMed), ]
head(select(cors, food, numMed, numHigh))


#results, cuz it takes so long to run
# [1] "mzid_395.278869_5.3755 and donut are columns ( 32 , 643 ) and have correlation 0.213758921896026"
# [1] "mzid_391.251076_5.3521 and donut are columns ( 43 , 643 ) and have correlation 0.363816273387038"
# [1] "mzid_391.251488_4.4307 and FF_POT are columns ( 48 , 618 ) and have correlation 0.203731497165278"
# [1] "mzid_405.112385_3.1192 and garlic are columns ( 56 , 591 ) and have correlation 0.22296435529906"
# [1] "mzid_427.269514_1.5112 and yog are columns ( 70 , 543 ) and have correlation 0.229063100939672"
# [1] "mzid_443.210900_5.8936 and liq are columns ( 97 , 636 ) and have correlation 0.21214119851694"
# [1] "mzid_445.227362_3.4312 and beer are columns ( 105 , 633 ) and have correlation 0.42014709818788"
# [1] "mzid_297.243016_6.3190 and liq are columns ( 113 , 636 ) and have correlation 0.237872079970313"
# [1] "mzid_449.254283_4.2680 and beer are columns ( 115 , 633 ) and have correlation 0.209281362388811"
# [1] "mzid_299.259208_5.5568 and liq are columns ( 156 , 636 ) and have correlation 0.204067352377753"
# [1] "mzid_606.283867_4.5146 and DK_FISH are columns ( 160 , 603 ) and have correlation 0.232989600890418"
# [1] "mzid_606.283867_4.5146 and OTH_FISH are columns ( 160 , 604 ) and have correlation 0.217051721453184"
# [1] "mzid_291.194445_4.3834 and DK_FISH are columns ( 173 , 603 ) and have correlation 0.216829028718093"
# [1] "mzid_265.177794_3.6499 and beer are columns ( 189 , 633 ) and have correlation 0.204872782278508"
# [1] "mzid_265.177794_3.6499 and liq are columns ( 189 , 636 ) and have correlation 0.207278902943846"
# [1] "mzid_315.196238_1.9972 and yog are columns ( 209 , 543 ) and have correlation 0.305901978710795"
# [1] "mzid_305.173630_5.7157 and POT_CHIP are columns ( 225 , 620 ) and have correlation 0.270026670439233"
# [1] "mzid_305.246542_5.9632 and FF_POT are columns ( 241 , 618 ) and have correlation 0.254625200587568"
# [1] "mzid_305.248615_6.1491 and corn are columns ( 243 , 576 ) and have correlation 0.206863810221751"
# [1] "mzid_331.246878_3.2601 and OTH_FISH are columns ( 265 , 604 ) and have correlation 0.222783766035752"
# [1] "mzid_313.238675_5.0376 and FF_POT are columns ( 268 , 618 ) and have correlation 0.220470255079351"
# [1] "mzid_331.263756_6.2746 and FF_POT are columns ( 270 , 618 ) and have correlation 0.241334347357092"
# [1] "mzid_315.195339_2.7282 and coff are columns ( 277 , 631 ) and have correlation 0.28759852369963"
# [1] "mzid_317.214641_4.9914 and DK_FISH are columns ( 292 , 603 ) and have correlation 0.218091376270222"
# [1] "mzid_321.239996_6.3408 and tofu are columns ( 304 , 568 ) and have correlation 0.279956598164671"
# [1] "mzid_321.239996_6.3408 and MIX_VEG are columns ( 304 , 578 ) and have correlation 0.236215748638047"
# [1] "mzid_339.252953_5.4621 and FF_POT are columns ( 306 , 618 ) and have correlation 0.231830629723571"
# [1] "mzid_341.233353_4.2053 and POT_CHIP are columns ( 319 , 620 ) and have correlation 0.225110582406818"
# [1] "mzid_323.257341_6.4061 and liq are columns ( 327 , 636 ) and have correlation 0.210594817425688"
# [1] "mzid_327.217613_4.0425 and donut are columns ( 332 , 643 ) and have correlation 0.273316981023506"
# [1] "mzid_327.220900_4.1352 and donut are columns ( 335 , 643 ) and have correlation 0.23958125371827"
# [1] "mzid_329.175422_2.2519 and coff are columns ( 342 , 631 ) and have correlation 0.229853081759661"
# [1] "mzid_329.232478_3.9113 and FF_POT are columns ( 347 , 618 ) and have correlation 0.226948800255934"
# [1] "mzid_329.232478_3.9113 and donut are columns ( 347 , 643 ) and have correlation 0.287419164507941"
# [1] "mzid_331.189811_2.6978 and P_BU are columns ( 357 , 651 ) and have correlation 0.229074873861914"
# [1] "mzid_289.180130_3.1744 and beer are columns ( 373 , 633 ) and have correlation 0.3377364127539"
# [1] "mzid_333.207269_1.8727 and coff are columns ( 376 , 631 ) and have correlation 0.212225025724888"
# [1] "mzid_353.268454_6.1965 and FF_POT are columns ( 386 , 618 ) and have correlation 0.280138837406538"
# [1] "mzid_353.268454_6.1965 and donut are columns ( 386 , 643 ) and have correlation 0.324452822549439"
# [1] "mzid_337.238459_5.0766 and FF_POT are columns ( 393 , 618 ) and have correlation 0.210319609347267"
# [1] "mzid_341.207620_6.4727 and ALF_SPRT are columns ( 401 , 590 ) and have correlation 0.270253319466362"
# [1] "mzid_343.226243_4.3057 and DK_FISH are columns ( 409 , 603 ) and have correlation 0.2397180372192"
# [1] "mzid_343.226508_6.5481 and ALF_SPRT are columns ( 410 , 590 ) and have correlation 0.265148279471564"
# [1] "mzid_363.254344_5.6632 and FF_POT are columns ( 417 , 618 ) and have correlation 0.210925377513508"
# [1] "mzid_345.242983_5.9250 and DK_FISH are columns ( 425 , 603 ) and have correlation 0.251941788531085"
# [1] "mzid_345.243806_6.6100 and ALF_SPRT are columns ( 426 , 590 ) and have correlation 0.22278590758791"
# [1] "mzid_347.221681_4.9459 and FF_POT are columns ( 438 , 618 ) and have correlation 0.20072619436192"
# [1] "mzid_347.219727_5.0766 and FF_POT are columns ( 440 , 618 ) and have correlation 0.216535351534898"
# [1] "mzid_349.202301_1.1295 and coff are columns ( 445 , 631 ) and have correlation 0.220935412449903"
# [1] "mzid_367.249592_4.2525 and FF_POT are columns ( 446 , 618 ) and have correlation 0.203510932841563"
# [1] "mzid_361.237487_2.4129 and decaf are columns ( 489 , 630 ) and have correlation 0.291589497271351"
# [1] "mzid_363.253595_6.0788 and FF_POT are columns ( 495 , 618 ) and have correlation 0.229972719986233"
# [1] "mzid_363.253595_6.0788 and POT_CHIP are columns ( 495 , 620 ) and have correlation 0.204147278169363"
# [1] "mzid_363.252993_2.8168 and prun are columns ( 497 , 550 ) and have correlation 0.218335812178485"
# [1] "mzid_363.253845_5.4961 and FF_POT are columns ( 499 , 618 ) and have correlation 0.203655029589978"
# [1] "mzid_368.115126_3.3764 and rais are columns ( 510 , 549 ) and have correlation 0.312341269944916"
# [1] "mzid_368.224901_6.5171 and milk are columns ( 511 , 537 ) and have correlation 0.32228330351842"


# [1] "mzid_606.283867_4.5146 and total fish are (c, r) ( 160 , 661 ) and have correlation 0.25722534075285"
# [1] "mzid_331.246878_3.2601 and total fish are (c, r) ( 265 , 661 ) and have correlation 0.224061299943633"
# [1] "mzid_343.226243_4.3057 and total fish are (c, r) ( 409 , 661 ) and have correlation 0.215111084249936"
# [1] "mzid_345.242983_5.9250 and total fish are (c, r) ( 425 , 661 ) and have correlation 0.219217415224351"



#put the highest correlation variables in a regression

ggplot(data = md) +
  geom_point(mapping = aes(x = mzid_445.227362_3.4312, y = beer))

ggplot(data = md) +
  geom_point(mapping = aes(x = log(mzid_327.217613_4.0425), y = log(mzid_329.232478_3.9113)))

skrrrahh(16)

summary(lm(mzid_445.227362_3.4312 ~ beer + AGE8 + SEX, data = md))
summary(lm(mzid_445.227362_3.4312 ~ beer, data = md))

fishCor <- md
fishCor <- mutate(fishCor, totFish = tuna + DK_FISH + OTH_FISH)
fishCors = numeric(); ct <- 1

for(i in eicOnly){
  temp <- select(fishCor, i, 'totFish')
  temp <- filter(temp, complete.cases(temp))
  fishCors[ct] <- cor(temp[1], temp[2])
  if(fishCors[ct] > .2){
    print(paste(colnames(temp)[1], '(col', i, ') and total fish have correlation', fishCors[ct], sep = ' '))
  }
  ct <- ct + 1
}



rdCor <- md
rdCor <- mutate(rdCor, totRed = bacon + hotdog + PROC_MTS + liver + hamb + SAND_BF + beef)
rdCors = numeric(); ct <- 1

for(i in eicOnly){
  temp <- select(rdCor, i, 'totRed')
  temp <- filter(temp, complete.cases(temp))
  rdCors[ct] <- cor(temp[1], temp[2])
  if(rdCors[ct] > .2){
    print(paste(colnames(temp)[1], '(col', i, ') and total red meat have correlation', rdCors[ct], sep = ' '))
  }
  ct <- ct + 1
}

orderedRedCors <- rdCors[order(-abs(rdCors))]
head(orderedRedCors)



nutCor <- md
nutCors = numeric(); ct <- 1

for(i in eicOnly){
  temp <- select(nutCor, i, 'nuts')
  temp <- filter(temp, complete.cases(temp))
  nutCors[ct] <- cor(temp[1], temp[2])
  if(abs(nutCors[ct]) > .2){
    print(paste(colnames(temp)[1], '(col', i, ') and these nuts have correlation', nutCors[ct], sep = ' '))
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
  temp <- select(starchCor, i, 'totStarch')
  temp <- filter(temp, complete.cases(temp))
  starchCors[ct] <- cor(temp[1], temp[2])
  if(abs(starchCors[ct]) > .2){
    print(paste(colnames(temp)[1], '(col', i, ') and total starch have correlation', starchCors[ct], sep = ' '))
  }
  ct <- ct + 1
}

orderedStarchCor <- starchCors[order(-abs(starchCors))]
head(orderedStarchCor)



sweetsCor <- md
sweetsCor <- mutate(sweetsCor, totSweet = choc + candynut + candy + COOX_HOM + COOX_COM + brownie + donut + CAKE_HOM + CAKE_COM + 
                      S_ROLL_H + S_ROLL_C + PIE_HOME + PIE_COMM + jam + P_BU)
sweetsCors = numeric(); ct <- 1

for(i in eicOnly){
  temp <- select(sweetsCor, i, 'totSweet')
  temp <- filter(temp, complete.cases(temp))
  sweetsCors[ct] <- cor(temp[1], temp[2])
  if(abs(sweetsCors[ct]) > .2){
    print(paste(colnames(temp)[1], '(col', i, ') and total sweets have correlation', sweetsCors[ct], sep = ' '))
  }
  ct <- ct + 1
}

orderedSweetsCor <- sweetsCors[order(-abs(sweetsCors))]
head(orderedSweetsCor)



?cor
?abs
?order
?log
?kmeans
