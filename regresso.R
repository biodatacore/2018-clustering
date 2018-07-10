grep('points', colnames(md))
grep('numID', colnames(md))


pairCors = numeric()
ct <- 1
for(i in 12:534){
  for(j in 536:661){
    temp <- select(md, i, j)
    temp <- filter(temp, complete.cases(temp))
    pairCors[ct] <- cor(temp[1], temp[2])
    if(pairCors[ct] > .25){
      print(paste(colnames(temp)[1], 'and', colnames(temp)[2], 'have correlation', pairCors[ct], sep = ' '))
    }
    ct <- ct + 1
  }
}

temp <- pairCors[order(-pairCors)]

head(temp)


skrrrahh(16)

?cor
?order
