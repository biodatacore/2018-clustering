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
      print(paste(colnames(temp)[1], 'and', colnames(temp)[2], 'are columns (', i, ',', j, ') and have correlation', pairCors[ct], sep = ' '))
    }
    ct <- ct + 1
  }
}

orderedCors <- pairCors[order(-pairCors)]

head(orderedCors)


#put the highest correlation variables in a regression

ggplot(data = md) +
  geom_point(mapping = aes(x = mzid_445.227362_3.4312, y = beer))

ggplot(data = md) +
  geom_point(mapping = aes(x = mzid_327.217613_4.0425, y = mzid_329.232478_3.9113))

skrrrahh(16)

?cor
?order
?kmeans
