library(dplyr)
library(tidyverse)
library(readxl)

getwd()
setwd('/Users/krao/Dropbox (Partners HealthCare)/Susan-Mir Shared/Eicosanoids-FS-CODE/eicdata/FHS')

clinicData <- read_excel('FHS_Mir_Corrected_df16_Progenesis_eic_only.xlsx')
idKey <- read_excel('fhs_ex8_platepos_id_key.xls')
ffqData <- read.csv('/Users/krao/Dropbox (Partners HealthCare)/2018 Applied Bioinformatics Work/Personal folders/Kevin Rao/temp data/ffq ex8 for kr.csv')




fhs <- readRDS('/Users/krao/Dropbox (Partners HealthCare)/2018 Applied Bioinformatics Work/Personal folders/Kevin Rao/fhs.rds')

View(fhs$id)
View(fhs$clinical)


# Filter to Eicosanoids ---------------------------------------------------

eics <-  
  fhs$metabolites$info %>% 
  filter(metabolite_type == 'Eicosanoids') %>% 
  pull(label)

eics

fhs$metabolites$data

eic_data <- 
  fhs$metabolites$data %>% 
  select(plate, well, plate_well, one_of(eics))

colnames(eic_data)

# Select Clinical Variables -----------------------------------------------

clin <-
  fhs$clinical %>% 
  select(ID, SEX, AGE8, chd, hardchd, hxhardcvd, chdtime, points)

colnames(clin)

View(clin$ID)

# Join --------------------------------------------------------------------

fhs$id <- fhs$id[order(fhs$id$ID), ]
View(fhs$id)

mergeEicId <- left_join(eic_data, fhs$id, by = c('plate_well' = 'Plate_Position'))
mergeClin <- left_join(mergeEicId, clin, by = 'ID')

md <- select(mergeClin, ID, plate, well, plate_well, one_of(names(clin)), everything())
md <- mutate(md, SEX = replace(SEX, which(md$SEX == 2), 0))

colnames(md)
md <- md[order(md$ID), ]
#View(md$ID)

ffqData <- transformFFQ(data)
dataWithId <- filter(ffqData, !is.na(ffqData$ID))
#View(dataWithId$ID)
colnames(dataWithId)

md <- filter(md, nchar(ID) == 6)
#View(md$ID)

md <- mutate(md, numID = strtoi(substr(ID, 3, 6), base = 10))
#View(select(md, ID, numID))

md <- left_join(md, dataWithId, by = c('numID' = 'ID'))
colnames(md)
#confirm that the match is correct somehow

#View(select(md, ID, numID))

#View(md[which(md$numID == 4), ])
md <- md[order(md$ID), ]


?setwd
?read_excel
?left_join
?one_of
