library(readxl)

getwd()
setwd('/Users/krao/Dropbox (Partners HealthCare)/Susan-Mir Shared/Eicosanoids-FS-CODE/eicdata/FHS')

clinicData <- read_excel('FHS_Mir_Corrected_df16_Progenesis_eic_only.xlsx')
idKey <- read_excel('fhs_ex8_platepos_id_key.xls')
ffqData <- read.csv('/Users/krao/Dropbox (Partners HealthCare)/2018 Applied Bioinformatics Work/Personal folders/Kevin Rao/temp data/ffq ex8 for kr.csv')
setwd('/Users/krao/Dropbox/2018 Applied Bioinformatics Work/Personal Folders/Kevin Rao')
data <- read.csv('sample.csv')



fhs <- readRDS('/Users/krao/Dropbox (Partners HealthCare)/2018 Applied Bioinformatics Work/Personal folders/Kevin Rao/fhs.rds')


# Filter to Eicosanoids ---------------------------------------------------

eics <-  
  fhs$metabolites$info %>% 
  filter(metabolite_type == 'Eicosanoids') %>% 
  pull(label)


eic_data <- 
  fhs$metabolites$data %>% 
  select(plate, well, plate_well, one_of(eics))

#colnames(eic_data)

# Select Clinical Variables -----------------------------------------------

clin <-
  fhs$clinical %>% 
  select(ID, SEX, AGE8, chd, hardchd, hxhardcvd, chdtime, points, BMI8, BG8, SBP8, CURRSMK8, curr_diab8, TC8)

#colnames(clin)

#View(clin$ID)

# Join --------------------------------------------------------------------

fhs$id <- fhs$id[order(fhs$id$ID), ]
#View(fhs$id)

mergeEicId <- left_join(eic_data, fhs$id, by = c('plate_well' = 'Plate_Position'))
mergeClin <- left_join(mergeEicId, clin, by = 'ID')

md <- select(mergeClin, ID, plate, well, plate_well, one_of(names(clin)), everything())
md <- mutate(md, SEX = replace(SEX, which(md$SEX == 2), 0))



# Clean and Impute --------------------------------------------------------



eicImputer <- function(x) {
  #replace missing values with a quarter of minimum value, log transform data
  x <- na_if(x, 0)
  
  x[is.na(x)] <- min(x, na.rm = T)/4
  
  x <- log(x)
  return(x)
}

foodImputer <- function(x){
  #replace missing food items with the lowest possible response
  x[is.na(x)] <- 1/60
  return(x)
}



md <- 
  md %>% 
  mutate_at(vars(starts_with('mzid')), eicImputer)



ffqData <- transformFFQ(data)
dataWithId <- filter(ffqData, !is.na(ffqData$id))

md <- filter(md, nchar(ID) == 6)

md <- mutate(md, numID = strtoi(substr(ID, 3, 6), base = 10))

md <- left_join(md, dataWithId, by = c('numID' = 'id'))
#colnames(md)
#confirm that the match is correct somehow

#View(select(md, ID, numID))

#View(md[which(md$numID == 4), ])
md <- md[order(md$ID), ]

start_ffq <- grep('skim', colnames(md))
end_ffq <- grep('S_SHAKE', colnames(md))
foodOnly <- start_ffq:end_ffq

noFFQ <- md[foodOnly] %>% 
  apply(1, function(x) all(is.na(x))) %>% 
  which()

md <- md[-noFFQ, ]

md <- 
  md %>% 
  mutate_at(foodOnly, foodImputer)

md <- mutate_at(md, grep('totcal', colnames(md)), function(x){x[is.na(x)] <- 100; return(x)})
md <- computeHEI(md)

?setwd
?read_excel
?left_join
?one_of
?log
