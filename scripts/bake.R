md <- readRDS('data/md.rds')

start_eic <- grep('mzid_231.176117_1.8182', colnames(md))
end_eic <- grep('mzid_395.241335_2.6439', colnames(md))
start_ffq <- grep('skim', colnames(md))
end_ffq <- grep('S_SHAKE', colnames(md))

eicOnly <- start_eic:end_eic
foodOnly <- start_ffq:end_ffq

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


saveRDS(adj_cors, 'correlations/adjusted_correlations.rds')

