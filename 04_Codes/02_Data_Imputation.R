# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  21
# Purpose:      Data imputation
# programmer:   Zhe Liu
# Date:         2022-04-12
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Dividing data set ----
## data set of non-missing values
# big5.nm <- big5.cleaned %>% 
#   filter(NMV == 0) %>% 
#   mutate(flag = 0)

## data set containing missing values
# big5.cm <- big5.cleaned %>% 
#   filter(NMV >= 1) %>% 
#   mutate(flag = 1)

## replacing 0 with NaN
big5.nan <- big5.cleaned[, 1:50] %>% 
  replace((. == 0), NaN)


##---- K nearest neighbors imputation ----
## dimension segmentation
big5.opn <- select(big5.nan, starts_with('OPN'))
big5.csn <- select(big5.nan, starts_with('CSN'))
big5.ext <- select(big5.nan, starts_with('EXT'))
big5.agr <- select(big5.nan, starts_with('AGR'))
big5.est <- select(big5.nan, starts_with('EST'))

## OPN imputation
imp.opn <- knnImputation(
  big5.opn, 
  k = 10, 
  scale = FALSE, 
  meth = 'weighAvg', 
  distData = NULL
)

write.csv(imp.opn, '03_Outputs/OPN_imp.csv', row.names = FALSE)

## CSN imputation
imp.csn <- knnImputation(
  big5.csn, 
  k = 10, 
  scale = FALSE, 
  meth = 'weighAvg', 
  distData = NULL
)

write.csv(imp.csn, '03_Outputs/CSN_imp.csv', row.names = FALSE)

## EXT imputation
imp.ext <- knnImputation(
  big5.ext, 
  k = 10, 
  scale = FALSE, 
  meth = 'weighAvg', 
  distData = NULL
)

write.csv(imp.ext, '03_Outputs/EXT_imp.csv', row.names = FALSE)

## AGR imputation
imp.agr <- knnImputation(
  big5.agr, 
  k = 10, 
  scale = FALSE, 
  meth = 'weighAvg', 
  distData = NULL
)

write.csv(imp.agr, '03_Outputs/AGR_imp.csv', row.names = FALSE)

## EST imputation
imp.est <- knnImputation(
  big5.est, 
  k = 10, 
  scale = FALSE, 
  meth = 'weighAvg', 
  distData = NULL
)

write.csv(imp.est, '03_Outputs/EST_imp.csv', row.names = FALSE)


##---- Imputed data ----
imp.opn <- read.csv('03_Outputs/OPN_imp.csv')
imp.csn <- read.csv('03_Outputs/CSN_imp.csv')
imp.ext <- read.csv('03_Outputs/EXT_imp.csv')
imp.agr <- read.csv('03_Outputs/AGR_imp.csv')
imp.est <- read.csv('03_Outputs/EST_imp.csv')

big5.imp <- bind_cols(imp.opn, imp.csn, imp.ext, imp.agr, imp.est) %>% 
  mutate_all(round) %>% 
  bind_cols(big5.cleaned[, 51:53])

write.csv(big5.imp, '03_Outputs/Big5_imp.csv', row.names = FALSE)
