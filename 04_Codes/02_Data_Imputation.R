# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  21
# Purpose:      Data Imputation
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
  replace((. == 0), NaN) %>% 
  mutate_all(function(x) {(x - 3) / 2})


##---- K nearest neighbors imputation ----
## OPN imputation
big5.opn <- select(big5.nan, starts_with('OPN'))

imp.opn <- knnImputation(
  big5.opn, 
  k = 10, 
  scale = FALSE, 
  meth = 'weighAvg', 
  distData = NULL
)

write.csv(imp.opn, '03_Outputs/OPN_imp.csv', row.names = FALSE)

## CSN imputation
big5.csn <- select(big5.nan, starts_with('CSN'))

imp.csn <- knnImputation(
  big5.csn, 
  k = 10, 
  scale = FALSE, 
  meth = 'weighAvg', 
  distData = NULL
)

write.csv(imp.csn, '03_Outputs/CSN_imp.csv', row.names = FALSE)

## EXT imputation
big5.ext <- select(big5.nan, starts_with('EXT'))

imp.ext <- knnImputation(
  big5.ext, 
  k = 10, 
  scale = FALSE, 
  meth = 'weighAvg', 
  distData = NULL
)

write.csv(imp.ext, '03_Outputs/EXT_imp.csv', row.names = FALSE)

## AGR imputation
big5.agr <- select(big5.nan, starts_with('AGR'))

imp.agr <- knnImputation(
  big5.agr, 
  k = 10, 
  scale = FALSE, 
  meth = 'weighAvg', 
  distData = NULL
)

write.csv(imp.agr, '03_Outputs/AGR_imp.csv', row.names = FALSE)

## EST imputation
big5.est <- select(big5.nan, starts_with('EST'))

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
  mutate_all(function(x) {x * 2 + 3}) %>% 
  mutate_all(round) %>% 
  bind_cols(big5.cleaned[, 51:53]) %>% 
  select(OPN1, OPN2, OPN3, OPN4, OPN5, OPN6, OPN7, OPN8, OPN9, OPN10, 
         CSN1, CSN2, CSN3, CSN4, CSN5, CSN6, CSN7, CSN8, CSN9, CSN10, 
         EXT1, EXT2, EXT3, EXT4, EXT5, EXT6, EXT7, EXT8, EXT9, EXT10, 
         AGR1, AGR2, AGR3, AGR4, AGR5, AGR6, AGR7, AGR8, AGR9, AGR10, 
         EST1, EST2, EST3, EST4, EST5, EST6, EST7, EST8, EST9, EST10, 
         country, ID, NMV)

write.csv(big5.imp, '03_Outputs/Big5_imp.csv', row.names = FALSE)
