# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  21
# Purpose:      Data cleaning
# programmer:   Zhe Liu
# Date:         2022-04-12
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin raw data of Big5 ----
big5.raw <- read.delim2('02_Inputs/1.Personality/Big5/data-final.csv')


##---- Missing value manipulation ----
## variables selection
big5.var <- big5.raw %>% 
  select(-ends_with('_E')) %>% 
  select(ends_with(as.character(0:9)), country) %>% 
  arrange(country)

## missing values check
summary(big5.var)
colSums(is.na(big5.var))
colSums(big5.var == 0)
table(rowSums(big5.var[1:50] == 0))

## eliminating missing values
big5.cleaned <- big5.var %>% 
  filter(EXT1 != 'NULL') %>% 
  mutate_at(vars(ends_with(as.character(0:9))), as.numeric) %>% 
  mutate(NMV = rowSums(select(., ends_with(as.character(0:9))) == 0)) %>% 
  filter(NMV <= 5) %>% 
  mutate(ID = row_number())

colSums(big5.cleaned == 0, na.rm = TRUE)

## writing out cleaned data
write.csv(big5.cleaned, '03_Outputs/Big5_Cleaned.csv', row.names = FALSE)
