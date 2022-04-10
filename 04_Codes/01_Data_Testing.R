# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  21
# Purpose:      Data testing
# programmer:   Zhe Liu
# Date:         2022-04-08
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Data testing ----
## NBA
test.data.1 <- read.csv('02_Inputs/NBA/train.csv', header = TRUE)
test.data.2 <- read.csv('02_Inputs/NBA/test.csv', header = TRUE)

## MBTI
test.data.3 <- read_csv('02_Inputs/MBTI/MBTI 500.csv')

t.check <- test.data.3 %>% 
  mutate(
    num = sapply(.$posts, function(x) {
      x %>% strsplit(split = ' ') %>% unlist() %>% length()
    }, simplify = TRUE)
  )

## Mobile
test.data.4 <- read.csv('02_Inputs/China_Mobile/events.csv', header = TRUE)

## Big 5
test.data.5 <- read.csv('02_Inputs/Big5/IPIP-FFM-data-8Nov2018/data-final.csv', header = TRUE)





