# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  21
# Purpose:      Data testing
# programmer:   Zhe Liu
# Date:         2022-04-08
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- testing data set ----
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

## OSRI
osri.raw <- read.delim2('02_Inputs/1.Personality/OSRI/data.csv')

colSums(is.na(osri.raw))

osri.cleaned <- osri.raw %>% 
  filter_at(vars(starts_with('Q')), all_vars(. != 0))

## Big 5
big5.raw <- read.delim2('02_Inputs/1.Personality/Big5/data-final.csv')

big5.cleaned <- big5.raw %>% 
  select(-ends_with('_E')) %>% 
  select(ends_with(as.character(0:9)), country) %>% 
  filter(EXT1 != 'NULL') %>% 
  filter_all(all_vars(. !=0)) %>% 
  mutate_at(vars(ends_with(as.character(0:9))), as.numeric) %>% 
  arrange(country) %>% 
  mutate(ID = row_number())

colSums(is.na(big5.cleaned))
colSums(big5.cleaned == 0)
summary(big5.cleaned)
length(unique(big5.cleaned$country))

correlation <- cor(as.matrix(mutate_all(test.cleaned[1:50], as.numeric)))

plot(table((test.cleaned$country)))
head(sort(table(test.cleaned$country), decreasing = TRUE))

boxplot(correlation)






