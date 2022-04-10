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

## Big 5
test.data.5 <- read.delim2('02_Inputs/Big5/data-final.csv')

test.cleaned <- test.data.5 %>% 
  select(-ends_with('_E')) %>% 
  select(ends_with(as.character(0:9))) %>% 
  filter(EXT1 != 'NULL') %>% 
  mutate_all(as.numeric) %>% 
  mutate_all()
  mutate_all(function(x) {
    if (x == 0) {
      NaN
    } else {
      x - 3
    }
  }) %>% 
  mutate(ID = row_number())

for (i in 0:5) {
  na.check <- colSums(is.na(test.cleaned[(10*i+1):(10*(i+1))]))
  print(na.check)
}

for (i in 0:4) {
  for (j in (10*i+1):(10*(i+1))) {
    score.summary <- table(test.cleaned[[j]])
    print(score.summary)
  }
}


correlation <- cor(as.matrix(mutate_all(test.cleaned[1:50], as.numeric)))

plot(table((test.cleaned$country)))
head(sort(table(test.cleaned$country), decreasing = TRUE))

boxplot(correlation)




