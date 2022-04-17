# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  21
# Purpose:      Data visualization
# programmer:   Zhe Liu
# Date:         2022-04-17
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Score distributions of items ----
## question
ques.opn <- c('OPN1', 'OPN2', 'OPN3', 'OPN4', 'OPN5', 
              'OPN6', 'OPN7', 'OPN8', 'OPN9', 'OPN10')
ques.csn <- c('CSN1', 'CSN2', 'CSN3', 'CSN4', 'CSN5', 
              'CSN6', 'CSN7', 'CSN8', 'CSN9', 'CSN10')
ques.ext <- c('EXT1', 'EXT2', 'EXT3', 'EXT4', 'EXT5', 
              'EXT6', 'EXT7', 'EXT8', 'EXT9', 'EXT10')
ques.agr <- c('AGR1', 'AGR2', 'AGR3', 'AGR4', 'AGR5', 
              'AGR6', 'AGR7', 'AGR8', 'AGR9', 'AGR10')
ques.est <- c('EST1', 'EST2', 'EST3', 'EST4', 'EST5', 
              'EST6', 'EST7', 'EST8', 'EST9', 'EST10')

## OPN
count.opn <- big5.imp[, ques.opn] %>% 
  pivot_longer(
    cols = everything(), 
    names_to = 'Question', 
    values_to = 'Scale'
  ) %>% 
  count(Question, Scale, name = 'Frequency')

plot.opn <- ggplot(mapping = aes(Scale, Frequency, fill = Question)) + 
  xlab(label = NULL) + 
  ylab(label = NULL) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none') + 
  scale_y_continuous(
    limits = c(0, 500000), 
    breaks = c(0, 100000, 200000, 300000, 400000, 500000), 
    labels = c('0', '100000', '200000', '300000', '400000', '500000')
  )

hist.opn1 <- plot.opn + 
  geom_bar(data = subset(count.opn, Question == 'OPN1'), stat = 'identity') + 
  labs(title = 'I have')

hist.opn2 <- plot.opn + 
  geom_bar(data = subset(count.opn, Question == 'OPN2'), stat = 'identity') + 
  labs(title = 'No')

hist.opn1+hist.opn2










