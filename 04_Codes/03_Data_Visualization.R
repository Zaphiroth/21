# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  21
# Purpose:      Data visualization
# programmer:   Zhe Liu
# Date:         2022-04-17
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Score distributions of items ----
## item list
item.seq <- read.xlsx('02_Inputs/1.Personality/Big5/Item.xlsx')

## frequency
big5.count <- big5.imp %>% 
  select(-country, -NMV, -ID) %>% 
  pivot_longer(
    cols = everything(), 
    names_to = 'Item', 
    values_to = 'Scale'
  ) %>% 
  count(Item, Scale, name = 'Frequency')

## ggplot
ggplot.item <- ggplot(mapping = aes(Scale, Frequency)) + 
  xlab(label = NULL) + 
  ylab(label = NULL) + 
  theme(plot.title = element_text(hjust = 0.5, size = 5), 
        legend.position = 'none') + 
  scale_y_continuous(
    limits = c(0, 500000), 
    breaks = seq(0, 500000, 100000), 
    labels = c('0', '100000', '200000', '300000', '400000', '500000')
  )

for (item in item.seq$Item) {
  plot.data <- subset(big5.count, Item == item)
  plot.title <- subset(item.seq[, 1:2], Item == item) %>% paste(collapse = ': ')
  plot.colour <- item.seq$Colour[which(item.seq$Item == item)]
  plot.filename <- paste0(item, '.png')
  
  plot.item <- ggplot.item + 
    geom_bar(data = plot.data, stat = 'identity', fill = plot.colour) + 
    labs(title = plot.title)
  
  ggsave(filename = plot.filename, plot = plot.item, path = '03_Outputs/Hist_Plot', 
         width = 4, height = 4)
  assign(item, plot.item)
}

## arranging plot
plot.opn <- OPN1 + OPN2 + OPN3 + OPN4 + OPN5 + OPN6 + OPN7 + OPN8 + OPN9 + OPN10 + 
  plot_layout(nrow = 2)
ggsave(filename = 'OPN.png', plot = plot.opn, path = '03_Outputs', 
       width = 10, height = 4)

plot.csn <- CSN1 + CSN2 + CSN3 + CSN4 + CSN5 + CSN6 + CSN7 + CSN8 + CSN9 + CSN10 + 
  plot_layout(nrow = 2)
ggsave(filename = 'CSN.png', plot = plot.csn, path = '03_Outputs', 
       width = 10, height = 4)

plot.ext <- EXT1 + EXT2 + EXT3 + EXT4 + EXT5 + EXT6 + EXT7 + EXT8 + EXT9 + EXT10 + 
  plot_layout(nrow = 2)
ggsave(filename = 'EXT.png', plot = plot.ext, path = '03_Outputs', 
       width = 10, height = 4)

plot.agr <- AGR1 + AGR2 + AGR3 + AGR4 + AGR5 + AGR6 + AGR7 + AGR8 + AGR9 + AGR10 + 
  plot_layout(nrow = 2)
ggsave(filename = 'AGR.png', plot = plot.agr, path = '03_Outputs', 
       width = 10, height = 4)

plot.est <- EST1 + EST2 + EST3 + EST4 + EST5 + EST6 + EST7 + EST8 + EST9 + EST10 + 
  plot_layout(nrow = 2)
ggsave(filename = 'EST.png', plot = plot.est, path = '03_Outputs', 
       width = 10, height = 4)


##---- Country distribution of participants ----
## country code
country.code <- read_csv('02_Inputs/1.Personality/wikipedia-iso-country-codes.csv')

## count country
country.count <- big5.imp %>% 
  select(country) %>% 
  count(country, name = 'n') %>% 
  mutate(country = ifelse(n >= 10000 & country != 'NONE', country, 'Others')) %>% 
  group_by(country) %>% 
  summarise(n = sum(n, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  left_join(country.code, by = c('country' = 'Alpha-2 code')) %>% 
  select(country = `English short name lower case`, n) %>% 
  mutate(country = ifelse(is.na(country), 'Others', country), 
         country = factor(country, levels = country))

## ggplot
plot.country <- ggplot(country.count, aes(country, n, fill = country)) + 
  xlab(label = NULL) + 
  ylab(label = NULL) + 
  labs(title = 'The Country of participants') + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = 'none') + 
  scale_y_continuous(
    limits = c(0, 550000), 
    breaks = seq(0, 550000, 50000), 
    labels = c('0', '50000', '100000', '150000', '200000', '250000', '300000', 
               '350000', '400000', '450000', '500000', '550000')
  ) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = n, vjust = 'bottom'))

ggsave(filename = 'Country.png', plot = plot.country, path = '03_Outputs', 
       width = 10, height = 4)
