# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  21
# Purpose:      Data Visualization
# programmer:   Zhe Liu
# Date:         2022-04-17
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Scale distributions of items ----
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
  count(Item, Scale, name = 'Freq')

## ggplot
ggplot.item <- ggplot(mapping = aes(Scale, Freq)) + 
  xlab(label = NULL) + 
  ylab(label = NULL) + 
  theme(plot.title = element_text(hjust = 0.5, size = 5), 
        legend.position = 'none', 
        axis.text.y = element_text(size = 5)) + 
  scale_y_continuous(
    limits = c(0, 500000), 
    breaks = seq(0, 500000, 100000), 
    labels = c('0', '100000', '200000', '300000', '400000', '500000')
  )

for (item in item.seq$Item) {
  plot.data <- subset(big5.count, Item == item)
  plot.title <- subset(item.seq[, 2:3], Item == item) %>% paste(collapse = ': ')
  plot.colour <- item.seq$Colour[which(item.seq$Item == item)]
  plot.filename <- paste0(item, '.png')
  
  plot.item <- ggplot.item + 
    geom_bar(data = plot.data, stat = 'identity', fill = plot.colour) + 
    labs(title = plot.title)
  
  ggsave(filename = plot.filename, plot = plot.item, path = '03_Outputs/Hist_Plot', 
         width = 4, height = 4)
  assign(item, plot.item)
}

rm(item, plot.data, plot.title, plot.colour, plot.filename, plot.item)

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


##---- Score distributions of dimensions
chk <- big5.dim[, 1:5] %>% 
  pivot_longer(cols = everything(), names_to = 'dim', values_to = 'score') %>% 
  group_by(dim, score) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  filter(dim == 'EST')

plot(chk$score, chk$num)


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
         country = factor(country, levels = country)) %>% 
  filter(country != 'Others')

## ggplot
plot.country <- ggplot(country.count, aes(country, n, fill = country)) + 
  xlab(label = NULL) + 
  ylab(label = NULL) + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = 'none') + 
  scale_y_continuous(
    limits = c(0, 550000), 
    breaks = seq(0, 550000, 50000), 
    labels = c('0', '50000', '100000', '150000', '200000', '250000', '300000', 
               '350000', '400000', '450000', '500000', '550000')
  ) + 
  guides(x = guide_axis(angle = 45)) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = n, vjust = 'bottom'))

ggsave(filename = 'Country.png', plot = plot.country, path = '03_Outputs', 
       width = 6, height = 4)


##---- Correlation plot ----
## correlation matrix of items
cor.item <- cor(big5.imp[, 1:50])
cmp.item <- cor_pmat(big5.imp[, 1:50])

cor.item.check <- as.data.frame(as.table(cor.item)) %>% 
  rename(Item1 = Var1, Item2 = Var2, Cor = Freq) %>% 
  mutate(Item1 = as.character(Item1), 
         Item2 = as.character(Item2), 
         Item11 = ifelse(Item1 < Item2, Item1, Item2), 
         Item22 = ifelse(Item1 > Item2, Item1, Item2)) %>% 
  unite(Items, Item11, Item22, remove = FALSE) %>% 
  distinct(Items, Item11, Item22, Cor) %>% 
  filter(Cor < 1, stri_sub(Item11, 1, 3) != stri_sub(Item22, 1, 3)) %>% 
  mutate(Cor_abs = abs(Cor)) %>% 
  arrange(desc(Cor_abs))

## corrplot of items
corrplot.item <- ggcorrplot(
  cor.item, 
  type = 'lower', 
  show.diag = TRUE, 
  p.mat = cmp.item, 
  sig.level = 0.05, 
  insig = 'blank', 
  tl.srt = 0
) + 
  scale_x_discrete(
    breaks = c('OPN7', 'CSN7', 'EXT7', 'AGR7', 'EST7'), 
    labels = c('OPN', 'CSN', 'EXT', 'AGR', 'EST')
  ) + 
  scale_y_discrete(
    breaks = c('OPN6', 'CSN6', 'EXT6', 'AGR6', 'EST6'), 
    labels = c('OPN', 'CSN', 'EXT', 'AGR', 'EST')
  )

ggsave(filename = 'Corrplot_Item.png', plot = corrplot.item, path = '03_Outputs', 
       width = 6, height = 5)

## correlation matrix of dimensions
scale.reverse <- c('OPN2', 'OPN4', 'OPN6', 
                   'CSN2', 'CSN4', 'CSN6', 'CSN8', 
                   'EST2', 'EST4', 
                   'AGR1', 'AGR3', 'AGR5', 'AGR7', 
                   'EST2', 'EST4')

big5.dim <- big5.imp %>% 
  mutate_at(scale.reverse, function(x) {6 - x}) %>% 
  mutate(
    OPN = OPN1 + OPN2 + OPN3 + OPN4 + OPN5 + OPN6 + OPN7 + OPN8 + OPN9 + OPN10, 
    CSN = CSN1 + CSN2 + CSN3 + CSN4 + CSN5 + CSN6 + CSN7 + CSN8 + CSN9 + CSN10, 
    EXT = EXT1 + EXT2 + EXT3 + EXT4 + EXT5 + EXT6 + EXT7 + EXT8 + EXT9 + EXT10, 
    AGR = AGR1 + AGR2 + AGR3 + AGR4 + AGR5 + AGR6 + AGR7 + AGR8 + AGR9 + AGR10, 
    EST = EST1 + EST2 + EST3 + EST4 + EST5 + EST6 + EST7 + EST8 + EST9 + EST10
  ) %>% 
  select(OPN, CSN, EXT, AGR, EST, country, ID, NMV)

cor.dim <- cor(big5.dim[, 1:5])
cmp.dim <- cor_pmat(big5.dim[, 1:5])

cor.dim.check <- as.data.frame(as.table(cor.dim)) %>% 
  rename(Dim1 = Var1, Dim2 = Var2, Cor = Freq) %>% 
  mutate(Dim1 = as.character(Dim1), 
         Dim2 = as.character(Dim2), 
         Dim11 = ifelse(Dim1 < Dim2, Dim1, Dim2), 
         Dim22 = ifelse(Dim1 > Dim2, Dim1, Dim2)) %>% 
  unite(Dims, Dim11, Dim22, remove = FALSE) %>% 
  distinct(Dims, Dim11, Dim22, Cor) %>% 
  filter(Cor < 1, stri_sub(Dim11, 1, 3) != stri_sub(Dim22, 1, 3)) %>% 
  mutate(Cor_abs = abs(Cor)) %>% 
  arrange(desc(Cor_abs))

## corrplot of dimensions
corrplot.dim <- ggcorrplot(
  cor.dim, 
  type = 'lower', 
  show.diag = TRUE, 
  lab = TRUE, 
  p.mat = cmp.dim, 
  sig.level = 0.05, 
  insig = 'blank', 
  tl.srt = 45
)

ggsave(filename = 'Corrplot_Dimension.png', plot = corrplot.dim, path = '03_Outputs', 
       width = 6, height = 5)
