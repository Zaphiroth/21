# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  21
# Purpose:      The Comparison of Big5 and MBTI
# programmer:   Zhe Liu
# Date:         2022-04-24
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- K-means clustering ----
## 16-means clustering
cluster.km16 <- bigkmeans(
  big5.dim.scale, 
  centers = 16, 
  iter.max = 300
)

## 16-means cluster centers
cluster.km16.center <- cluster.km16$centers * 20 + 30


##---- Describing the clusters ----
## frequency count
cluster.km16.count <- big5.dim %>% 
  mutate(Cluster = cluster.km16$cluster) %>% 
  pivot_longer(cols = c(OPN, CSN, EXT, AGR, EST), 
               names_to = 'Dim', 
               values_to = 'Scale') %>% 
  count(Cluster, Dim, Scale, name = 'Freq')

## cumulative sums
cluster.dim.scale <- merge(
  distinct(cluster.km16.count, Cluster, Dim), 
  distinct(cluster.km16.count, Scale)
)

cluster.km16.prop <- cluster.km16.count %>% 
  right_join(cluster.dim.scale, by = c("Cluster", "Dim", "Scale")) %>% 
  mutate(Freq = ifelse(is.na(Freq), 0, Freq)) %>% 
  arrange(Cluster, Dim, Scale) %>% 
  group_by(Cluster, Dim) %>% 
  mutate(Prop = Freq / sum(Freq), 
         Cum_Prop = cumsum(Freq) / sum(Freq)) %>% 
  ungroup()

## symbol
cluster.km16.symbol <- cluster.km16.prop %>% 
  filter(Scale == 30) %>% 
  mutate(Symbol = ifelse(Cum_Prop >= 0.8, -1, 
                         ifelse(Cum_Prop - Prop < 0.2, 1, 
                                0))) %>% 
  pivot_wider(
    id_cols = Cluster, 
    names_from = Dim, 
    values_from = Symbol
  ) %>% 
  arrange(OPN, CSN, EXT, AGR, EST) %>% 
  select(Cluster, OPN, CSN, EXT, AGR, EST)


##---- Visualization ----
ggplot.cluster.km16 <- ggplot(mapping = aes(factor(Scale, levels = 10:50), Freq)) + 
  xlab(label = NULL) + 
  ylab(label = NULL) + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = 'none', 
        axis.text.y = element_text(size = 5)) + 
  scale_x_discrete(
    limits = factor(10:50, levels = 10:50),
    breaks = factor(seq(10, 50, 10)), 
    labels = c('10', '20', '30', '40', '50')
  ) + 
  scale_y_continuous(
    limits = c(0, 12000), 
    breaks = seq(0, 12000, 3000), 
    labels = c('0', '3000', '6000', '9000', '12000')
  )

for (cluster in 1:16) {
  for (dimension in c('OPN', 'CSN', 'EXT', 'AGR', 'EST')) {
    plot.data <- subset(cluster.km16.count, Cluster == cluster & Dim == dimension)
    # plot.title <- subset(item.seq[, 1:2], Dim == item) %>% paste(collapse = ': ')
    plot.colour <- item.seq$Colour[which(item.seq$Dim == dimension)][1]
    plot.filename <- paste0('Cluster', cluster, '_', dimension, '.png')
    
    plot.cluster.km16 <- ggplot.cluster.km16 + 
      geom_bar(data = plot.data, stat = 'identity', fill = plot.colour) + 
      geom_vline(aes(xintercept = '30'), linetype = "dashed") + 
      labs(title = dimension)
    
    ggsave(filename = plot.filename, plot = plot.cluster.km16, 
           path = '03_Outputs/Cluster16_Plot', width = 4, height = 4)
    # assign(paste0('Cluster', cluster, '.', dimension), plot.cluster.km5)
  }
}

rm(cluster, dimension, plot.data, plot.colour, plot.filename, plot.cluster.km16)

# 3-6, 10-11, 5-13, 14-15

