# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  21
# Purpose:      K-Means Clustering
# programmer:   Zhe Liu
# Date:         2022-04-22
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Scaling dimension scores ----
big5.dim.scale <- big5.dim[, 1:5] %>% 
  mutate_all(function(x) {(x - 30) / 20}) %>% 
  as.matrix()

# big5.nc <- NbClust(
#   as.matrix(big5.dim.sample), 
#   distance = 'minkowski', 
#   max.nc = 16, 
#   method = 'kmeans'
# )


##---- Determining the number of Clusters ----
## indices by bagging method
nc.list <- list()

for(i in 1:10) {
  print(i)
  
  set.seed(i+21)
  nb.sample <- big5.dim.scale[sample(nrow(big5.dim.scale), 1000), ]
  
  nc.list[[i]] <- NbClust(
    nb.sample, 
    distance = 'euclidean', 
    min.nc = 5, 
    max.nc = 32, 
    method = 'kmeans'
  )
}

rm(i, nb.sample)

## determining the number of Clusters
NCVoteFunc <- function(data) {
  data$Best.nc[1, ] %>% 
    table() %>% 
    which.max() %>% 
    names() %>% 
    as.numeric()
}

big5.nc <- nc.list %>% 
  lapply(NCVoteFunc) %>% 
  unlist()

print(big5.nc) # Best number of clusters is 5

# big5.nc <- fviz_nbclust(
#   big5.dim.sample,
#   kmeans,
#   method = 'wss',
#   diss = dist(big5.dim.sample, method = 'euclidean'),
#   k.max = 32
# )
# 
# big5.elbow <- big5.nc + 
#   geom_vline(xintercept = 10, linetype = 2) + 
#   labs(subtitle = 'Elbow Method')

##---- K-means clustering ----
## 5-means clustering
cluster.km5 <- bigkmeans(
  big5.dim.scale, 
  centers = 5, 
  iter.max = 100
)

## 5-means cluster centers
cluster.km5.center <- cluster.km5$centers * 20 + 30


##---- Visualizing 5-means clustering result ----
## Scale distributions of clusters
cluster.km5.count <- big5.dim %>% 
  mutate(Cluster = cluster.km5$cluster) %>% 
  pivot_longer(cols = c(OPN, CSN, EXT, AGR, EST), 
               names_to = 'Dim', 
               values_to = 'Scale') %>% 
  count(Cluster, Dim, Scale, name = 'Freq')

## ggplot
ggplot.cluster.km5 <- ggplot(mapping = aes(factor(Scale, levels = 10:50), Freq)) + 
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
    limits = c(0, 40000), 
    breaks = seq(0, 40000, 10000), 
    labels = c('0', '10000', '20000', '30000', '40000')
  )

for (cluster in 1:5) {
  for (dimension in c('OPN', 'CSN', 'EXT', 'AGR', 'EST')) {
    plot.data <- subset(cluster.km5.count, Cluster == cluster & Dim == dimension)
    # plot.title <- subset(item.seq[, 1:2], Dim == item) %>% paste(collapse = ': ')
    plot.colour <- item.seq$Colour[which(item.seq$Dim == dimension)][1]
    plot.filename <- paste0('Cluster', cluster, '_', dimension, '.png')
    
    plot.cluster.km5 <- ggplot.cluster.km5 + 
      geom_bar(data = plot.data, stat = 'identity', fill = plot.colour) + 
      geom_vline(aes(xintercept = '30'), linetype = "dashed") + 
      labs(title = dimension)
    
    ggsave(filename = plot.filename, plot = plot.cluster.km5, 
           path = '03_Outputs/Cluster5_Plot', width = 4, height = 4)
    assign(paste0('Cluster', cluster, '.', dimension), plot.cluster.km5)
  }
}

rm(cluster, dimension, plot.data, plot.colour, plot.filename, plot.cluster.km5)

## arrange plot
plot.cluster1 <- Cluster1.OPN + Cluster1.CSN + Cluster1.EXT + Cluster1.AGR + Cluster1.EST + 
  plot_layout(nrow = 1)
ggsave(filename = 'Cluster1.png', plot = plot.cluster1, path = '03_Outputs', 
       width = 10, height = 2)

plot.cluster2 <- Cluster2.OPN + Cluster2.CSN + Cluster2.EXT + Cluster2.AGR + Cluster2.EST + 
  plot_layout(nrow = 1)
ggsave(filename = 'Cluster2.png', plot = plot.cluster2, path = '03_Outputs', 
       width = 10, height = 2)

plot.cluster3 <- Cluster3.OPN + Cluster3.CSN + Cluster3.EXT + Cluster3.AGR + Cluster3.EST + 
  plot_layout(nrow = 1)
ggsave(filename = 'Cluster3.png', plot = plot.cluster3, path = '03_Outputs', 
       width = 10, height = 2)

plot.cluster4 <- Cluster4.OPN + Cluster4.CSN + Cluster4.EXT + Cluster4.AGR + Cluster4.EST + 
  plot_layout(nrow = 1)
ggsave(filename = 'Cluster4.png', plot = plot.cluster4, path = '03_Outputs', 
       width = 10, height = 2)

plot.cluster5 <- Cluster5.OPN + Cluster5.CSN + Cluster5.EXT + Cluster5.AGR + Cluster5.EST + 
  plot_layout(nrow = 1)
ggsave(filename = 'Cluster5.png', plot = plot.cluster5, path = '03_Outputs', 
       width = 10, height = 2)

## describing the clusters
cluster.km5.prop <- cluster.km5.count %>% 
  arrange(Cluster, Dim, Scale) %>% 
  group_by(Cluster, Dim) %>% 
  mutate(Prop = Freq / sum(Freq), 
         Cum_Prop = cumsum(Freq) / sum(Freq)) %>% 
  ungroup()

write.xlsx(cluster.km5.prop, '03_Outputs/Cluster_5_Distribution.xlsx')




## selecting sample to be visualized
VisualSampleFunc <- function(object, n = 1000, method = 'random') {
  if (method == 'random') {
    sample.index <- sample(length(object$cluster), n)
  } else if (method == 'head') {
    sample.index <- 1:min(n, length(object$cluster))
  } else {
    return("Wrong method. ")
  }
  
  object$cluster <- object$cluster[sample.index]
  object$withinss <- object$withinss[sample.index]
  object$size <- NaN
  
  return(list(index = sample.index, object = object))
}

## 5-means clustering reslult
# visual.sample <- VisualSampleFunc(cluster.km5)

# plot.km5 <- fviz_cluster(
#   visual.sample$object,
#   big5.dim.scale[visual.sample$index, ],
#   ellipse = TRUE,
#   ellipse.type = 'euclid',
#   stand = FALSE,
#   repel = TRUE
# ) +
#   geom_hline(aes(yintercept = 0), linetype = "dashed") +
#   geom_vline(aes(xintercept = 0), linetype = "dashed")

# plot.km16 <- fviz_cluster(
#   cluster.km16, 
#   big5.dim.scale, 
#   ellipse = TRUE, 
#   ellipse.type = 'euclid', 
#   stand = FALSE, 
#   repel = TRUE
# ) +
#   geom_hline(aes(yintercept = 0), linetype = "dashed") +
#   geom_vline(aes(xintercept = 0), linetype = "dashed")


##---- K-means clustering ----
## big k-means clustering function
BKMFunc <- function(i, iter, mtx = big5.dim.scale) {
  cluster.km <- bigkmeans(
    mtx, 
    centers = i, 
    iter.max = iter
  )
  
  cluster.list[[i]] <<- cluster.km$withinss
  cluster.wss[i] <<- sum(cluster.km$withinss)
}

## determining the number of Clusters
cluster.list <- list()
cluster.wss <- c()

BKMFunc(5, 100)
BKMFunc(6, 100)
BKMFunc(7, 100)
BKMFunc(8, 100)
BKMFunc(9, 150)
BKMFunc(10, 150)
BKMFunc(11, 150)
BKMFunc(12, 150)
BKMFunc(13, 200)
BKMFunc(14, 200)
BKMFunc(15, 200)
BKMFunc(16, 200)
BKMFunc(17, 250)
BKMFunc(18, 250)
BKMFunc(19, 250)
BKMFunc(20, 250)
BKMFunc(21, 300)
BKMFunc(22, 300)
BKMFunc(23, 300)
BKMFunc(24, 300)
BKMFunc(25, 350)
BKMFunc(26, 350)
BKMFunc(27, 350)
BKMFunc(28, 350)
BKMFunc(29, 400)
BKMFunc(30, 400)
BKMFunc(31, 400)
BKMFunc(32, 400)

plot(1:32, cluster.wss)

## 16-means clustering
cluster.km16 <- bigkmeans(
  big5.dim.scale, 
  centers = 16, 
  iter.max = 250
)

plot.km16 <- fviz_cluster(
  cluster.km16, 
  big5.dim.scale, 
  ellipse = TRUE, 
  ellipse.type = 'euclid', 
  stand = FALSE, 
  repel = TRUE
) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_vline(aes(xintercept = 0), linetype = "dashed")

## 32-means clustering
cluster.km32 <- bigkmeans(
  big5.dim.scale, 
  centers = 32, 
  iter.max = 500
)

plot.km32 <- fviz_cluster(
  cluster.km32, 
  big5.dim.scale, 
  ellipse = TRUE, 
  ellipse.type = 'euclid', 
  stand = FALSE, 
  repel = TRUE
) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_vline(aes(xintercept = 0), linetype = "dashed")












