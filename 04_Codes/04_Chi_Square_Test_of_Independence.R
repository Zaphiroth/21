# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  21
# Purpose:      Chi-Square Test of Independence between Dimension and Shape
# programmer:   Zhe Liu
# Date:         2022-04-20
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Creating contingency table ----
## dimension-shape count
shape.dim <- read.xlsx('03_Outputs/分布形状.xlsx') %>% 
  mutate(Dimension = stri_sub(Item, 1, 3))

# shape.dim <- dim.shape %>% 
#   mutate()
#   group_by(Dimension = stri_sub(Item, 1, 3), Shape) %>% 
#   summarise(n = n()) %>% 
#   ungroup() %>% 
#   pivot_wider(
#     id_cols = Dimension, 
#     names_from = Shape, 
#     values_from = n, 
#     values_fill = 0
#   )

## contingency table
shape.dim.con <- table(shape.dim$Dimension, shape.dim$Shape)


##---- Chi-square test of independence ----
set.seed(21)
shape.dim.chisq <- chisq.test(shape.dim.con, simulate.p.value = TRUE)
# many of the expected values are very small, so use ’simulate.p.value = TRUE‘
