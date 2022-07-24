install.packages("ggplot2")
install.packages("treemap")
install.packages("treemapify")
library(ggplot2)
library(treemap)
library(treemapify)

#created a count of the number of each registration in 2019 and 2020
plotd <- all2019 %>% count(REG)


#treemap
#replication of figure2 from Merson et al paper
ggplot(plotd, aes(fill = REG,
                  area = n,
                  label= REG)) + geom_treemap() + geom_treemap_text()+
theme(legend.position = 0.2)
