FIGURE 2 WHICH IS DONE

# created subset dataframe with all trials in 2019 and 2020
sub1 <- combined[combined$year >= "2019",]
unique(sub1$REG)
tab2 <- table(sub1$REG)
sum(sub1$REG == "NC")

#created a count of the number of each registration in 2019 and 2020
plotd <- sub1 %>% count(REG)

# treemap

install.packages("ggplot2")
install.packages("treemap")
install.packages("treemapify")
library(ggplot2)
library(treemap)
library(treemapify)

#ggplot for treemap
ggplot(plotd, aes(fill = REG,
                  area = n,
                  label= REG)) + geom_treemap() + geom_treemap_text()+
  theme(legend.position = 0.2)
