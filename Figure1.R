install.packages("tidyverse")
library(ggplot2)
library(dplyr)
library(tidyverse)

years <- combined[, c(1,25,58)]  

# CUT years into groups 
years$cut <- cut(years$year, breaks = c(0,2005,2006,2007,2008,2009,2010,2011,
                                        2012,2013,2014,2015,2016,2017,2018,2019
                                        ,2020,2021),
                 
                 labels = c("before 2005","2005","2006","2007","2008","2009",
                            "2010","2011","2012","2013","2014","2015","2016",
                            "2017","2018","2019","2020"),
                 right = FALSE)



#replication of figure 1 from Merson et al paper
ggplot(years, aes(x=as.factor(cut), fill=as.factor(IPD_PLAN)))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  scale_y_continuous(labels = scales::percent) +
labs(title = "",
     fill = "Response",
     x = "Year",
     y = "% Of Studies Registered")
















