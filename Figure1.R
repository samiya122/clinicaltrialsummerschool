
test1 <- combined[, c(1,25,58)]  
class(test1$year)  
unique(test1$year)  



# CUT years into groups 
test1$cut <- cut(test1$year, breaks = c(0,2005,2006,2007,2008,2009,2010,2011,
                                        2012,2013,2014,2015,2016,2017,2018,2019
                                        ,2020,2021),
                 
                 labels = c("before 2005","2005","2006","2007","2008","2009",
                            "2010","2011","2012","2013","2014","2015","2016",
                            "2017","2018","2019","2020"),
                 right = FALSE)




table3 <- table(test1$cut)
table3 
sum(table3)

install.packages("tidyverse")
library(ggplot2)
library(dplyr)
library(tidyverse)



# not exactly like figure 1 in the paper. y-axis is different 
counts1 <- table(test1$IDP_PLAN, test1$cut)
barplot(counts1, main = 'IPD sharing plans of all studies included on ICTRP', xlab  = "years", col= c("red","blue","green","orange"),
        legend = rownames(counts1), beside = TRUE)

# change y axis to percentage but still not right
data_perc <- t(prop.table(table(test1$cut,test1$IDP_PLAN))) * 100
data_perc
sum()

barplot(data_perc, main = 'IPD sharing plans of all studies included on ICTRP',
        xlab  = "years",ylab = "percent", col= c("red","blue","green","orange"),
        legend = rownames(data_perc), beside = TRUE)

table7 <- table(test1$cut,test1$IDP_PLAN)
table7 

