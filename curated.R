getwd()
setwd("~/Documents/dataverse_files")

#import raw data set
ICTRP_download_all_registries_but_CT_15DEC20 <- read.csv("ICTRP download all registries but CT 15DEC20.csv")
ICTRP_download_CT_registry_15DEC20 <- read.csv("ICTRP download CT registry 15DEC20.csv")

# import curated data set
ICTRP_Analysis_seperate_Dataset <- read.csv("ICTRP Analysis seperate Dataset.csv")

#merge the data sets
combined <- rbind(ICTRP_download_all_registries_but_CT_15DEC20,ICTRP_download_CT_registry_15DEC20)
combined

#creates dataset with TrialdID and date of registry, too look at data closer
bridge <- combined[, c(1,39,40,41)]

#removes all "child" in bridgred_type column as those with child are linked to
#the parent clinical trial and counted as 1 trial.
bridge2 <- bridge[!(bridge$Bridged_type == "\"child     \""),]
bridge2

combined <- combined[!(combined$Bridged_type == "\"child     \""),]


library(dplyr)

#creates abreviation of all trial registries. first two letters 
combined$REG <- substr(combined$TrialID,1,2)
#change all text in REG to uppercase so there is no longer 2 versions of anything
combined$REG <- toupper(combined$REG)
unique(combined$REG)


#moves coloumn to after TrialID coloumn
combined <- combined %>% relocate(REG, .after = TrialID)

--------
rm(combined)
names(combined)
combined$`(No column name)`
  
comb1 <- com  
  

combined$Date_Reg <-  lubridate::parse_date_time(combined$Date_registration, orders = c('dmy','ymd'))

#replace empty values in dateregistration with date in x.coumn.names
combined$Date_Reg[is.na(combined$Date_Reg)] <-  lubridate::dmy(combined$`(No column name)`, na.rm = TRUE)
which(is.na(combined$Date_Reg))

combined$year <- strftime(combined$Date_Reg, "%Y")
class(combined$year)

#table of number of trials each year
table1 <- table(combined$year, useNA = "ifany")
table1
sum(table1)


#moves coloumn to after date_regisration coloumn
combined <- combined %>% relocate(Date_Reg, .after = Date_registration )
combined <- combined %>% relocate(year, .after = Date_Reg )


#note need to find  a way of removing Date_Reg fromb combined dataset


# used IDP PLAN column from  curated data
combined$IDP_PLAN <- ICTRP_Analysis_seperate_Dataset$`IPD PLAN`
combined <- combined %>% relocate(IDP_PLAN , .after = results_IPD_plan)

#change all text in IDP_PLAN COLUMN to uppercase so there is no longer 2 versions of yes
combined$IDP_PLAN <- toupper(combined$IDP_PLAN)
unique(combined$IDP_PLAN )


table2 <- table(combined$IDP_PLAN)
table2

-----------
 
test1 <- combined[, c(1,25,58)]  

test1$year <- as.numeric(test1$year)  
class(test1$year)  
unique(test1$year)  


                

test1$cut <- cut(test1$year, breaks = c(0,2005,2006,2007,2008,2009,2010,2011,
                                        2012,2013,2014,2015,2016,2017,2018,2019
                                        ,2020,2021),
                 
                 labels = c("before 2005","2005","2006","2007","2008","2009",
                            "2010","2011","2012","2013","2014","2015","2016",
                            "2017","2018","2019","2020"),
                 right = FALSE)
                
  
test1$year <- as.numeric(test1$year)  
class(test1$year)  
unique(test1$year)  

unique(test1$cut)

table3 <- table(test1$cut)
table3 
sum(table3)

library(ggplot2)
library(dplyr)
library(tidyverse)


test1 %>%
ggplot(aes(x = cut,fill = IDP_PLAN)) + geom_bar(stat = "count")
                                                
  
       
barplot(test1$cut, col = c(test1$IDP_PLAN), beside = TRUE)





library(ggplot2)
ggplot(table3, aes(cut), Freq, fill = IDP_PLAN)) +     
  geom_col(position = 'dodge')

# not exactly like figure 1 in the paper. y-axis is different 
counts1 <- table(test1$IDP_PLAN, test1$cut)
barplot(counts1, main = 'IPD sharing plans of all studies included on ICTRP', xlab  = "years", col= c("red","blue","green","orange"),
                                   legend = rownames(counts1), beside = TRUE)


data_perc <- t(prop.table(table(test1$cut,test1$IDP_PLAN))) * 100
data_perc
sum()

barplot(data_perc,ylab = "percent")

barplot(data_perc, main = 'IPD sharing plans of all studies included on ICTRP',
        xlab  = "years",ylab = "percent", col= c("red","blue","green","orange"),
        legend = rownames(data_perc), beside = TRUE)

table3 <- table(test1$cut,test1$IDP_PLAN)


-------------
  FIGURE 2 WHICH IS DONE
  
# made testing subset with TRIALID, REG and YEAR
test2<- combined[, c(1,2,25)] 

#made year numeric
test2$year <- as.numeric(test2$year)  

# created subset dataframe with all trials in 2019
sub1 <-test2[test2$year >= "2019",]
unique(sub1$REG)
tab2 <- table(sub1$REG)
sum(sub1$REG = "NC")

#created a count of the number of each registration in 2019 and 2020
plotd <- sub1 %>% count(REG)

# treemap
library(treemap)
library(treemapify)


#ggplot for treemap
ggplot(plotd, aes(fill = REG,
                  area = n,
                  label= REG)) + geom_treemap() + geom_treemap_text()
+ theme(legend.position = FALSE)

----------------
  figure 4 

names(ICTRP_Analysis_seperate_Dataset)
class(ICTRP_Analysis_seperate_Dataset$`DATE REG`)
names(combined)
test4 <- ICTRP_Analysis_seperate_Dataset[, c(11,12,13)]
test4$year <- combined$year
test4$date <- combined$Date_Reg


test3 <-test4[test4$date > "2019-01-01",]


rm(test4)
unique(test4$`Primary Sp Look up` )
unique(test4$`Sec SP Look up`)
unique(test4$`Funder Look up`)

rm(test3)
test3 <-test4[test4$`DATE REG` >= "2019",]
rm(test3)
#change commercial ans no commmercial to uppercase
test3$`Primary Sp Look up` <- toupper(test3$`Primary Sp Look up`)
test3$`Sec SP Look up` <- toupper(test3$`Sec SP Look up`)
test3$`Funder Look up` <- toupper(test3$`Funder Look up`)
unique(test3$`Primary Sp Look up` )
unique(test3$`Sec SP Look up`)
unique(test3$`Funder Look up`)


table5 <- table(test3$`Primary Sp Look up`)
table6 <- table(test3$`Sec SP Look up`)
table7 <-  table(test3$`Funder Look up`)

sum(table5)  
  
test3 <- test3[test3$`Primary Sp Look up` != "BLANK" & 
                 test3$`Sec SP Look up` != "BLANK" ,]
             & test3$`Funder Look up` != "BLANK",]
  
  !(combined$Bridged_type == "\"child     \""),]
  
sum(test3 == "COMMERCIAL")                         # Count in all columns

TESTtable <- table(test3$`Primary Sp Look up`,useNA = "ifany")  
table1 <- table(combined$year, useNA = "ifany")
table1
sum(table1)

is.na(test3)
sum(is.na(test3))
sum(is.na(test4$`DATE REG`))
sum(is.na(ICTRP_Analysis_seperate_Dataset$`DATE REG`))
sum(is.na())

unique(ICTRP_Analysis_seperate_Dataset$`DATE REG`)
tab <- table(ICTRP_Analysis_seperate_Dataset$`DATE REG`)
sum(tab)
