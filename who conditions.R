install.packages("ggplot2")
install.packages("scales")
library(ggplot2)
library(scales)

#subset of trailid and conditons 
new <- combined[, c(1,24,33,58,60,61)] 

#subset of all trials concerning 12  WHO conditions
whoconditions <- new[grep("ebola|nipah|crimean|zika|Marburg|lassa |middle east[a-z]* r|rift v|covid|corona |coronav|sars?-? ?cov|Severe Acute Respiratory syndrome"
                           , new$Conditions, ignore.case = TRUE),]


#checks for any duplicate trial ids
any(duplicated(whoconditions$TrialID))


#data set extracting curated column of conditions to compare 
condtions <- ICTRP_Analysis_seperate_Dataset[, c(1,16)] 
unique(condtions$CONDITION)
table1 <- table(condtions$CONDITION)

#create subset with all 12 WHO conditions to compare with WHO Conditions subset
notother <- condtions[condtions$CONDITION != "OTHER",]


#convert results_date_completed from character class to date class
whoconditions$results_date_completed<-  lubridate::parse_date_time(whoconditions$results_date_completed, orders = c('dmy','ymd'))
(is.na(whoconditions$date))

tablewhoconditions <- table(whoconditions$IPD_PLAN)


#removes all rows with na values. so only  trials with dates are kept
complete_whoconditions<- na.omit(whoconditions)

tablecomplete_whoconditions <- table(complete_whoconditions$IPD_PLAN)
tablecomplete_whoconditions

plot5 <- ggplot(complete_whoconditions, aes(IPD_PLAN, fill = IPD_PLAN,)) +                             
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)


plot5 + ggtitle("12 emerging infections completed trials") +
  xlab("IPD plans") + ylab("percentage")

-------------

incomplete <- new[new$results_date_completed == "NULL",]


incompletewho <- incomplete[grep("ebola|nipah|crimean|zika|Marburg|lassa |middle east[a-z]* r|rift v|covid|corona |coronav|sars?-? ?cov|Severe Acute Respiratory syndrome"
                           , incomplete$Conditions, ignore.case = TRUE),]

tableincompletewho <- table(incompletewho$IPD_PLAN)

plot6 <- ggplot(incompletewho, aes(IPD_PLAN, fill = IPD_PLAN,)) +                             
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)


plot6 + ggtitle(" incomplete trial of 12 who emerging infections") +
  xlab("IPD plans") + ylab("percentage")


#prop test for 12 who emerging infections
prop.test(x=c(91,786), n=c(310,6104),alternative = "greater" ,correct = FALSE)

---------
#subset of data of cancer
cancer <- new[grep("cancer", new$Conditions, ignore.case = TRUE),]

#convert results_date_completed from character class to date class
cancer$results_date_completed <-  lubridate::parse_date_time(cancer$results_date_completed , orders = c('dmy','ymd'))

cancer_complete <- na.omit(cancer)


tablecancer_complete <- table(cancer_complete$IPD_PLAN)


plot7 <- ggplot(cancer_complete, aes(IPD_PLAN, fill = IPD_PLAN,)) +                             
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)


plot7 + ggtitle(" complete trial concerning cancer") +
  xlab("IPD plans") + ylab("percentage")


#incomplete cancer related trials
incompletecancer<-incomplete[grep("cancer", incomplete$Conditions, ignore.case = TRUE),]

tableincompletecancer <- table(incompletecancer$IPD_PLAN)

1695 /55180

plot8 <- ggplot(incompletecancer, aes(IPD_PLAN, fill = IPD_PLAN,)) +                             
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)


plot8 + ggtitle(" incomplete trial concerning cancer") +
  xlab("IPD plans") + ylab("percentage")




  
#prop test for cancer completed and incomplete trials
prop.test(x=c(497,1695), n=c(4364,55180),alternative = "greater" ,correct = FALSE)




