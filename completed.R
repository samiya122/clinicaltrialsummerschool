#looking at colums with results date posted

completed <- combined[, c(1,33,50,58,61)]

install.packages("ggplot2")  
library("ggplot2")


completed$results_date_posted <-  lubridate::parse_date_time(completed$results_date_posted, orders = c('dmy','ymd'))
class(completed$results_date_posted)

#removes all rows with na values. so only  trials with dates are kept
completed <- na.omit(completed)
talblecomplete <- table(completed$IPD_PLAN)

#bar plot for all completed trials
plot3 <- ggplot(completed, aes(IPD_PLAN, fill = IPD_PLAN,)) +                             
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)


plot3 + ggtitle("IPD sharing plans of all completed" ) +
  xlab("IPD plans") + ylab("percentage")





#looking at those completed after the start of 2019
complete2019 <- completed[completed$results_date_posted >= "2019-01-01",] 
tablecomplete2019 <- table(complete2019$IPD_PLAN)


#bar plot for trials completed after 2019
plot2 <- ggplot(complete2019, aes(IPD_PLAN, fill = IPD_PLAN,)) +                             
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)


plot2 + ggtitle("IPD sharing plans acros completed trials  from 2019") +
  xlab("IPD plans") + ylab("percentage")





#not completed trials  
a<- combined[, c(1,33,50,58,61)]

notcompleted <- a[a$results_date_posted == "NULL",]
tablenotcomplete <- table(notcompleted$IPD_PLAN)


install.packages("scales")                         
library("scales")

plot <- ggplot(notcompleted, aes(IPD_PLAN, fill = IPD_PLAN,)) +                             
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)

  
plot + ggtitle("IPD sharing plans across not completed trials") +
  xlab("IPD plans") + ylab("percentage")


#prop test to compare proportions of complete and incomplete trials
prop.test(x=c(3971,24813 ), n=c(53758,539837), correct = FALSE)



#look at results_yes_column
yes_no<- combined[, c(1,24,33,50,58,61)]

yeses<- yes_no[yes_no$results_yes_no == "\"Yes\"",]
nos<- yes_no[yes_no$results_yes_no == "NULL",]

yeses2019 <- yeses[yeses$Date_Reg >= "2019-01-01",]
tableyeses2019 <- table(yeses2019$IPD_PLAN)



  








