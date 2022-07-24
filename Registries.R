library(finalfit)
install.packages("expss")  
library(expss)
install.packages("flextable")  
library(flextable)

Y <- cross_cases(all2019, REG, list(total(), complete, complete %nest% IPD_PLAN))
Y[is.na(Y)] = 0

final <- as.data.frame(Y)

#rename columns
final <- final%>% 
  rename(Registry = 1, Total = 2, NOT_COMPLETED = 3, NOT_COMPLETED_NO = 5, 
  NOT_COMPLETED_NO_INFORMATION = 6,  NOT_COMPLETED_UNDECIDED = 7,  
  NOT_COMPLETED_YES =8 , COMPLETED_NO= 9,   COMPLETED_NO_INFORMATION = 10, 
  COMPLETED_UNDECIDED = 11, COMPLETED_YES = 12)


#percentage of each  trial
final <- final%>%group_by(Registry)%>%mutate('%'=paste0(round(Total/132943*100,2),"%"))
final <- final %>% relocate('%', .after = Total)

final <- final%>%group_by(Registry)%>%mutate('COMPLETED %' =paste0(round(COMPLETED/Total*100,2),"%"))
final <- final %>% relocate('COMPLETED %', .after = COMPLETED)


final <- final%>%group_by(Registry)%>%mutate('NOT_COMPLETED %'=paste0(round(NOT_COMPLETED/Total*100,2),"%"))
final <- final %>% relocate('NOT_COMPLETED %', .after = NOT_COMPLETED)


final <- final%>%group_by(Registry)%>%mutate('COMPLETED_NO %'=paste0(round(COMPLETED_NO/COMPLETED*100,2),"%"))
final <- final %>% relocate('COMPLETED_NO %', .after = COMPLETED_NO)


final <- final%>%group_by(Registry)%>%mutate('COMPLETED_NO_INFORMATION %'=paste0(round(COMPLETED_NO_INFORMATION/COMPLETED*100,2),"%"))
final <- final %>% relocate('COMPLETED_NO_INFORMATION %', .after = COMPLETED_NO_INFORMATION)


final <- final%>%group_by(Registry)%>%mutate('COMPLETED_UNDECIDED %'=paste0(round(COMPLETED_UNDECIDED/COMPLETED*100,2),"%"))
final <- final %>% relocate('COMPLETED_UNDECIDED %', .after = COMPLETED_UNDECIDED)


final <- final%>%group_by(Registry)%>%mutate('COMPLETED_YES %'=paste0(round(COMPLETED_YES/COMPLETED*100,2),"%"))
final <- final %>% relocate('COMPLETED_YES %', .after = COMPLETED_YES)


final <- final%>%group_by(Registry)%>%mutate('NOT_COMPLETED_NO %'=paste0(round(NOT_COMPLETED_NO/NOT_COMPLETED*100,2),"%"))
final <- final %>% relocate('NOT_COMPLETED_NO %', .after = NOT_COMPLETED_NO)


final <- final%>%group_by(Registry)%>%mutate('NOT_COMPLETED_NO_INFORMATION %'=paste0(round(NOT_COMPLETED_NO_INFORMATION/NOT_COMPLETED*100,2),"%"))
final <- final %>% relocate('NOT_COMPLETED_NO_INFORMATION %', .after = NOT_COMPLETED_NO_INFORMATION)

final <- final%>%group_by(Registry)%>%mutate('NOT_COMPLETED_UNDECIDED %'=paste0(round(NOT_COMPLETED_UNDECIDED/NOT_COMPLETED*100,2),"%"))
final <- final %>% relocate('NOT_COMPLETED_UNDECIDED %' ,.after = NOT_COMPLETED_UNDECIDED)

final <- final%>%group_by(Registry)%>%mutate('NOT_COMPLETED_YES %'=paste0(round(NOT_COMPLETED_YES/NOT_COMPLETED*100,2),"%"))
final <- final %>% relocate('NOT_COMPLETED_YES %', .after = NOT_COMPLETED_YES)

#replace all NaN% WITH 0%
final[final == "NaN%"] <- "0%"


#remove registries with no completed trials
completed <- final[!(final$COMPLETED == 0),]


#VISUAL display of table
a <- flextable(final)
a <- theme_box(a)
a <- set_caption(a, caption = "IPD Sharing PLans Among Registries")


