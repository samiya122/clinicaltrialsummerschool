a <- combined[, c(1,2,24,25,32,33,50,58,61,68,69,70,71,72,73,74,75,76,77,78,79,80)]

#all trials registered in 2019 and 2020
all2019 <- new[new$Date_Reg >= "2019-01-01",]

all2019$complete  <- all2019$results_date_posted

# all trials with date are equated to complete
all2019$complete[all2019$complete != "NULL"] = "COMPLETED" 
#trials without date are equated to incomplete
all2019$complete[all2019$complete == "NULL"] = "NOT COMPLETED" 

tableall2019 <- table(all2019$complete, all2019$IPD_PLAN)

#swap order of bars on x-axis
all2019$complete <- factor(all2019$complete, levels = c("NOT COMPLETED","COMPLETED"))
