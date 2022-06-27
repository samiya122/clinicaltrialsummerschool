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
combined <- combined[!(combined$Bridged_type == "\"child     \""),]


library(dplyr)

#creates abbreviation of all trial registries. first two letters 
combined$REG <- substr(combined$TrialID,1,2)
#change all text in REG to uppercase so there is no longer 2 versions of anything
combined$REG <- toupper(combined$REG)
unique(combined$REG)

#moves coloumn to after TrialID coloumn
combined <- combined %>% relocate(REG, .after = TrialID)



combined$Date_Reg <-  lubridate::parse_date_time(combined$Date_registration, orders = c('dmy','ymd'))
#replace empty values in dateregistration with date in x.coumn.names
combined$Date_Reg[is.na(combined$Date_Reg)] <-  lubridate::dmy(combined$X.No.column.name., na.rm = TRUE)
#counts if there is any NA
sum(which(is.na(combined$Date_Reg)))

#creates column of just the year of registration
combined$year <- strftime(combined$Date_Reg, "%Y")
class(combined$year)
combined$year <- as.numeric(combined$year)  

#moves column to after date_registration coloumn
combined <- combined %>% relocate(Date_Reg, .after = Date_registration )
combined <- combined %>% relocate(year, .after = Date_Reg )


# used IDP PLAN column from  curated data
combined$IDP_PLAN <-ICTRP_Analysis_seperate_Dataset$IPD.PLAN
combined <- combined %>% relocate(IDP_PLAN , .after = results_IPD_plan)


#change all text in IDP_PLAN COLUMN to uppercase so there is no longer 2 versions of yes
combined$IDP_PLAN <- toupper(combined$IDP_PLAN)
unique(combined$IDP_PLAN )



 
