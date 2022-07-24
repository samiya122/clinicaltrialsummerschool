install.packages("ggplot2")  
library("ggplot2")
install.packages("scales")                         
library("scales")
library(ggpubr)

#looking at columns with results date posted
a <- combined[, c(1,2,24,25,32,33,50,58,61,68,69,70,71,72,73,74,75,76,77,78,79,80)]

#completed vs incomplete of all trials from 2019
pl <- ggplot(data = all2019 , aes(x = complete, fill = IPD_PLAN)) +
geom_bar(stat = "count", position = "fill") +
scale_y_continuous(labels = scales::percent)+
scale_x_discrete(guide = guide_axis(n.dodge=1)) +
scale_fill_manual(values = c("orange","dark grey","yellow","blue")) +
labs(title = "Complted vs Incomplete Trials Registered in 
2019 & 2020",
                fill = "Response",
                x = "",
                y = ""
                )+
theme_bw()


#comparing proportions of yeses in complete and incomplete trials  
prop.test(x=c(645,14212), n=c(3792,129151), alternative = "greater" ,correct = FALSE)

#comparing proportions of yeses in complete trials and Merson et al paper
prop.test(x=c(645,14854), n=c(3792,132545), alternative = "greater" ,correct = FALSE)



#12 WHO emerging infections 
whoconditionstrials <- all2019[grep("ebola|nipah|crimean|zika|Marburg|lassa |middle east[a-z]* r|rift v|covid|corona |coronav|sars?-? ?cov|Severe Acute Respiratory syndrome"
, all2019$Conditions, ignore.case = TRUE),]

tablewhoconditions <- table( whoconditionstrials$complete)  

#swap order of bars on x-axis
whoconditionstrials$complete <- factor(whoconditionstrials$complete, levels = c("NOT COMPLETED","COMPLETED"))

tablewhoconditions <- table(whoconditionstrials$complete, whoconditionstrials$IPD_PLAN)

wh <- ggplot(data = whoconditionstrials , aes(x = complete, fill = IPD_PLAN)) +
geom_bar(stat = "count", position = "fill")+
scale_y_continuous(labels = scales::percent) +
scale_x_discrete(guide = guide_axis(n.dodge=1)) +
scale_fill_manual(values = c("orange","dark grey","yellow","blue")) +
labs(title = "Completed vs Incomplete WHO Emerging Infection 
Trials Registered 2019 & 2020",
                fill = "Response",
                x = "",
                y = ""
)+ theme_bw()

#WHO CONDITIONS from 2015 
all2015 <- new[new$Date_Reg >= "2015-01-01",]

all2015$complete  <- all2015$results_date_posted

# all trials with date are equated to complete
all2015$complete[all2015$complete != "NULL"] = "COMPLETED" 
#trials without date are equated to incomplete
all2015$complete[all2015$complete == "NULL"] = "NOT COMPLETED" 


whoconditionstrials2015 <- all2015[grep("ebola|nipah|crimean|zika|Marburg|lassa |middle east[a-z]* r|rift v|covid|corona |coronav|sars?-? ?cov|Severe Acute Respiratory syndrome"
                                    , all2015$Conditions, ignore.case = TRUE),]

tablewhoconditions2015 <- table( whoconditionstrials2015$complete)  




#covid only stdies
covid <- all2019[grep("Middle East Respiratory Syndrome Coronavirus|covid|corona |coronav|sars?-? ?cov"
                                    , all2019$Conditions, ignore.case = TRUE),]

tablecovid <- table(covid$complete,covid$IPD_PLAN)

cv <- ggplot(data = covid, aes(x = complete, fill = IPD_PLAN)) +
geom_bar(stat = "count", position = "fill")+
 scale_y_continuous(labels = scales::percent) +
 scale_x_discrete(guide = guide_axis(n.dodge=1)) +
 scale_fill_manual(values = c("orange","dark grey","yellow","blue")) +
labs(title = "Completed vs Not Completed Covid Related Trials
Registered 2019 & 2020",
                fill = "Response",
                x = "",
                y = "") +
theme_bw()


#cancer related trials  
cancertrials <- all2019[grep("\"ALL\"|\"AML\"|\"Mets\"|Mets,|Astrocytoma|bcc|Blastoma|cancer|Carcinoma|Carcinosarcoma|Carcinoid|Cholangiocarcinoma|cll|cml|DLBCL|malignant|tumour|tumour|Leukaemia|Leukemia|Lymphoma|MDS|Melanoma|Meningioma|Metastases|Metastatic|Mycosis fungoides|Myeloma|nhl|NSCLC|Neuroblastoma|Non?-? Hodgkin|Osteosarcoma|Retinoblastoma|Ependymoma|gestational trophoblastic disease|gbm|Glioblastoma|Gastrointestinal Stromal Tumor|hcc|Hodgkin|Kaposi", 
                       all2019$Conditions, ignore.case = TRUE),]


tablecancer <- table(cancertrials$complete, cancertrials$IPD_PLAN)

#swap order of bars on x-axis
cancertrials$complete <- factor(cancertrials$complete, levels = c("NOT COMPLETED","COMPLETED"))

cn <- ggplot(data = cancertrials, aes(x = complete, fill = IPD_PLAN)) +
geom_bar(stat = "count", position = "fill") +
scale_y_continuous(labels = scales::percent)+
scale_x_discrete(guide = guide_axis(n.dodge=1))+
scale_fill_manual(values = c("orange","dark grey","yellow","blue"))+
labs(title = "Completed vs Not Completed Cancer Trials Registered 
2019 & 2020",
                fill = "Response",
                x = "",
                y = "")+
theme_bw()
          
prop.test(x=c(23,997), n=c(108,11119), alternative = "greater" ,correct = FALSE)


#HIV
HIVtrials <- all2019[grep("\"AIDs\"|HIV|?\\AIDS|? AIDS|?;aids|Human immunodeficiency virus|Acquired Immunodeficiency Syndrome", all2019$Conditions, ignore.case = TRUE),]


TABLEHIV <- table(HIVtrials$complete,HIVtrials$IPD_PLAN)

#swap order of bars on x-axis
HIVtrials$complete <- factor(HIVtrials$complete, levels = c("NOT COMPLETED","COMPLETED"))


hv <- ggplot(data = HIVtrials , aes(x = complete, fill = IPD_PLAN))+
geom_bar(stat = "count", position = "fill") +
scale_y_continuous(labels = scales::percent)+
scale_x_discrete(guide = guide_axis(n.dodge=1))+
scale_fill_manual(values = c("orange","dark grey","yellow","blue"))+
labs(title = "HIV/AIDS Completed vs Not Complete Trials Registered
2019 & 2020",
                fill = "Response",
                x = "",
                y = "")+
theme_bw()


#combine all graphs onto 1 figure
figure <-ggarrange(
  pl, cn ,wh, hv, labels = ,
  common.legend = TRUE, legend = "bottom"
)

  