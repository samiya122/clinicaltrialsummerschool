install.packages("ggplot2")  
library("ggplot2")

install.packages("scales")                         
library("scales")

library(ggpubr)

#looking at columns with results date posted
new <- combined[, c(1,2,24,33,50,58,61)]


#all trials registered in 2019 nd 2020
all2019 <- new[new$Date_Reg >= "2019-01-01",]


#stacked bar chart    
all2019$complete  <- all2019$results_date_posted

# all trials with date are equated to complete
all2019$complete[all2019$complete != "NULL"] = "COMPLETED" 
#trials without date are equated to incomplete
all2019$complete[all2019$complete == "NULL"] = "NOT COMPLETED" 

  
pl <- ggplot(data = all2019 , aes(x = complete, fill = IPD_PLAN))
pl <- pl + geom_bar(stat = "count", position = "fill")
pl <- pl + scale_y_continuous(labels = scales::percent)
pl <- pl +  scale_x_discrete(guide = guide_axis(n.dodge=1))
pl <- pl + scale_fill_manual(values = c("orange","dark grey","yellow","blue"))
pl <- pl + ggtitle("All Trials")  +
    xlab("" )  + ylab("")

#comaparing propertions of yeses in complete and incomplete trials  
prop.test(x=c(645,14212), n=c(3792,129151), alternative = "greater" ,correct = FALSE)

#12 WHO emerging infections  
whoconditions <- all2019[grep("ebola|nipah|crimean|zika|Marburg|lassa |middle east[a-z]* r|rift v|covid|corona |coronav|sars?-? ?cov|Severe Acute Respiratory syndrome"
, all2019$Conditions, ignore.case = TRUE),]

tablewhoconditions <- table( whoconditions$complete)  
  
wh <- ggplot(data = whoconditions , aes(x = complete, fill = IPD_PLAN))
wh <- wh + geom_bar(stat = "count", position = "fill")
wh <- wh + scale_y_continuous(labels = scales::percent)
wh <- wh +  scale_x_discrete(guide = guide_axis(n.dodge=1))
wh <- wh + scale_fill_manual(values = c("orange","dark grey","yellow","blue"))
wh <- wh + ggtitle(" WHO Conditions")  + xlab("" )  + ylab("")


#cancer related trials  
cancer <- all2019[grep("\"ALL\"|\"AML\"|\"Mets\"|Mets,|Astrocytoma|bcc|Blastoma|cancer|Carcinoma|Carcinosarcoma|Carcinoid|Cholangiocarcinoma|cll|cml|DLBCL|malignant|tumour|tumour|Leukaemia|Leukemia|Lymphoma|MDS|Melanoma|Meningioma|Metastases|Metastatic|Mycosis fungoides|Myeloma|nhl|NSCLC|Neuroblastoma|Non?-? Hodgkin|Osteosarcoma|Retinoblastoma|Ependymoma|gestational trophoblastic disease|gbm|Glioblastoma|Gastrointestinal Stromal Tumor|hcc|Hodgkin|Kaposi", 
                       all2019$Conditions, ignore.case = TRUE),]


tablecancer <- table(cancer$complete, cancer$IPD_PLAN)

cn <- ggplot(data = cancer, aes(x = complete, fill = IPD_PLAN))
cn <- cn + geom_bar(stat = "count", position = "fill")
cn <- cn + scale_y_continuous(labels = scales::percent)
cn <- cn +  scale_x_discrete(guide = guide_axis(n.dodge=1))
cn <- cn + scale_fill_manual(values = c("orange","dark grey","yellow","blue"))
cn <- cn + ggtitle("Cancer")  +  xlab("" )  + ylab("")

prop.test(x=c(23,997), n=c(108,11119), alternative = "greater" ,correct = FALSE)


#HIV
HIV<- all2019[grep("\"AIDs\"|HIV|?\\AIDS|? AIDS|?;aids", all2019$Conditions, ignore.case = TRUE),]

TABLEHIV <- table(HIV$complete, HIV$IPD_PLAN)

hv <- ggplot(data = HIV , aes(x = complete, fill = IPD_PLAN))
hv <- hv + geom_bar(stat = "count", position = "fill")
hv <- hv + scale_y_continuous(labels = scales::percent)
hv <- hv +  scale_x_discrete(guide = guide_axis(n.dodge=1))
hv <- hv + scale_fill_manual(values = c("orange","dark grey","yellow","blue"))
hv + ggtitle("HIV/AIDS")  +
  xlab("" )  + ylab("")


#combine all graphs onto 1 figure
figure<-ggarrange(
  pl, cn ,wh, hv, labels = c("A", "B","C","D"),
  common.legend = TRUE, legend = "bottom"
)
