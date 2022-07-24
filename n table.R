library(finalfit)
install.packages("expss")  
library(expss)
library(formattable)
library(gridExtra)
library(grid)

N <- cross_cases(test1,HIC.LMIC, list(total(),complete))


cancertrials$canncer <- "cancer"
M <- cross_cases(cancertrials, canncer, list(total(), complete))

whoconditionstrials$whoconditions <-  "whoconditions"
O <-  cross_cases(whoconditionstrials, whoconditions, list(total(), complete))


HIVtrials$HIV <- "HIV/AIDS"
Q <- cross_cases(HIVtrials, HIV, list(total(), complete))


figure3 <- rbind(N,M,O,Q)


figure4 <- figure3[!(figure3$row_labels  == "HIC.LMIC|#Total cases" |
                       figure3$row_labels  ==    "canncer|#Total cases" |
                       figure3$row_labels == "whoconditions|#Total cases" |
                       figure3$row_labels == "HIV|#Total cases")]


#renames so it looks better
figure4[1, "row_labels"] <- "HIC"
figure4[2, "row_labels"] <- "LMIC"
figure4[3, "row_labels"] <- "CANCER"
figure4[4, "row_labels"] <- "WHOCONDITIONS"
figure4[5, "row_labels"] <- "HIV/AIDS"


#rename complete and not completed
figure4 <- figure4 %>% 
  rename(
    " " = "row_labels",
    Total = "#Total"
  )


#removes row indexes
rownames(figure4) <- NULL

#formatabble table
figure5 <- formattable(figure4, 
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")) 
            ))

#grid table
grid.table(figure4)


