test1 <- all2019[, c(1,4,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]

#all HIC trials
test1$HIC[test1$HIC.East.Asia...Pacific != 0 | 
            test1$HIC.Europe...Central.Asia != 0 | test1$HIC..LatinAmerica...Carribean!= 0
          | test1$HIC.Middle.East..North.Africa != 0| test1$HIC.North.America != 0
          |  test1$HIC.Sub.Saharan != 0] = "HIC"


#all LMIC trials
test1$LMIC[test1$LMIC.East.Asia...Pacific != 0 | 
             test1$LMIC.Europe...Central.Asia != 0 | test1$LMIC.LatinAmerica...Carribean != 0
           | test1$LMIC.Middle.East..North.Africa != 0| test1$LMIC.North.America != 0
           |  test1$LMIC.Sub.Saharan.Africa != 0 | test1$LMIC.South.Asia != 0 ] ="LMIC"

#all trials both in HIC and LMIC
#test1$HIC.LMIC[test1$HIC == "HIC" & test1$LMIC == "LMIC"] = "both"

# MAKE ALL NA values NULL
test1$LMIC[is.na(test1$LMIC)] = "NULL"
test1$HIC[is.na(test1$HIC)] = "NULL"


test1$HIC.LMIC <- "NULL"


#only HIC trials
test1$HIC.LMIC[test1$HIC == "HIC" & test1$LMIC == "NULL"] = "HIC"
#only LMIC trials
test1$HIC.LMIC[test1$LMIC == "LMIC" & test1$HIC == "NULL"] = "LMIC"

#replace all nuLL's with NA for plotting graph 
test1$HIC.LMIC[ test1$HIC.LMIC == "NULL" ] <- NA


#dataset graph without NA values so we can plot graph without NA
test1 <- test1[!is.na(test1$HIC.LMIC),]


#swap order of bars on x-axis
test1$complete <- factor(test1$complete, levels = c("NOT COMPLETED","COMPLETED"))


#stacked graph of results
p <- ggplot(data =test1 , aes(x = complete, fill = IPD_PLAN)) +
geom_bar(stat = "count", position = "fill") +
 scale_y_continuous(labels = scales::percent) +
scale_x_discrete(guide = guide_axis(n.dodge=10)) +
scale_fill_manual(values = c("orange","dark grey","yellow","blue")) +
facet_grid(~ HIC.LMIC) +
labs(title = " ",
             fill = "Response",
             x = "",
             y = ""
)















