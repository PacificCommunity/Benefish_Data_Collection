##
##    Programme:  Make_Pictures_And_Check.r
##
##    Objective:  What is this programme designed to do?
##
##    Author:     <PROGRAMMER>, <TEAM>, <DATE STARTED>
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
   ##
   ##    Load some generic functions or colour palattes, depending on what you're doing.
   ##
      source("R/functions.r")
      source("R/themes.r")
   ##
   ##    Load data from somewhere
   ##
      load("Data_Output/Final_Cleaned_Benefish4_Data.rda")
      load("Data_Intermediate/Cleaned_Data.rda")
      load("Data_Intermediate/All_Tables.rda")
      load("Data_Intermediate/Cleaned_Table_Names.rda")
      load("Data_Intermediate/Together.rda")
      load("Data_Output/Exchange_Rates.rda") 

      load("C:/From BigDisk/GIT/American_Samoa_Tuna_Policy/Data_Spatial/Countries.rda") 


   ##
   ##    Carve it out, check it out, and make pictures
   ##
      Catches_By_Method         <- Final_Cleaned_Benefish4_Data[["Catches by Method"]]
      Domestic_Fish_Consumption <- Final_Cleaned_Benefish4_Data[["Domestic Fish Consumption"]]
      Benefish_studies          <- Final_Cleaned_Benefish4_Data[["Estimates by the Benefish studies of annual fisheries harvests"]]
      Fish_Exports              <- Final_Cleaned_Benefish4_Data[["Fish Exports"]]
      Fisheries_Revenue         <- Final_Cleaned_Benefish4_Data[["Fisheries Revenue"]]
      Fish_GDP                  <- Final_Cleaned_Benefish4_Data[["Fishing contribution to GDP"]]
      Fish_GDP_Var              <- Final_Cleaned_Benefish4_Data[["Fishing contribution to GDP - VAR Method"]]
      Fishing_Employment        <- Final_Cleaned_Benefish4_Data[["Fishing Employment"]]
      Hhld_Fishing              <- Final_Cleaned_Benefish4_Data[["Number of households engaged in fishing"]]
      Price_Measures            <- Final_Cleaned_Benefish4_Data[["Price Measures"]]
      Val_of_Prod               <- Final_Cleaned_Benefish4_Data[["Value of production in 2021"]]
      Vol_of_Prod               <- Final_Cleaned_Benefish4_Data[["Volume of fishery production by PICT"]]
      Vol_of_Prod2021           <- Final_Cleaned_Benefish4_Data[["Volume of production in 2021"]]         

   ##
   ##    Benefish studies
   ##
      ##
      ##    Estimate US$ for non-US$ currencies
      ##
         Benefish_studies <- merge(Benefish_studies,
                                   Exchange_Rates,
                                   by.x = c("Unit", "Year"),
                                   by.y = c("Currency", "Year"),
                                   all.x = TRUE)
                              
         Benefish_studies$ToUSDollar <- ifelse(Benefish_studies$Unit == "US$", 1, 
                                          ifelse(Benefish_studies$Unit %in% c("Pieces", "Tonnes"), NA, Benefish_studies$ToUSDollar))
                                   
         Benefish_studies$USDollarEquivalent <- Benefish_studies$Value / Benefish_studies$ToUSDollar
         Benefish_studies <- data.table(Benefish_studies)
         
         Aggregate_Values <- Benefish_studies[,
                                         list(USDollarEquivalent = sum(USDollarEquivalent, na.rm = TRUE)),
                                         keyby = list(Member_Country ,
                                                      Year,
                                                      Harvest_Sector)]
         Total <- Benefish_studies[,
                             list(USDollarEquivalent = sum(USDollarEquivalent, na.rm = TRUE)),
                             key = list(Member_Country ,
                                          Year,
                                          Harvest_Sector = rep("Total", nrow(Benefish_studies)))]
         Aggregate_Values <- rbind(Aggregate_Values,
                                   Total)
         Aggregate_Values <- data.table::dcast(Aggregate_Values,
                                               Member_Country + Year ~ Harvest_Sector,
                                               value.var = "USDollarEquivalent")
                                               
         Aggregate_Values[Aggregate_Values$Year == 2021,]                                 
         Aggregate_Values[Aggregate_Values$Year == 2021, c("Member_Country","Year","Coastal commercial","Coastal subsistence","Freshwater","Offshore Foreign Based","Offshore Locally Based","Aquaculture","Total")]  
         
         Table_Aggregate_Values <- Aggregate_Values
         save(Table_Aggregate_Values, file = "Data_Intermediate/Table_Aggregate_Values.rda")
         
         ##
         ##    The following totals are out - All the errors are from the pdf document - comparison to Table 29.2
         ##       1. French Polynesia B4 = 128,471,800   JH = 128,473,935    Diff of 2,135 is spreadsheet error in Table 29.2 not correctly summing components
         ##       2. Nauru            B4 = 302,618,577   JH = 302,617,852    Diff of 725   is error in Table 11.6 with attributing 2021 aquiculture to 2014 year
         ##       3. Palau            B4 =  22,372,922   JH =  22,362,922    Diff of 10000 is error in Table 13.6 with attributing 2021 freshwater to 2014 year
         ##
         
         Aggregate_Volumes <- Benefish_studies[Unit %in% c("Tonnes"),
                                         list(Volume = sum(Value, na.rm = TRUE)),
                                         keyby = list(Member_Country ,
                                                      Year,
                                                      Harvest_Sector)]
                                                      
         Total <- Benefish_studies[Unit %in% c("Pieces", "Tonnes"),
                             list(Volume = sum(Value, na.rm = TRUE)),
                             key = list(Member_Country ,
                                        Year,
                                        Unit,
                                        Harvest_Sector = rep("Total", nrow(Benefish_studies[Unit %in% c("Pieces", "Tonnes"),])))]

         Total <- data.table::dcast(Total,
                                    Member_Country + Year ~ Unit,
                                    value.var = "Volume")
                                    
         Total$Pieces[is.na(Total$Pieces)] <- 0


         Aggregate_Volumes <- data.table::dcast(Aggregate_Volumes,
                                                Member_Country + Year ~ Harvest_Sector,
                                                value.var = "Volume")
         Aggregate_Volumes <- data.table::melt(Aggregate_Volumes,
                                               id = c("Member_Country", "Year"))
         Aggregate_Volumes$value[is.na(Aggregate_Volumes$value)] <- 0
         Aggregate_Volumes <- data.table::dcast(Aggregate_Volumes,
                                                Member_Country + Year ~ variable,
                                                value.var = "value")
                                               
                                        
         Aggregate_Volumes <- merge(Aggregate_Volumes,
                                    Total,
                                    by = c("Member_Country", "Year"))
                                    
         Aggregate_Volumes$`Total Tonnes (excl Aqua)` <- Aggregate_Volumes$Tonnes - Aggregate_Volumes$Aquaculture 
         Aggregate_Volumes[Aggregate_Volumes$Year == 2021, c("Member_Country","Year","Coastal commercial","Coastal subsistence","Freshwater","Offshore Foreign Based","Offshore Locally Based","Total Tonnes (excl Aqua)","Aquaculture","Pieces")]  
         
         Table_Aggregate_Volumes <- Aggregate_Volumes
         save(Table_Aggregate_Volumes, file = "Data_Intermediate/Table_Aggregate_Volumes.rda")
         
      ##
      ##    Lets check something out - how are the proportions of each group changing over time?
      ##       Drop freakign aquiculture
      ##
         Aggregate_Volumes$Tonnes <- Aggregate_Volumes$Tonnes - Aggregate_Volumes$Aquaculture
         Proportions <- Aggregate_Volumes[,c("Member_Country","Year","Coastal commercial","Coastal subsistence","Freshwater","Offshore Foreign Based","Offshore Locally Based","Tonnes")]

         
         Proportions <- data.table::melt(Proportions,
                                               id = c("Member_Country", "Year", "Tonnes"))            
                                               
                                               
         Proportions$Proportions <- with(Proportions, value / Tonnes)
         ProportionsLook <- data.table::dcast(Proportions,
                                                Member_Country + Year ~ variable,
                                                value.var = "Proportions")   
                                                
         Proportions <- data.frame(Proportions)
         Subsistence <- Proportions[((Proportions$variable == 'Coastal subsistence') & (Proportions$Member_Country != 'International_Waters')),c("Member_Country", "Year", "Proportions")]
         
         Subsistence$StdProportions <- (Subsistence$Proportions - mean(Subsistence$Proportions)) / sd(Subsistence$Proportions)
         
         Subsistence$Importance_of_Subsistence <- ifelse(Subsistence$StdProportions  > 2, "Very High",
                                                  ifelse(Subsistence$StdProportions  > 1, "High",         
                                                  ifelse((Subsistence$StdProportions < 1) &
                                                         (Subsistence$StdProportions > -1),"Normal",         
                                                  ifelse(Subsistence$StdProportions  > -2, "Low","Very Low"))))         
                                                  
         Subsistence <- Subsistence[order(Subsistence$StdProportions,Subsistence$Importance_of_Subsistence, Subsistence$Member_Country),]
         Subsistence$Member_Country <- str_replace_all(Subsistence$Member_Country, "_"," ")

         Subsistence$Member_Country <- ifelse(Subsistence$Member_Country == "Northern Marianas Islands", "Northern Mariana Islands",Subsistence$Member_Country)

         Subsistence <- merge(Subsistence,
                              st_drop_geometry(Countries),
                              by.x = "Member_Country",
                              by.y = "NAME_EN",
                              all.x = TRUE)
                              
         Subsistence$Importance_of_Subsistence <- factor(Subsistence$Importance_of_Subsistence, levels = c("Very High","High","Normal","Low"))
         Subsistence <- Subsistence[order(Subsistence$GDP_MD_EST),]
         Subsistence$Member_Country <- as.character(Subsistence$Member_Country)
         Subsistence$Member_Country <- ifelse(Subsistence$Member_Country == "Federated States of Micronesia", "FSM", 
                                       ifelse(Subsistence$Member_Country == "Northern Mariana Islands", "Mariana Is.", 
                                       ifelse(Subsistence$Member_Country == "Papua New Guinea", "PNG", Subsistence$Member_Country)))

      ggplot(Subsistence[!is.na(Subsistence$INCOME_GRP) & Subsistence$Member_Country != "Pitcairn Islands",], 
             aes(x = reorder(str_wrap(Member_Country, 8), sort(-as.numeric(GDP_MD_EST))), 
                 y = Proportions,
                 colour = Year))  + 
             geom_point(alpha = 1, size = 0.5) +
             facet_wrap(. ~ Importance_of_Subsistence ,scales = "free_x")+
             scale_colour_manual(values = SPCColours()) + 
             scale_y_continuous(breaks = seq(from = 0, to = 1, by =0.2), label = percent) +             
             labs(title = "Importance of Subsistence Fishing within PICTs",
                  subtitle = "\nDigitised Benefish 4 Data\n",
                  caption  = "The Pacific Community (SPC)") +
             ylab("Subsistence Fishing\n(Proportion of Total Catch)\n") +
             xlab("") +
             theme_bw(base_size=12, base_family =  "Calibri") %+replace%
             theme(legend.title.align=0.5,
                   plot.margin = unit(c(1,1,1,1),"mm"),
                   panel.border = element_blank(),
                   strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                   strip.text = element_text(colour = "white", 
                                             size   = 10,
                                             family = "MyriadPro-Bold",
                                             margin = margin(1.25,.25,1.25,0.25, unit = "mm")),
                   panel.spacing = unit(1, "lines"),                                              
                   legend.text   = element_text(size = 6, family = "MyriadPro-Regular"),
                   legend.title  = element_text(size = 6, family = "MyriadPro-Regular"),
                   plot.title    = element_text(size = 12, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Light"),
                   plot.subtitle = element_text(size = 8, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                   plot.caption  = element_text(size = 6,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                   axis.title    = element_text(size = 10, colour = SPCColours("Dark_Blue")),
                   axis.text.x   = element_text(size =  6, colour = SPCColours("Dark_Blue"), angle = 90,  margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                   axis.text.y   = element_text(size =  6, colour = SPCColours("Dark_Blue"), angle = 00),
                   legend.key.width = unit(0, "mm"),
                   legend.spacing.y = unit(0, "mm"),
                   legend.margin = margin(0, 0, 0, 0),
                   legend.position  = "bottom")         
          ggsave(paste0("Graphical_Output/Subsistence_Fishing.png"), height =15.13, width = 20.66, dpi = 265, units = c("cm"))
         
     ggplot(Subsistence[!is.na(Subsistence$INCOME_GRP) & Subsistence$Member_Country != "Pitcairn Islands",], 
             aes(x = reorder(str_wrap(Member_Country, 8), sort(-as.numeric(GDP_MD_EST))), 
                 y = Proportions,
                 colour = Year))  + 
             geom_point(alpha = 1, size = 0.7) +
             facet_wrap(. ~ Importance_of_Subsistence ,scales = "free_x")+
             scale_colour_manual(values = SPCColours()) + 
             scale_y_continuous(breaks = seq(from = 0, to = 1, by =0.2), label = percent) +             
             ylab("Subsistence Fishing\n(Proportion of Total Catch)\n") +
             xlab("") +
             theme_bw(base_size=12, base_family =  "Calibri") %+replace%
             theme(legend.title.align=0.5,
                   plot.margin = unit(c(1,1,1,1),"mm"),
                   panel.border = element_blank(),
                   strip.background =  element_rect(fill   = SPCColours("Light_Blue")),
                   strip.text = element_text(colour = "white", 
                                             size   = 10,
                                             family = "MyriadPro-Bold",
                                             margin = margin(1.25,.25,1.25,0.25, unit = "mm")),
                   panel.spacing = unit(1, "lines"),                                              
                   legend.text   = element_text(size = 8, family = "MyriadPro-Regular"),
                   legend.title  = element_text(size = 8, family = "MyriadPro-Regular"),
                   plot.title    = element_text(size = 12, colour = SPCColours("Dark_Blue"),  family = "MyriadPro-Light"),
                   plot.subtitle = element_text(size = 8, colour = SPCColours("Light_Blue"), family = "MyriadPro-Light"),
                   plot.caption  = element_text(size = 6,  colour = SPCColours("Dark_Blue"), family = "MyriadPro-Light", hjust = 1.0),
                   plot.tag      = element_text(size =  9, colour = SPCColours("Red")),
                   axis.title    = element_text(size = 10, colour = SPCColours("Dark_Blue")),
                   axis.text.x   = element_text(size = 8, colour = SPCColours("Dark_Blue"), angle = 90,  margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                   axis.text.y   = element_text(size = 10, colour = SPCColours("Dark_Blue"), angle = 00),
                   legend.key.width = unit(0, "mm"),
                   legend.spacing.y = unit(0, "mm"),
                   legend.margin = margin(0, 0, 0, 0),
                   legend.position  = "bottom")         
          ggsave(paste0("Graphical_Output/Subsistence_Fishing_NoTitle.png"), height =12.13, width = 23.66, dpi = 265, units = c("cm"))
         
         
      ##
      ##    No errors in the volume data.
      ##       I manually removed Freshwater 2016 from Kiribati, table 9.4. What was it doing there anyway, when 
      ##       in no other table?
      ##
      
      
      ##
      ##    Make Pictures    
      ##
         Aggregate_Values  <- data.table::melt(Aggregate_Values,
                                               id = c("Member_Country", "Year"))
         Aggregate_Volumes <- data.table::melt(Aggregate_Volumes,
                                               id = c("Member_Country", "Year"))
                                               
         Aggregate_Values$Source  <- "Nominal"
         Aggregate_Volumes$Source <- "Volumes"
         
         Together <- rbind(Aggregate_Values,
                           Aggregate_Volumes)
                           
         Together <- data.table::dcast(Together,
                                       Member_Country + variable + Year ~ Source,
                                       value.var = "value")
         Together$Average_Price <- Together$Nominal / Together$Volumes

         ##
         ##    Aquiculture - complete rubbish
         ##
            Aquiculture <- merge(Together[Together$variable %in% c("Aquaculture"), c("Member_Country", "Year", "Nominal", "Volumes")],
                                 Together[Together$variable %in% c("Pieces"),      c("Member_Country", "Year", "Volumes")],
                                 by = c("Member_Country", "Year"))
            names(Aquiculture) <- c("Member_Country", "Year", "Value (US$)", "Tonnes", "Pieces")
            Aquiculture <- Aquiculture[((Aquiculture$`Value (US$)` > 0) & !is.na(Aquiculture$`Value (US$)`)),]
            
            Table_Aquiculture <- Aquiculture
            save(Table_Aquiculture, file = "Data_Intermediate/Table_Aquiculture.rda")
            
            ##
            ##    Simultaneous equation to reveal price? Nup - negative prices
            ##
               Reduce <- Aquiculture[,
                                   list(Value  = sum(`Value (US$)`, na.rm = TRUE),
                                        Tonnes = sum(Tonnes, na.rm = TRUE),
                                        Pieces = sum(Pieces, na.rm = TRUE)),
                                   key = list(Year = as.numeric(Year))]
               Reduce
               
               A <- as.matrix(Reduce[,c(3,4,1)])     
               b <- as.matrix(Reduce[,c(2)])  
               solve(A, b)
               

               
               A <- as.matrix(Reduce[1:2,c(3,4)])     
               b <- as.matrix(Reduce[1:2,c(2)])  
               solve(A, b)
               
               A <- as.matrix(Reduce[2:3,c(3,4)])     
               b <- as.matrix(Reduce[2:3,c(2)])  
               solve(A, b)
               
               A <- as.matrix(Reduce[c(1,3),c(3,4)])     
               b <- as.matrix(Reduce[c(1,3),c(2)])  
               solve(A, b)
               
            ##
            ##    Regression to reveal price? Nup - negative prices. Me thinks the data is very wrong.
            ##
               OLS <- lm(`Value (US$)` ~ (-1 + Tonnes + Pieces)*Year, Aquiculture)
               summary(OLS)


##
##    And we're done
##
