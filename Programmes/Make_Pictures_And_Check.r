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
      
      Final_Table <- Final_Cleaned_Benefish4_Data[["Estimates by the Benefish studies of annual fisheries harvests"]]

   ##
   ##    Estimate US$ for non-US$ currencies
   ##
      Final_Table <- merge(Final_Table,
                           Exchange_Rates,
                           by.x = c("Unit", "Year"),
                           by.y = c("Currency", "Year"),
                           all.x = TRUE)
                           
      Final_Table$ToUSDollar <- ifelse(Final_Table$Unit == "US$", 1, 
                                ifelse(Final_Table$Unit %in% c("Pieces", "Tonnes"), NA, Final_Table$ToUSDollar))
                                
      Final_Table$USDollarEquivalent <- Final_Table$Value / Final_Table$ToUSDollar
      Final_Table <- data.table(Final_Table)
      
      Aggregate_Values <- Final_Table[,
                                      list(USDollarEquivalent = sum(USDollarEquivalent, na.rm = TRUE)),
                                      keyby = list(Member_Country ,
                                                   Year,
                                                   Harvest_Sector)]
      Total <- Final_Table[,
                          list(USDollarEquivalent = sum(USDollarEquivalent, na.rm = TRUE)),
                          key = list(Member_Country ,
                                       Year,
                                       Harvest_Sector = rep("Total", nrow(Final_Table)))]
      Aggregate_Values <- rbind(Aggregate_Values,
                                Total)
      Aggregate_Values <- data.table::dcast(Aggregate_Values,
                                            Member_Country + Year ~ Harvest_Sector,
                                            value.var = "USDollarEquivalent")
                                            
      Aggregate_Values[Aggregate_Values$Year == 2021,]                                 
      
      ##
      ##    The following totals are out - All the errors are from the pdf document - comparison to Table 29.2
      ##       1. French Polynesia B4 = 128,471,800   JH = 128,473,935    Diff of 2,135 is spreadsheet error in Table 29.2 not correctly summing components
      ##       2. Nauru            B4 = 302,618,577   JH = 302,617,852    Diff of 725   is error in Table 11.6 with attributing 2021 aquiculture to 2014 year
      ##       3. Palau            B4 =  22,372,922   JH =  22,362,922    Diff of 10000 is error in Table 13.6 with attributing 2021 freshwater to 2014 year
      ##
      
      Aggregate_Volumes <- Final_Table[Unit %in% c("Tonnes"),
                                      list(Volume = sum(Value, na.rm = TRUE)),
                                      keyby = list(Member_Country ,
                                                   Year,
                                                   Harvest_Sector)]
                                                   
      Total <- Final_Table[Unit %in% c("Pieces", "Tonnes"),
                          list(Volume = sum(Value, na.rm = TRUE)),
                          key = list(Member_Country ,
                                     Year,
                                     Unit,
                                     Harvest_Sector = rep("Total", nrow(Final_Table[Unit %in% c("Pieces", "Tonnes"),])))]

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
                                 
      Aggregate_Volumes$Tonnes <- Aggregate_Volumes$Tonnes - Aggregate_Volumes$Aquaculture 
      Aggregate_Volumes[Aggregate_Volumes$Year == 2021,]                                 

      ##
      ##    The following totals are out - All the errors are from the pdf document - comparison to Table 29.1
      ##
      ##
      ##
      ##
      ##
      ##


      

   ##
   ## Save files our produce some final output of something
   ##
      save(xxxx, file = 'Data_Intermediate/xxxxxxxxxxxxx.rda')
      save(xxxx, file = 'Data_Output/xxxxxxxxxxxxx.rda')
##
##    And we're done
##
