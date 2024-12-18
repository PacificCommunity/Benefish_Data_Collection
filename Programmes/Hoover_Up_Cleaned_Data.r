##
##    Programme:  Hoover_Up_Cleaned_Data.R
##
##    Objective:  This programme goes through all of the spreadsheet tabs, and 
##                makes a single list which contains all of the member countries,
##                as separate lists, and all of their tables as data frames associated
##                with each country.
##
##    Author:     James Hogan, FAME - SPC, 10 September 2024
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
      
   ##
   ##    Identify the tables in the raw data
   ##
      Contents <- as.data.frame(list.files(path = "Data_Intermediate/", pattern = "*Manually_Collected_Data_Version2.rda"))
      names(Contents) <- "DataFrames"
      Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n=2)[,1]
      
      All_Raw_Data <- lapply(Contents$DataFrames, function(File){
                           load(paste0("Data_Intermediate/", File))
                           Find_One <- get(str_split_fixed(File, "\\.", n=2)[,1])
                           
                           Find_One$ID <- 1:nrow(Find_One)

                           X <- reshape2::melt(Find_One,
                                               id.var = c("ID"),
                                               factorsAsStrings = FALSE)
                           X$variable <- as.character(X$variable)                    


                           Y <- X[X$value %in% c("Measure", "Table"),]

                           

                           Y$ID <- Y$ID + 1                  
                                 
                           Y <- merge(X,
                                      Y,
                                      by = c("ID", "variable"))
                                 
                           Y <- reshape2::dcast(Y,
                                                ID ~ value.y,
                                                value.var = "value.x")      
                                                
                           Y$Country <- str_replace_all(str_split_fixed(File, "XX",2)[,1], "RAWDATA_", "")
                           names(Y)[names(Y) == "Measure"] <- "BeneFish_Table_Name"
                           rm(list = c(as.character(File)))
                           return(Y)
                        })
      All_Tables <- do.call(rbind, All_Raw_Data)
      All_Tables$In_All_Tables <- 1
      
   ##
   ##    Collect up the cleaned files
   ##
      Contents <- data.frame(DataFrames = list.files(path = "Data_Intermediate/"))
      Contents <- data.frame(DataFrames = Contents[str_detect(Contents$DataFrames, "Clean_"),])
      Contents$Dframe <- str_split_fixed(Contents$DataFrames, "\\.", n=2)[,1]

      All_Processes_Data <- lapply(Contents$DataFrames, function(File){
                                    load(paste0("Data_Intermediate/", File))
                                    X <- get(str_split_fixed(File, "\\.", n=2)[,1])
                                    rm(list = c(as.character(File)))
                                    return(X)
                                 })
      names(All_Processes_Data) <- str_replace_all(Contents$Dframe, "Clean_","")
      
      ##
      ##    What tables have we been able to salvage?
      ##
      
      Capture_Countries_and_Tables <- do.call(rbind, lapply(names(All_Processes_Data), function(First_Level){
                                                      Dlist   <- All_Processes_Data[[First_Level]]
                                                      X <- data.frame(Table   = as.character(),
                                                                      Standardised_Table_Name = as.character())
                                                      for(i in names(Dlist))
                                                      {  
                                                         Y <- data.frame(Table = unique(Dlist[[i]]$Table))
                                                         Y$Standardised_Table_Name <- i
                                                         
                                                         X <- rbind(X, Y)
                                                      }
                                                      X$Country <- rep(First_Level, nrow(X))
                                                      return(X)
                                                   })
                                      )
      Capture_Countries_and_Tables$In_Captured <- "Captured"
      
      ##
      ##    What tables have we been able to salvage?
      ##
      
      Together <- merge(All_Tables,
                        Capture_Countries_and_Tables,
                        by = c("Country", "Table"),
                        all = TRUE)
      ##
      ##    Do the why exclude bits
      ##
      Together$In_Captured <- ifelse(Together$Table %in% c('20-3','24-3','21-6','22-3','9-3','10-4','11-5','23-4','12-1','13-5','25-1','14-4','16-5','26-4','17-2','18-2','19-1','27-4'),"Captured in 'Estimates by the Benefish studies of annual fisheries harvests'", 
                              ifelse(is.na(Together$In_Captured) & str_detect(Together$BeneFish_Table_Name, regex("export", ignore_case = TRUE)), "MAKE THIS AN EXPORT MEASURE", 
                              
                              ifelse(is.na(Together$In_Captured) & str_detect(Together$BeneFish_Table_Name, regex("Employ", ignore_case = TRUE)), "MAKE THIS AN EMPLOYMENT MEASURE", 
                              ifelse(is.na(Together$In_Captured) & str_detect(Together$BeneFish_Table_Name, regex("consum", ignore_case = TRUE)), "MAKE THIS AN CONSUMPTION MEASURE", 
                              ifelse(Together$Table %in% c('12-3'),  "MAKE THIS NOT A PRICE MEASURE", 
                              ifelse(is.na(Together$In_Captured) & str_detect(Together$BeneFish_Table_Name, regex("price", ignore_case = TRUE)),  "MAKE THIS AN PRICE MEASURE", 
                              #ifelse(is.na(Together$In_Captured) & (str_detect(Together$BeneFish_Table_Name, regex("fleet", ignore_case = TRUE)) | str_detect(Together$BeneFish_Table_Name, regex("vess", ignore_case = TRUE))),  "MAKE THIS AN FLEET SIZE MEASURE", 
                              
                              ifelse(is.na(Together$In_Captured) & Together$Table %in% c('10-11','11-9','12-5','6-9','10-10','16-14','16-15','26-8','13-11'), "MAKE THIS GOVT REVENUE METRIC",Together$In_Captured)))))))

      Together$Table <- paste0("'", Together$Table)
      
      Together <- Together[order(Together$Standardised_Table_Name, Together$Country, Together$BeneFish_Table_Name),]
      write.table(Together, file = "Data_Output/Mapping_Tables_To_Standard.csv", row.names = FALSE, sep= ",")

   ##
   ##    Aggregate up the metrics into their Standardised_Table_Name dimensions
   ##
      Cleaned_Table_Names <- unique(Together$Standardised_Table_Name)[!is.na(unique(Together$Standardised_Table_Name))]
      
      Cleaned_Data <- lapply(Cleaned_Table_Names, function(Table_Name){
                             Find_Me <- lapply(names(All_Processes_Data), function(Member_Countries){
                                               A_Country <- All_Processes_Data[[Member_Countries]]
                                               
                                               if(Table_Name %in% names(A_Country)) 
                                                {
                                                   Found_Table <- A_Country[[Table_Name]]
                                                   Found_Table$Member_Country <- Member_Countries
                                                   return(Found_Table)
                                                } else return(NULL)
                                          })
                             return(do.call(rbind.fill, Find_Me))
                           })
      names(Cleaned_Data) <- Cleaned_Table_Names
   
   
   ##
   ##    Now cycle through their dimensions and lets check for spelling errors and inconsistancies
   ##
      Standard_Dimensions <- lapply(Cleaned_Table_Names, function(Table_Name){
      
                                    A_Table <- Cleaned_Data[[Table_Name]]
                                    
                                    Dimensions <- names(A_Table)
                                    Dimensions <- Dimensions[!(Dimensions %in% c("Measure","Table", "Value", "Member_Country"))]
                                    
                                    Table_Dimensions <- data.frame(Measure = character(),
                                                                   variable = character(),
                                                                   value      = character())
                                                                   
                                    for(i in Dimensions)
                                    {
                                       Table_Dimensions <- rbind(Table_Dimensions,
                                                                 data.frame(Measure = rep(Table_Name, length(unique(A_Table[,i]))),
                                                                            variable = rep(i, length(unique(A_Table[,i]))),
                                                                            value      = unique(as.character(A_Table[,names(A_Table)[names(A_Table) == i]]))))
                                    }
                                    return(Table_Dimensions)
                                   })
                                   
      Standard_Dimensions <- do.call(rbind, Standard_Dimensions)

   ##
   ##    Output it for posterity
   ##
      write.table(Standard_Dimensions, file = "Data_Output/Standard_Dimensions.csv", row.names = FALSE, sep= ",")

   ##
   ##    Now lets clean it up.
   ##
      Standard_Dimensions$Cleaned_Value <- str_squish(Standard_Dimensions$value)
      
      Standard_Dimensions$Cleaned_Value <- ifelse(str_detect(Standard_Dimensions$Cleaned_Value, regex("Long", ignore_case = TRUE)),     "Long Line", 
                                           ifelse(str_detect(Standard_Dimensions$Cleaned_Value, regex("Bigeye", ignore_case = TRUE)),   "Bigeye", 
                                           ifelse(str_detect(Standard_Dimensions$Cleaned_Value, regex("Albacore", ignore_case = TRUE)), "Albacore", 
                                           ifelse(str_detect(Standard_Dimensions$Cleaned_Value, regex("Skipjack", ignore_case = TRUE)), "Skipjack", 
                                           ifelse(str_detect(Standard_Dimensions$Cleaned_Value, regex("Yellowfin", ignore_case = TRUE)), "Yellowfin", 
                                           ifelse(str_detect(Standard_Dimensions$Cleaned_Value, regex("Pacific bluefin", ignore_case = TRUE)), "Pacific Bluefin", 
                                           ifelse(str_detect(Standard_Dimensions$Cleaned_Value, regex("2017", ignore_case = TRUE)),  "2017", 
                                           ifelse(str_detect(Standard_Dimensions$Cleaned_Value, regex("2018", ignore_case = TRUE)),  "2018", 
                                           ifelse(str_detect(Standard_Dimensions$Cleaned_Value, regex("2019", ignore_case = TRUE)),  "2019", 
                                           ifelse(str_detect(Standard_Dimensions$Cleaned_Value, regex("2020", ignore_case = TRUE)),  "2020", 
                                           ifelse(str_detect(Standard_Dimensions$Cleaned_Value, regex("2021", ignore_case = TRUE)),  "2021", 
                                           ifelse(str_detect(Standard_Dimensions$Cleaned_Value, regex("Value added", ignore_case = TRUE)),  "Value added", 
                                           ifelse(str_detect(Standard_Dimensions$Cleaned_Value, regex("Offshore local", ignore_case = TRUE)),  "Offshore Locally Based", 
                                           ifelse(str_detect(Standard_Dimensions$Cleaned_Value, regex("Offshore foreign", ignore_case = TRUE)),  "Offshore Foreign Based", 
                                           ifelse(str_detect(Standard_Dimensions$Cleaned_Value, regex("Fishing contribution to GDP", ignore_case = TRUE)),  "Fishing Contribution", 
                                           ifelse(str_detect(Standard_Dimensions$Cleaned_Value, regex("Gross value of production", ignore_case = TRUE)),  "Gross Value of Production", 
                                           Standard_Dimensions$Cleaned_Value))))))))))))))))


#      Look <- unique(Standard_Dimensions$Cleaned_Value)
#      Look <- Look[order(Look)]
#      Look


      Standard_Dimensions <- lapply(Cleaned_Table_Names, function(Table_Name){
                                    print(Table_Name)
                                    A_Table <- Cleaned_Data[[Table_Name]]
                                    A_Table$ID <- 1:nrow(A_Table)
                                    
                                    A_Table <- reshape2::melt(A_Table,
                                                              id.var = c("ID", "Measure","Table", "Value", "Member_Country"),
                                                              factorsAsStrings = FALSE)                                    
                                    
                                    A_Table <- merge(A_Table,
                                                     Standard_Dimensions,
                                                     by = c("Measure", "variable", "value"),
                                                     all.x = TRUE)
                                                     
                                    A_Table <- reshape2::dcast(A_Table,
                                                              ID + Member_Country + Measure + Table + Value ~ variable,
                                                              value.var = "Cleaned_Value")                                    
                                    return(A_Table)
                                   })
      names(Standard_Dimensions) <- Cleaned_Table_Names

   ##
   ##    Manually eye-ball the results
   ##
   Standard_Dimensions[["Catches by Method"]]
   Standard_Dimensions[["Domestic Fish Consumption"]]
   Standard_Dimensions[["Estimates by the Benefish studies of annual fisheries harvests"]]
   Standard_Dimensions[["Fish Exports"]]
   Standard_Dimensions[["Fisheries Revenue"]]
   Standard_Dimensions[["Fishing contribution to GDP"]]
   Standard_Dimensions[["Fishing contribution to GDP - VAR Method"]]
   Standard_Dimensions[["Fishing Employment"]]
   Standard_Dimensions[["Number of households engaged in fishing"]]
   Standard_Dimensions[["Price Measures"]]
   Standard_Dimensions[["Value of production in 2021"]]
   Standard_Dimensions[["Volume of fishery production by PICT"]]
   Standard_Dimensions[["Volume of production in 2021"]]

   ##
   ##    Save the results
   ##
      Final_Cleaned_Benefish4_Data <- Standard_Dimensions

      write.table(Standard_Dimensions[["Catches by Method"]],            file = "Data_Output/Catches_by_Method.csv", row.names = FALSE, sep= ",")
      write.table(Standard_Dimensions[["Domestic Fish Consumption"]],    file = "Data_Output/Domestic_Fish_Consumption.csv", row.names = FALSE, sep= ",")
      write.table(Standard_Dimensions[["Fish Exports"]],                 file = "Data_Output/Fish_Exports.csv", row.names = FALSE, sep= ",")
      write.table(Standard_Dimensions[["Fisheries Revenue"]],            file = "Data_Output/Fisheries_Revenue.csv", row.names = FALSE, sep= ",")
      write.table(Standard_Dimensions[["Fishing contribution to GDP"]],  file = "Data_Output/Fishing_contribution_to_GDP.csv", row.names = FALSE, sep= ",")
      write.table(Standard_Dimensions[["Fishing Employment"]],           file = "Data_Output/Fishing_Employment.csv", row.names = FALSE, sep= ",")
      write.table(Standard_Dimensions[["Price Measures"]],               file = "Data_Output/Price_Measures.csv", row.names = FALSE, sep= ",")
      write.table(Standard_Dimensions[["Value of production in 2021"]],  file = "Data_Output/Value_of_production_in_2021.csv", row.names = FALSE, sep= ",")
      write.table(Standard_Dimensions[["Volume of production in 2021"]], file = "Data_Output/Volume_of_production_in_2021.csv", row.names = FALSE, sep= ",")
      
      write.table(Standard_Dimensions[["Fishing contribution to GDP - VAR Method"]],file = "Data_Output/Fishing_contribution_to_GDP_VAR_Method.csv", row.names = FALSE, sep= ",")
      write.table(Standard_Dimensions[["Volume of fishery production by PICT"]],    file = "Data_Output/Volume_of_fishery_production_by_PICT.csv", row.names = FALSE, sep= ",")
      write.table(Standard_Dimensions[["Number of households engaged in fishing"]], file = "Data_Output/Number_of_households_engaged_in_fishing.csv", row.names = FALSE, sep= ",")
      
      write.table(Standard_Dimensions[["Estimates by the Benefish studies of annual fisheries harvests"]], file = "Data_Output/Estimates_by_the_Benefish_studies_of_annual_fisheries_harvests.csv", row.names = FALSE, sep= ",")

      save(Standard_Dimensions, file = "Data_Output/Final_Cleaned_Benefish4_Data.rda")
 
##
##    And we're done
##

