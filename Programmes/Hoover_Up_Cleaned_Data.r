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
                              ifelse(is.na(Together$In_Captured) & str_detect(Together$BeneFish_Table_Name, regex("price", ignore_case = TRUE)),  "MAKE THIS AN PRICE MEASURE", 
                              ifelse(is.na(Together$In_Captured) & (str_detect(Together$BeneFish_Table_Name, regex("fleet", ignore_case = TRUE)) |
                                                                    str_detect(Together$BeneFish_Table_Name, regex("vess", ignore_case = TRUE))),  "MAKE THIS AN FLEET SIZE MEASURE", 
                              
                              ifelse(Together$Table %in% c('10-11','11-9','12-5','6-9','10-10','16-14','16-15','26-8','13-11'), "MAKE THIS GOVT REVENUE METRIC",Together$In_Captured)))))))

      Together$Table <- paste0("'", Together$Table)
      
      Together <- Together[order(Together$Standardised_Table_Name, Together$Country, Together$BeneFish_Table_Name),]
      write.table(Together, file = "Data_Output/Mapping_Tables_To_Standard.csv", row.names = FALSE, sep= ",")
      
##
##    And we're done
##
