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
      Capture_Countries_and_Tables$In_Captures <- 1
      
      ##
      ##    What tables have we been able to salvage?
      ##
      
      Together <- merge(All_Tables,
                        Capture_Countries_and_Tables,
                        by = c("Country", "Table"),
                        all = TRUE)
      
      Together <- Together[order(Together$Standardised_Table_Name, Together$Country, Together$BeneFish_Table_Name),]
      write.table(Together, file = "Data_Output/Mapping_Tables_To_Standard.csv", row.names = FALSE, sep= ",")
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
   ##
   ##    Collect up and process the A_Member_Country files
   ##
      Country_Data <- list()
      
      for(MC in Member_Country)
      {
         A_Member_Country <- All_Data[[MC]]
         ##
         ##    Find all of the blank rows - these are separations between tables
         ##
            Blank_Rows <- do.call(c, sapply(1:nrow(A_Member_Country), function(Row)
                                             {
                                                if(all(A_Member_Country[Row,1:(length(A_Member_Country)-1)] == "") == TRUE) return(Row)
                                                else return(NULL)
                                             })
                                 )
            
            Separated_Data <- list()
            for(i in 1:length(Blank_Rows))
            {
               if(length(Separated_Data) == 0)
                  {
                     Next <- A_Member_Country[1:Blank_Rows[i],1:which(A_Member_Country[1,] == "Table")]
                     ##
                     ##    Clean up some of these table names
                     ##
                        Table_Name <- Next[2,which(Next[1,] == "Measure")]
                        Table_Name <- ifelse(str_detect(Table_Name,"Annual fisheries and aquaculture harvest"), "Annual fisheries and aquaculture harvest", 
                                       # ifelse(str_detect(Table_Name,"Fishing contribution to"),"Fishing contribution to GDP",
                                         ifelse(str_detect(Table_Name,"Estimates by the Benefish studies"),"Estimates by the Benefish studies of annual fisheries harvests",
                                          ifelse(str_detect(Table_Name,"Fish exports of") | str_detect(Table_Name,"Fisheries exports") | str_detect(Table_Name,"Fishery exports"),"Fish exports",
                                           Table_Name)))
                     
                     
                     Separated_Data[[paste(Table_Name, Next[2,length(Next)], sep = "XXTable")]] <- Next
                     
                  } else {
                     if((Blank_Rows[(i-1)]+1) != Blank_Rows[i])
                        {
                           Next <- A_Member_Country[(Blank_Rows[(i-1)]+1):Blank_Rows[i],] 
                           Next <- Next[,1:which(Next[1,] == "Table")] 
                           ##
                           ##    Clean up some of these table names
                           ##
                              Table_Name <- Next[2,which(Next[1,] == "Measure")]
                              Table_Name <- ifelse(str_detect(Table_Name,"Annual fisheries and aquaculture harvest"), "Annual fisheries and aquaculture harvest", 
                                             # ifelse(str_detect(Table_Name,"Fishing contribution to"),"Fishing contribution to GDP",
                                               ifelse(str_detect(Table_Name,"Estimates by the Benefish studies"),"Estimates by the Benefish studies of annual fisheries harvests",
                                                ifelse(str_detect(Table_Name,"Fish exports of") | str_detect(Table_Name,"Fisheries exports") | str_detect(Table_Name,"Fishery exports"),"Fish exports",
                                                 Table_Name)))
                           
                           Separated_Data[[paste(Table_Name, Next[2,length(Next)], sep = "XXTable")]] <- Next
                        }                  
                  }
            }
         ##
         ##    Accumulate the data 
         ##
         Country_Data[[MC]] <- Separated_Data
      }
      
   ##
   ##    What sort of coverage do we have?
   ##
      Country_and_Tables <- do.call(rbind,lapply(names(Country_Data), function(Countries){
                                                 return(data.frame(Tables    = str_split_fixed(names(Country_Data[[Countries]]),"XX",2)[,1],
                                                                   Countries = rep(Countries, length(names(Country_Data[[Countries]])))))
                                                })
                                   )
      Country_and_Tables <- Country_and_Tables[order(Country_and_Tables$Tables),]
      table(Country_and_Tables$Tables, Country_and_Tables$Countries)                  
      
      
   ##
   ## Save files our produce some final output of something
   ##
      save(Country_Data, file = 'Data_Intermediate/Country_Data.rda')
##
##    And we're done
##
