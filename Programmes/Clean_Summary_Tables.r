##
##    Programme:  Clean_Summary_Tables.R
##
##    Objective:  What is this programme designed to do?
##
##    Author:     James Hogan, FAME - SPC, 10 September 2024
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))
      
   ##
   ##    Collect up the downloaded files
   ##
      load('Data_Intermediate/Country_Data.rda')
      
   ##
   ##    Collect up and process the Summary_Tables files
   ##
      Summary_Tables <- Country_Data[["Summary_Tables"]]
      
      Clean_Summary_Tables <- list()
      
   ##
   ##    Volume of production in 2021XXTable29-1
   ##
      X <- Summary_Tables[["Volume of production in 2021XXTable29-1"]]
      names(X)[names(X) == "V1"] <- "PICT"
      Mapping_Table <- data.frame(Metric   = paste0(X[1,],X[2,]),
                                  variable = names(X))
      X$ID <- 1:nrow(X)
      
      Mapping_Table$Metric[Mapping_Table$Metric == "Aquaculturet"] <- "Aquaculture - Tonnes"
      Mapping_Table$Metric[Mapping_Table$Metric == "Pieces"]       <- "Aquaculture - Pieces"
      Mapping_Table$Metric[Mapping_Table$Metric == "Table29-1"]    <- "Table"
      Mapping_Table$Metric[Mapping_Table$Metric == "MeasureVolume of production in 2021"] <- "Measure"
      
      Mapping_Table <- Mapping_Table[Mapping_Table$Metric != "Total",]
      Mapping_Table$Metric <- str_replace_all(Mapping_Table$Metric, "  ", " ")
      
      
         
      X <- reshape2::melt(X[3:nrow(X),],
                          id.var = c("ID", "PICT"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X <- X[!is.na(X$Value),]
      
      X <- merge(X,
                 Mapping_Table,
                 by = c("variable"))
      X$Measure <- "Volume of production in 2021"
      X$Table   <- "Table29-1"
      X$Unit    <- ifelse(str_detect(X$Metric, "Pieces"), "Pieces", "Tonnes")
      Clean_Summary_Tables[["Volume of production in 2021"]] <- X[,c("Measure", "Table", "Metric", "Unit", "PICT", "Value")]


   ##
   ##    Value of production in 2021XXTable29-1
   ##
      X <- Summary_Tables[["Value of production in 2021 (US$)XXTable29-2"]]
      names(X)[names(X) == "V1"] <- "PICT"
      X$PICT[1] <- "PICT"
      Mapping_Table <- data.frame(Metric   = paste0(X[1,]),
                                  variable = names(X))
      X$ID <- 1:nrow(X)
      
      Mapping_Table <- Mapping_Table[Mapping_Table$Metric != "Total",]
         
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("ID", "PICT"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X <- X[!is.na(X$Value),]
      
      X <- merge(X,
                 Mapping_Table,
                 by = c("variable"))
      X$Measure <- "Value of production in 2021"
      X$Table   <- "Table29-2"
      X$Unit    <- "US$"
      Clean_Summary_Tables[["Value of production in 2021"]] <- X[,c("Measure", "Table", "Metric", "Unit", "PICT", "Value")]
       ##
   ##    Estimates by the Benefish studies of annual fisheries harvests - Table6-4
   ##
      X <- Summary_Tables[["Volume of fishery production by PICT, 2007 vs 2014 vs 2021 (t)XXTable29-7"]]
      for(i in 2:nrow(X))
      {
         X$V1[i] <- ifelse((X$V1[i] == "") &(X$V1[(i-1)] != ""), X$V1[(i-1)], X$V1[i])
      }
      names(X) <- X[1,]
         
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "PICT", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, "\\D+", ""))
      X$PICT <- str_trim(X$PICT)
      X$Metric <- as.character(X$variable)
      X$Measure <- "Volume of fishery production by PICT"
      X$Table   <- "Table29-7"
      X$Unit <- "Tonnes"
      X <- X[X$Metric != "Total",]
      X <- X[!is.na(X$Value),]
      Clean_Summary_Tables[["Volume of fishery production by PICT"]] <- X[,c("Measure", "Table", "Metric", "Unit", "PICT","Year", "Value")]
      
   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Summary_Tables, file = 'Data_Intermediate/Clean_Summary_Tables.rda')
##
##    And we're done
##
