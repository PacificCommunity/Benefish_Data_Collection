##
##    Programme:  Clean_Kiribati.R
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
   ##    Collect up and process the Kiribati files
   ##
      Kiribati <- Country_Data[["Kiribati"]]
      
      Clean_Kiribati <- list()
      
   ##
   ##    Estimates by the Benefish studies of annual fisheries harvests - Table9-4
   ##
      X <- Kiribati[["Estimates by the Benefish studies of annual fisheries harvestsXXTable9-4"]]
      for(i in 2:nrow(X))
      {
         X$V1[i] <- ifelse((X$V1[i] == "") &(X$V1[(i-1)] != ""), X$V1[(i-1)], X$V1[i])
      }
      X$V1[1] <- "Harvest sector"
      X$V2[1] <- "Year"
      names(X) <- X[1,]
      ##
      ##    Aquaculture tonnes or pcs... Choose...
      ##       Pieces
      ##
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2007))] <- 100
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2014))] <- 8642
         X$`Volume `[((X$`Harvest sector` == "Aquaculture") & (X$`Year` == 2021))] <- 2
         
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector", "Year"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X$Measure <- "Estimates by the Benefish studies of annual fisheries harvests"
      X$Unit  = ifelse(X$variable == "Nominal value (A$)", "A$", 
                  ifelse(X$Harvest_Sector == "Aquaculture","Pieces", "Tonnes"))
      Clean_Kiribati[["Estimates by the Benefish studies of annual fisheries harvests"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]


   ##
   ##    Fishing contribution to Kiribati GDP in 2021 - Table20-5
   ##
      X <- Kiribati[["Fishing contribution to GDP in 2021 using an alternative approachXXTable9-6"]]
      X_Name <- names(Kiribati[6])
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      X <- X[!str_detect(X$Harvest_Sector, "Total"),]
      X$Year    <- 2021
      X$Measure <- "Fishing contribution to GDP - VAR Method"
      X$Unit  = ifelse(X$variable == "VAR", "Proportion", "A$")
      X$GDP_Dimension <- str_trim(X$variable)
      Clean_Kiribati[["Fishing contribution to GDP - VAR Method"]] <- X[,c("Measure","Table", "Harvest_Sector", "GDP_Dimension", "Year", "Unit", "Value")]
      
   ##
   ##    Fishing contribution to Kiribati GDP in 2021 - Table7-7
   ##
      X <- Kiribati[["Fishing contribution to GDP (A$ thousands)XXTable9-5"]]
      X$V1[1] <- "Harvest sector"
      names(X) <- X[1,]
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Harvest sector"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))*1000
      X$Harvest_Sector <- str_trim(X$`Harvest sector`)
      X <- X[!is.na(X$Value),]
      #X <- X[str_detect(X$Harvest_Sector, "Fishing contribution"),]
      X$Year    <- as.numeric(str_replace_all(X$variable, "\\D+", ""))
      X$Measure <- "Fishing contribution to GDP"
      X$Unit    <- "A$"
      Clean_Kiribati[["Fishing contribution to GDP"]] <- X[,c("Measure","Table", "Harvest_Sector", "Year", "Unit", "Value")]
      
      
   ##
   ##    Fish Exports
   ##
      X <- Kiribati[["Fish exportsXXTable9-7"]]
      names(X) <- X[1,]
      names(X)[1] <- "Aggregate Commodity"
 
      X <- reshape2::melt(X[2:nrow(X),],
                          id.var = c("Measure","Table", "Aggregate Commodity"),
                          factorsAsStrings = FALSE)
      X$Value <- as.numeric(str_replace_all(X$value, ",", ""))*1000
      X <- X[!is.na(X$Value),]
      X$Dimension       <- "Aggregate Commodity"
      X$Dimension_Value <- str_trim(X$`Aggregate Commodity`)
      #X <- X[!str_detect(X$`Number of People Employed`, regex("Total", ignore_case = TRUE)),]
      X$Year  <- X$variable
      X$Measure <- "Fish Exports"
      X$Unit    <- "A$"
      Clean_Kiribati[["Fish Exports"]] <- X[,c("Measure","Table", "Dimension", "Dimension_Value", "Year", "Unit", "Value")]


   ##
   ## Save files our produce some final output of something
   ##
      save(Clean_Kiribati, file = 'Data_Intermediate/Clean_Kiribati.rda')
##
##    And we're done
##
